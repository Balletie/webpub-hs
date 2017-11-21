{-# LANGUAGE Arrows #-}
module Text.WebPub.Compile
  ( CompilationState (..)
  , compileWebBook )
  where

import qualified Data.Map.Lazy as M
import           Data.Maybe (maybeToList, isNothing)
import           Data.List (mapAccumL)
import           Text.XML.HXT.Core

import           Text.WebPub.Data.Toc
import           Text.WebPub.Parse.Html (xpHtmlToc)

import           Network.URI
import           System.FilePath

inputConfig :: SysConfigList
inputConfig = [withValidate no, withSubstDTDEntities no, withRemoveWS yes, withParseHTML yes]

data CompilationState = CompilationState
  { fileRoutes :: M.Map FilePath FilePath
  , currentFile :: FilePath
  }
  deriving (Show)

setCurrentFile :: FilePath -> CompilationState -> CompilationState
setCurrentFile path state = state { currentFile = path }

makeRoutedFilePath :: FilePath -> FilePath
makeRoutedFilePath filePath =
  case takeExtension filePath of
    ".html" -> takeFileName filePath
    ".css" -> "styles" </> takeFileName filePath
    x | elem x [".jpg", ".jpeg", ".png" ] -> "img" </> takeFileName filePath
    _ -> takeFileName filePath

relativeToCurrentFile = proc filePath -> do
  state <- getUserState -< filePath
  let currFile = currentFile state
      (_acc, newSegments) = mapAccumL replaceDotDot
                (splitDirectories currFile)
                (splitDirectories filePath)
      newPath = joinPath newSegments
  _ <- traceMsg 1 ("Relativized " ++ filePath ++ " to " ++ newPath) -<< filePath
  returnA -< joinPath newSegments

replaceDotDot :: [FilePath] -> FilePath -> ([FilePath], FilePath)
replaceDotDot acc@(subst:rest) dir = if dir == ".."
                                           then (rest, subst)
                                           else (acc, dir)
replaceDotDot [] dir = ([], dir)

-- | Retrieves the routed file path for the given file if it's in the
-- file route map of the compilation state. Otherwise creates and
-- returns a new file route and inserts it in the state.
getOrCreateRoutedFilePath :: IOStateArrow CompilationState FilePath FilePath
getOrCreateRoutedFilePath = proc file -> do
  state <- getUserState -< file
  routes <- arr fileRoutes -< state
  file' <- relativeToCurrentFile -< file
  case M.lookup file' routes of
    Nothing -> do
      routedFile <- arr makeRoutedFilePath -< file'
      let newRoutes = M.insert file' routedFile routes
      -- Update the state.
      setUserState -< state { fileRoutes = newRoutes }
      returnA -< routedFile
    Just route -> returnA -< route

-- | This monster reroutes all relative links, and leaves other links
-- alone. Any paths that have not been routed before are inserted into
-- the compilation state.
routeLinks :: IOStateArrow CompilationState XmlTree XmlTree
routeLinks = processTopDownWithAttrl (replaceChildren (xshow getChildren
                                                       >>> replaceRelativeLinks)
                            `when` (isAttr >>> hasName "href" `orElse` hasName "src"))

  where replaceRelativeLinks = (arr parseRelativeReference
                                 >>> arrL maybeToList
                                 >>> (isNothing . uriAuthority)
                                 `guardsP` (arr uriPath &&& arr uriFragment
                                           -- Get the routed path, or create it.
                                           >>> getOrCreateRoutedFilePath *** this
                                           >>> arr2 (++)
                                           >>> mkText)
                               )
                               `orElse` (this >>> mkText)

compileDocument :: FilePath
                -> String
                -> IOStateArrow CompilationState XmlTree (FilePath, String)
compileDocument path contents =
  setCurrentDocument path
  >>> readString inputConfig contents
  >>> routeLinks
  >>> (getUserState >>> arr currentFile >>> getOrCreateRoutedFilePath)
  &&& writeDocumentToString [withIndent yes]
  where setCurrentDocument path =
          (changeUserState $ const $ setCurrentFile path)

compileToc :: IOStateArrow CompilationState Toc (FilePath, String)
compileToc = xpickleVal xpHtmlToc
             >>> constA "Contents.html" &&& writeDocumentToString [withIndent yes]

compileWebBook :: [(FilePath, String)]
               -> Toc
               -> IOStateArrow CompilationState XmlTree (FilePath, String)
compileWebBook inputDocuments toc =
  (compileDocument $<< constL inputDocuments)
  <+> (constA toc >>> compileToc)
