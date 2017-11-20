module Text.WebPub.Compile
  ( CompilationState (..)
  , compileWebBook )
  where

import qualified Data.Map.Lazy as M
import           Text.XML.HXT.Core

import           Text.WebPub.Data.Toc
import           Text.WebPub.Parse.Html (xpHtmlToc)

inputConfig :: SysConfigList
inputConfig = [withValidate no, withSubstDTDEntities no, withRemoveWS yes, withParseHTML yes]

data CompilationState = CompilationState
  { fileRoutes :: M.Map FilePath FilePath
  , currentFile :: FilePath
  }

setCurrentFile :: FilePath -> CompilationState -> CompilationState
setCurrentFile path state = state { currentFile = path }

compileDocument :: FilePath
                -> String
                -> IOStateArrow CompilationState XmlTree (FilePath, String)
compileDocument path contents =
  setCurrentDocument path
  >>> readString inputConfig contents
  >>> fixLinks
  >>> getFilePath &&& writeDocumentToString [withIndent yes]
  where setCurrentDocument path =
          (changeUserState $ const $ setCurrentFile path)

        getFilePath = getUserState >>> arr currentFile
        fixLinks = this

compileToc :: IOStateArrow CompilationState Toc (FilePath, String)
compileToc = xpickleVal xpHtmlToc
             >>> constA "toc.html" &&& writeDocumentToString [withIndent yes]

compileWebBook :: [(FilePath, String)]
               -> Toc
               -> IOStateArrow CompilationState XmlTree (FilePath, String)
compileWebBook inputDocuments toc =
  (compileDocument $<< constL inputDocuments)
  &&& (constA toc >>> compileToc)
  >>> mergeA (<+>)
