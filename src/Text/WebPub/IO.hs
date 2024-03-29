{-# LANGUAGE FlexibleContexts #-}
module Text.WebPub.IO
  ( getPkgPathXmlFromZip
  , getTocXmlFromZip
  , getDocumentsFromZip
  , makeWebBook
  )
  where

import           Data.List (find, map)
import           Data.Maybe (catMaybes)
import qualified Data.Map.Lazy as M
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B

import           Control.Arrow (first)
import           Control.Monad.Except

import           Text.XML.HXT.Arrow.XmlState.RunIOStateArrow ( initialState )
import           Text.XML.HXT.Arrow.XmlState.TypeDefs ( xioUserState )
import           Text.XML.HXT.Core

import           Text.WebPub.Data.Toc
import           Text.WebPub.Compile

import           Codec.Archive.Zip
import           Codec.Epub.Data.Spine
import           Codec.Epub.Data.Manifest

import           System.FilePath
import           System.Directory (createDirectoryIfMissing)

{- Ripped from epub-metadata, because it's not exported.
-}
locateRootFile :: (MonadIO m, MonadError String m) =>
   FilePath -> String -> m FilePath
locateRootFile containerPath' containerDoc = do
   result <- liftIO $ runX (
      readString [withValidate no] containerDoc
      >>> deep (isElem >>> hasName "rootfile")
      >>> getAttrValue "full-path"
      )

   case result of
      (p : []) -> return p
      _        -> throwError $
         "ERROR: rootfile full-path missing from " ++ containerPath'

{- Ripped from epub-metadata, because it's not exported.
-}
fileFromArchive :: MonadError String m =>
   FilePath -> Archive -> m String
fileFromArchive filePath archive = do
   let mbEntry = findEntryByPath filePath archive
   maybe
      (throwError $ "Unable to locate file " ++ filePath)
      (return . UTF8.toString . fromEntry) mbEntry

{- Ripped from epub-metadata, because it's not exported.
-}
containerPath :: FilePath
containerPath = "META-INF/container.xml"

{- Ripped from epub-metadata, but made it so that it works with an Archive as input,
  so that the archive doesn't have to be opened twice
-}
getPkgPathXmlFromZip :: (MonadError String m, MonadIO m) => Archive -> m (FilePath, String)
getPkgPathXmlFromZip archive = do
   {- We need to first extract the container.xml file
      It's required to have a certain path and name in the epub
      and contains the path to what we really want, the .opf file.
   -}
   containerDoc <- fileFromArchive containerPath archive

   rootPath <- locateRootFile containerPath containerDoc

   -- Now that we have the path to the .opf file, extract it
   rootContents <- fileFromArchive rootPath archive

   return (rootPath, rootContents)

-- | Extract the table of contents ncx file from the zip archive by looking
--   through the manifest for the filepath, using the given manifest item ID.
getTocXmlFromZip :: (MonadError String m, MonadIO m)
                 => Manifest
                 -> Spine
                 -> FilePath
                 -> Archive
                 -> m String
getTocXmlFromZip (Manifest mis) spine path zipArchive = do
    tocItem <- return $ find ((spineToc spine ==) . mfiId) mis
    tocItem <- return $ maybe
      (throwError $ "No table of contents found in manifest.")
      return tocItem
    tocPath <- (path </>) <$> mfiHref <$> tocItem
    fileFromArchive tocPath zipArchive

getDocumentsFromZip :: (MonadIO m)
                    => Spine            -- ^ The spine that specifies the order of files.
                    -> Manifest         -- ^ The manifest to find the file locations.
                    -> Archive          -- ^ The EPUB archive that stores the files.
                    -> FilePath         -- ^ The directory of the container, to resolve paths.
                    -> m [(FilePath, B.ByteString)] -- ^ The contents of all the documents.
getDocumentsFromZip spine (Manifest mis) archive relPath =
  return $ map (eRelativePath &&& fromEntry) foundEntries
  where itemRefs = map siIdRef $ spineItemrefs spine
        maybePaths = map (\ref -> mfiHref <$> find ((ref ==) . mfiId) mis) itemRefs
        maybeRelPaths = map (fmap (relPath </>)) maybePaths
        maybeEntries = map (>>= (flip findEntryByPath $ archive)) maybeRelPaths
        foundEntries = catMaybes maybeEntries

makeWebBook :: (MonadIO m)
            => FilePath
            -> Archive
            -> [(FilePath, B.ByteString)]
            -> Toc
            -> m ()
makeWebBook outputDir archive inputDocuments toc = do
  (state, result) <- liftIO $ runIOSLA
    (emptyRoot >>> setTraceLevel 0 >>> compileWebBook inputDocuments' toc) initState undefined

  -- Filter any routed documents that have been transformed.
  let resultPaths = map fst result
      filteredRoutes = M.filter (not . (`elem` resultPaths)) (fileRoutes $ xioUserState state)
      -- Make everything relative to the output directory.
      fileRoutes' = M.map (outputDir </>) filteredRoutes
      result' = map (first (outputDir </>)) result
      allPaths = resultPaths ++ (M.elems fileRoutes')

  liftIO $ putStrLn $ show resultPaths

  -- Create directories and write out the result
  liftIO $ mapM_ (createDirectoryIfMissing True . takeDirectory) allPaths
  liftIO $ mapM_ (uncurry writeFile) result'
  -- Extract from the archive any other files that were referenced in the documents.
  liftIO $ mapM_ (\(k, v) -> maybe
                             (putStrLn ("Could not find " ++ k ++ "!") >> return ())
                             (B.writeFile v . fromEntry)
                             $ findEntryByPath k archive) (M.toList fileRoutes')
  where
    emptyRoot = root [] []
    inputDocuments' = map (fmap UTF8.toString) inputDocuments
    initState = initialState $ CompilationState M.empty ""
