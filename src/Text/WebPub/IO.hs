{-# LANGUAGE FlexibleContexts #-}
module Text.WebPub.IO
  (
    getPkgPathXmlFromZip
  , getTocXmlFromZip
  )
  where

import           Data.List
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy.Char8 as C

import           Control.Arrow.ListArrows ( (>>>), deep )
import           Control.Monad.Except

import           Text.XML.HXT.Arrow.ReadDocument ( readString )
import           Text.XML.HXT.Arrow.XmlArrow ( getAttrValue, hasName, isElem )
import           Text.XML.HXT.Arrow.XmlState ( no, runX, withValidate )

import           Codec.Archive.Zip
import           Codec.Epub.Data.Spine
import           Codec.Epub.Data.Manifest
import           Codec.Epub.Data.Common

import           System.FilePath

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
-- through the manifest for the filepath, using the given manifest item ID.
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
