{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
module Main
  (
    main
  )
  where

import           Data.List (find)
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B

import           Control.Arrow ((&&&))
import           Control.Monad.Except

import           Codec.Archive.Zip
import           Codec.Epub.Parse (getManifest, getSpine)
import           Codec.Epub.Data.Manifest
import           Codec.Epub.Data.Spine

import           Text.WebPub.IO (getPkgPathXmlFromZip, getTocXmlFromZip)
import           Text.WebPub.Parse (getNcxToc, reverseNcxToc, reverseHtmlToc)

import           System.FilePath (takeFileName, takeDirectory, (</>))

extractWithSpine :: (MonadIO m)
                 => Spine            -- ^ The spine that specifies the order of files.
                 -> Manifest         -- ^ The manifest to find the file locations.
                 -> Archive          -- ^ The EPUB archive that stores the files.
                 -> FilePath         -- ^ The directory of the container, to resolve paths.
                 -> m [(FilePath, B.ByteString)] -- ^ The contents of all the documents.
extractWithSpine spine (Manifest mis) archive relPath =
  return $ map (eRelativePath &&& fromEntry) foundEntries
  where itemRefs = map siIdRef $ spineItemrefs spine
        maybePaths = map (\ref -> mfiHref <$> find ((ref ==) . mfiId) mis) itemRefs
        maybeRelPaths = map (fmap (relPath </>)) maybePaths
        maybeEntries = map (>>= (flip findEntryByPath $ archive)) maybeRelPaths
        foundEntries = catMaybes maybeEntries


main :: IO ()
main = do
  result <- runExceptT $ do
    let epubPath = "./Five_Faculties_171022.epub"
    zipFileBs <- liftIO $ B.readFile epubPath
    zipArchive <- return $ toArchive zipFileBs

    -- Get the path as well, to find relative paths given in the manifest.
    (xmlPath, packageXml) <- getPkgPathXmlFromZip zipArchive

    let path = takeDirectory xmlPath

    spine <- getSpine packageXml
    manifest <- getManifest packageXml

    tocXml <- getTocXmlFromZip manifest spine path zipArchive

    --liftIO $ putStrLn tocXml

    toc <- getNcxToc tocXml
    tocXml' <- reverseNcxToc toc
    tocHtml <- reverseHtmlToc toc

    inputDocuments <- extractWithSpine spine manifest zipArchive path
    liftIO $ putStrLn tocXml
    liftIO $ putStrLn $ show toc
    liftIO $ putStrLn tocHtml

  either putStrLn return result
