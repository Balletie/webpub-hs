{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}
module Main
  (
    main
  )
  where

import qualified Data.ByteString.Lazy as B

import           Control.Arrow ((&&&))
import           Control.Monad.Except

import           Codec.Archive.Zip
import           Codec.Epub.Parse (getManifest, getSpine)
import           Codec.Epub.Data.Manifest
import           Codec.Epub.Data.Spine

import           Text.WebPub.IO (getPkgPathXmlFromZip, getTocXmlFromZip, getDocumentsFromZip)
import           Text.WebPub.Parse (getNcxToc, reverseNcxToc, reverseHtmlToc)

import           System.FilePath (takeFileName, takeDirectory, (</>))


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

    inputDocuments <- getDocumentsFromZip spine manifest zipArchive path
    liftIO $ putStrLn tocXml
    liftIO $ putStrLn $ show toc
    liftIO $ putStrLn tocHtml

  either putStrLn return result
