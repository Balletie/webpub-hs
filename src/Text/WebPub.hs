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

import           Text.WebPub.IO ( getPkgPathXmlFromZip, getTocXmlFromZip
                                , getDocumentsFromZip, makeWebBook)
import           Text.WebPub.Parse (getNcxToc)

import           System.FilePath (takeFileName, takeDirectory, (</>))

outputDir :: FilePath
outputDir = "./_result/"

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

    toc <- getNcxToc tocXml

    inputDocuments <- getDocumentsFromZip spine manifest zipArchive path
    makeWebBook outputDir zipArchive inputDocuments toc

  either putStrLn return result
