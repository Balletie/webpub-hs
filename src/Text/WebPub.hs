module Main
  (
    main
  )
  where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Codec.Archive.Zip
import Codec.Epub.IO
import Codec.Epub.Parse
import Codec.Epub.Data.Manifest
import Codec.Epub.Data.Spine
import Codec.Epub.Format
import Control.Monad.Except
import System.FilePath

main :: IO ()
main = do
  result <- runExceptT $ do
    let epubPath = "./Five_Faculties_171022.epub"
    zipFileBs <- liftIO $ B.readFile epubPath
    zipArchive <- return $ toArchive zipFileBs

    (path, packageXml) <- getPkgPathXmlFromZip epubPath
    spine <- getSpine packageXml
    Manifest mis <- getManifest packageXml

    tocItem <- return $ find ((spineToc spine ==) . mfiId) mis
    tocPath <- return $ mfiHref <$> tocItem
    tocPath <- return $ (takeDirectory path </>) <$> tocPath
    let findEntryInArchiveByPath = (flip findEntryByPath) zipArchive
    zipEntry <- return $ findEntryInArchiveByPath =<< tocPath
    liftIO $ putStrLn $ maybe
      ("No file found at " ++ show tocPath)
      (C.unpack . fromEntry)
      zipEntry

  either putStrLn return result
