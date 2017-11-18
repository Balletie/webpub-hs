{-# LANGUAGE FlexibleContexts #-}
module Text.WebPub.Parse
  ( getNcxToc
  , reverseNcxToc
  , reverseHtmlToc
  )
  where

import Text.XML.HXT.Core

import Text.WebPub.Data.Toc
import Text.WebPub.Parse.Ncx
import Text.WebPub.Parse.Html
import Text.WebPub.Parse.Util

import Control.Arrow.ListArrows ( (>>>), (/>) )
import Control.Monad.Except hiding (when)

getNcxToc :: (MonadError String m, MonadIO m) => String -> m Toc
getNcxToc contents = do
  result <- liftIO $ runX (
    readString [withValidate no, withSubstDTDEntities no, withRemoveWS yes] contents
    >>> getChildren `when` isRoot
    >>> propagateNamespaces
    >>> hasQName (ncxName "ncx")
    /> hasQName (ncxName "navMap")
    >>> xunpickleVal xpNcxToc
    )

  case result of
    (r:[]) -> return r
    _      -> throwError $ "Couldn't parse ToC!" ++ show result

reverseToc :: (MonadError String m, MonadIO m) => PU Toc -> Toc -> m String
reverseToc pu = return . concat . (
  pickleDoc pu >>> runLA (writeDocumentToString [ withIndent yes
                                                , withAddDefaultDTD yes
                                                ]))

reverseHtmlToc :: (MonadError String m, MonadIO m) => Toc -> m String
reverseHtmlToc = reverseToc xpHtmlToc

reverseNcxToc :: (MonadError String m, MonadIO m) => Toc -> m String
reverseNcxToc = reverseToc xpNcxToc
