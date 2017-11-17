{-# LANGUAGE FlexibleContexts, Arrows #-}
module Text.WebPub.Parse
  (
    Toc,
    getToc
  )
  where

import Control.Arrow.ListArrows ( (>>>), (/>) )
import Control.Monad.Except hiding (when)

import Text.XML.HXT.DOM.QualifiedName (mkQName)
import Text.XML.HXT.Arrow.DTDProcessing
import Text.XML.HXT.Core

type Toc = [TocTree]

data TocTree = TocNode TocEntry [TocTree]
  deriving (Eq, Show)

data TocEntry = TocEntry
  { label :: String  -- ^ The label of this entry.
  , target :: String -- ^ The target of this entry where it points to.
  }
  deriving (Eq, Show)

instance XmlPickler TocTree where
  xpickle = ncxElem "navPoint"
            $ (xpFilterAttr $ hasAttr "id" >>> hasAttr "playOrder")
            $ xpWrap (uncurry TocNode
                     , \(TocNode e t) -> (e, t)
                     )
            $ xpPair xpickle (xpList xpickle)

instance XmlPickler TocEntry where
  xpickle = xpWrap (uncurry TocEntry, \(TocEntry l t) -> (l, t)) $
            xpPair
              (ncxElem "navLabel" $ ncxElem "text" $ xpText)
              (ncxElem "content" $ xpAttr "src" $ xpText)

ncxUri = "http://www.daisy.org/z3986/2005/ncx/"
ncxPrefix = "ncx"
ncxName localPart = mkQName ncxPrefix localPart ncxUri
ncxElem = xpElemNS ncxUri ncxPrefix
ncxAttr = xpAttrNS ncxUri ncxPrefix

xpToc :: PU Toc
xpToc = ncxElem "navMap" $ xpList1 xpickle

getToc :: (MonadError String m, MonadIO m) => String -> m Toc
getToc contents = do
  result <- liftIO $ runX (
    readString [withValidate no, withSubstDTDEntities no, withRemoveWS yes] contents
    >>> getChildren `when` isRoot
    >>> propagateNamespaces
    >>> hasQName (ncxName "ncx")
    /> hasQName (ncxName "navMap")
    >>> xunpickleVal xpToc
    )

  case result of
    (r:[]) -> return r
    _      -> throwError $ "Couldn't parse ToC!" ++ show result
