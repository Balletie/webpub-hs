module Text.WebPub.Parse.Ncx
  ( xpNcxToc
  , NcxTocTree (..)
  )
  where

import Control.Arrow.ListArrows ( (>>>), (/>) )
import Control.Monad.Except hiding (when)

import Text.XML.HXT.DOM.QualifiedName (mkQName)
import Text.XML.HXT.Core

import Text.WebPub.Data.Toc
import Text.WebPub.Parse.Util

newtype NcxTocTree = NcxTocTree { unNcxT :: TocTree }
  deriving (Show)

newtype NcxTocEntry = NcxTocEntry { unNcxE :: TocEntry }
  deriving (Show)

instance XmlPickler NcxTocTree where
  xpickle = ncxElem "navPoint"
            $ (xpFilterAttr $ hasAttr "id" >>> hasAttr "playOrder")
            $ xpWrap ( unNcxE *** map unNcxT >>> uncurry TocNode >>> NcxTocTree
                     , (\(TocNode e t) -> (NcxTocEntry e, map NcxTocTree t)) . unNcxT
                     )
            $ xpPair xpickle (xpList xpickle)

instance XmlPickler NcxTocEntry where
  xpickle = xpWrap ( NcxTocEntry . uncurry TocEntry
                   , (\(TocEntry l t) -> (l, t)) . unNcxE
                   )
            $ xpPair
              (ncxElem "navLabel" $ ncxElem "text" $ xpText)
              (ncxElem "content" $ xpAttr "src" $ xpText)

xpNcxToc :: PU Toc
xpNcxToc = ncxElem "navMap" $ xpList1 $ xpWrap (unNcxT, NcxTocTree) xpickle
