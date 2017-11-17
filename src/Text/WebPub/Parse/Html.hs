module Text.WebPub.Parse.Html
  ( xpHtmlToc
  , HtmlTocTree (..)
  )
  where

import Control.Monad.Except hiding (when)

import Text.XML.HXT.Core

import Text.WebPub.Data.Toc
import Text.WebPub.Parse.Util

newtype HtmlTocTree = HtmlTocTree { unHtmlT :: TocTree }
  deriving (Show)

newtype HtmlTocEntry = HtmlTocEntry { unHtmlE :: TocEntry }
  deriving (Show)

instance XmlPickler HtmlTocTree where
  xpickle = xpElem "li"
            -- $ (xpFilterAttr $ hasAttr "id" >>> hasAttr "playOrder")
            $ xpWrap ( unHtmlE *** map unHtmlT >>> uncurry TocNode >>> HtmlTocTree
                     , (\(TocNode e t) -> (HtmlTocEntry e, map HtmlTocTree t)) . unHtmlT
                     )
            $ xpPair xpickle (xpList $ xpElem "ul" xpickle)

instance XmlPickler HtmlTocEntry where
  xpickle = xpElem "a"
            $ xpWrap ( HtmlTocEntry . uncurry TocEntry
                   , (\(TocEntry l t) -> (l, t)) . unHtmlE
                   )
            $ xpPair
              (xpText)
              (xpAttr "href" xpText)

xpHtmlToc :: PU Toc
xpHtmlToc = xpElemWithAttrValue "nav" "epub:type" "toc"
            $ xpElem "ul" $ xpList1 $ xpWrap (unHtmlT, HtmlTocTree) xpickle
