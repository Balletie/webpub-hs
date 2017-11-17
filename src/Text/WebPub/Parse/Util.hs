module Text.WebPub.Parse.Util
  (ncxName, ncxElem, ncxAttr)
  where

import Text.XML.HXT.DOM.QualifiedName (mkQName)
import Text.XML.HXT.Core

ncxUri :: String
ncxUri = "http://www.daisy.org/z3986/2005/ncx/"

ncxPrefix :: String
ncxPrefix = "ncx"

ncxName lp = mkQName ncxPrefix lp ncxUri

ncxElem = xpElemNS ncxUri ncxPrefix

ncxAttr = xpAttrNS ncxUri ncxPrefix
