module Text.WebPub.Data.Toc
  ( Toc
  , TocTree (..)
  , TocEntry (..)
  )
  where

type Toc = [TocTree]

data TocTree = TocNode TocEntry [TocTree]
  deriving (Eq, Show)

data TocEntry = TocEntry
  { label :: String  -- ^ The label of this entry.
  , target :: String -- ^ The target of this entry where it points to.
  }
  deriving (Eq, Show)
