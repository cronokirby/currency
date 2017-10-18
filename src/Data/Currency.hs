{-|
Module : Data.Currency
Description : Simple module for formatting currency
-}
module Data.Currency 
    ( -- * Format Types
      -- | These types are used to specify the details of a how a currency should be formatted
      -- These types can and should be generated automatically from a locale / currency
      Placement (..)
    , Format (..)
    ) where

import Data.Text (Text)
import Data.Word (Word64)


-- | Used to indicate where to place one element with regards to another.
-- For example, to place the $ sign before or after the number
data Placement
    = Front -- ^ In the front
    | Back -- ^ In the back


-- | Represents the formatting options for a currency
data Format = Format 
    { displayDecimals :: Word64 -- ^ How many decimals to display? (>= 0)
    , decimalSep :: Char  -- ^ What character to seperate decimals with?
    , groupSep :: Char -- ^ What character to seperate groups to the left of the decimal with?
    , groupSize :: Word64 -- ^ How many characters to group by? (> 0)
    , symbol :: Text -- ^ What symbol represents this currency?
    , symbolPlacement :: Placement -- ^ Where to place this symbol?
    , negativePlacement :: Placement -- ^ Where to place the negative symbol (-)?
    }
