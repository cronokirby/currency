{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
      -- * Formatting functions
    , formatNum
    ) where


import Data.Fixed (mod')
import qualified Data.Text as T


-- | Used to indicate where to place one element with regards to another.
-- For example, to place the $ sign before or after the number
data Placement
    = Before -- ^ Before the element
    | After -- ^ After the element


-- | Represents the formatting options for a currency
data Format = Format 
    { displayDecimals :: Int -- ^ How many decimals to display? (>= 0)
    , decimalSep :: T.Text  -- ^ The seperator for the decimals
    , groupSize :: Int -- ^ How many characters to group by? (> 0)
    , groupSep :: T.Text -- ^ The seperator for groups left of the decimal
    , symbol :: T.Text -- ^ What symbol represents this currency?
    , symbolPlacement :: Placement -- ^ Where to place this symbol?
    , negativePlacement :: Placement -- ^ Where to place the negative symbol (-) relative to the number?
    , negativeToSymbol :: Placement -- ^ Place the negative symbol before or after placing the currency? (irrelevant if on different sides)
    }


-- Utility function to split from end
splitAtEnd :: Int -> T.Text -> (T.Text, T.Text)
splitAtEnd n t = (T.dropEnd n t, T.takeEnd n t)

chunksOfEnd :: Int -> T.Text -> [T.Text]
chunksOfEnd k = reverse . go
  where
    go t = case splitAtEnd k t of
            (a, b) | T.null a -> [b]
                   | otherwise -> b : go a


-- Turns a placement into a function for building text
place :: Placement -> T.Text -> T.Text -> T.Text
place Before = T.append
place After = flip T.append


-- Given a format, and a number converts the number to a currency 
-- this assumes the decimals have already been rounded
formatWith :: Format -> Bool -> T.Text -> T.Text -> T.Text
formatWith Format{..} isNegative front decimals = 
    let frontChunks = chunksOfEnd groupSize front
        seppedFront = T.intercalate groupSep frontChunks
        body = if decimals == ""
            then seppedFront
            else T.concat [seppedFront, decimalSep, decimals]
        placeSymbol = place symbolPlacement symbol
        placeNeg = if isNegative then place negativePlacement "-" else id
    in case negativeToSymbol of
        Before ->
            placeSymbol (placeNeg body)
        After ->
            placeNeg (placeSymbol body)


-- | Formats a number with a given format
-- Examples:
-- >>> formatNum (Format 2 "." "," 3 "$" Before Before After) (-1200300.106)
-- "-$1,200,300.11"
formatNum :: RealFrac n => Format -> n -> T.Text
formatNum f num = formatWith f isNegative notDecimals nonEmptyDecimals
  where
    corrected = num * signum num
    isNegative = negate num == abs num
    toText = T.pack . show
    notDecimals = toText (floor corrected)
    nDecimals = round ((10 ^ displayDecimals f) * mod' corrected 1)
    nonEmptyDecimals = if nDecimals == 0
        then ""
        else toText nDecimals