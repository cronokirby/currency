{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.Currency

main :: IO ()
main = hspec $ do
    formatWithTest


formatWithTest = describe "formatNum" $ do
    it "formats a simple format" $ 
        formatNum format 1200300.106 == "$1,200,300.11"
    it "formats negatives" $
        formatNum format (-1200300.106) == "-$1,200,300.11"
    it "formats different orders" $
        formatNum (format {negativeToSymbol = Before}) (-1200.189) == "$-1,200.19"
    it "formats without decimals" $
        formatNum format 1200 == "$1,200"
  where
    format = Format 2 "." "," 3 "$" Before Before After

