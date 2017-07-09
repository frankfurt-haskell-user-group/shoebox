{-# LANGUAGE OverloadedStrings #-}
module ShoeboxParserSpec(main, spec) where

import Test.Hspec
import Shoebox.Data
import Shoebox.Parser
import Shoebox.Basics
import Data.Typeable
import Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Parser Interface" $ do

  it "can create empty databases for lexDB" $ do
    lexDB
      `shouldBe` SBDatabase
            (SBDataSchema 
                leTag
                [
                    SBDataRowDef leTag "lexical value" SbtText,
                    SBDataRowDef meTag "meaning" SbtTextArray,
                    SBDataRowDef coTag "comment" SbtText
                ])
            (M.fromList [])