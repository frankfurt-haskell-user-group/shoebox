{-# LANGUAGE OverloadedStrings #-}
module ShoeboxDataSpec(main, spec) where

import Test.Hspec
import Shoebox.Data
import Shoebox.Basics
import Data.Typeable
import Data.Map as M

main :: IO ()
main = hspec spec 

spec :: Spec
spec = spec1 >> spec2

spec1 :: Spec
spec1 = describe "Update Interface" $ do

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

  it "can detect missing tags in DB" $ do
    dbCheckTag  lexDB refTag
      `shouldBe` False

  it "can detect existing tags in DB" $ do
    dbCheckTag  lexDB leTag
      `shouldBe` True

  it "can insert new data into empty db" $ do
    dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      `shouldBe` Right ( SBDatabase
            (SBDataSchema 
                leTag
                [
                    SBDataRowDef leTag "lexical value" SbtText,
                    SBDataRowDef meTag "meaning" SbtTextArray,
                    SBDataRowDef coTag "comment" SbtText
                ])
            (M.fromList [
              ( SbeText "maison", [
                SBDataRow meTag (SbeTextArray ["Haus"])
                ] )
              ]))

  it "can update data in existing key" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbUpdate db (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus-Update"]))
        `shouldBe` Right ( SBDatabase
              (SBDataSchema 
                  leTag
                  [
                      SBDataRowDef leTag "lexical value" SbtText,
                      SBDataRowDef meTag "meaning" SbtTextArray,
                      SBDataRowDef coTag "comment" SbtText
                  ])
              (M.fromList [
                ( SbeText "maison", [
                  SBDataRow meTag (SbeTextArray ["Haus-Update"])
                  ] )
                ]))

  it "can add data to existing key" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
        `shouldBe` Right ( SBDatabase
              (SBDataSchema 
                  leTag
                  [
                      SBDataRowDef leTag "lexical value" SbtText,
                      SBDataRowDef meTag "meaning" SbtTextArray,
                      SBDataRowDef coTag "comment" SbtText
                  ])
              (M.fromList [
                ( SbeText "maison", [
                  SBDataRow meTag (SbeTextArray ["Haus"]),
                  SBDataRow coTag (SbeText "comment to Haus")
                  ] )
                ]))

  it "can add data to existing key and keep others" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      let (Right db') = dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
      dbUpdate db' (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus-update", "cool"]))
        `shouldBe` Right ( SBDatabase
              (SBDataSchema 
                  leTag
                  [
                      SBDataRowDef leTag "lexical value" SbtText,
                      SBDataRowDef meTag "meaning" SbtTextArray,
                      SBDataRowDef coTag "comment" SbtText
                  ])
              (M.fromList [
                ( SbeText "maison", [
                  SBDataRow meTag (SbeTextArray ["Haus-update", "cool"]),
                  SBDataRow coTag (SbeText "comment to Haus")
                  ] )
                ]))  

  it "can detect insert with wrong key type as error" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbUpdate db (SbeNumber 2) (SBDataRow coTag (SbeText "comment to Haus"))
        `shouldBe` (Left SbErrorUpdateWrongKeyType)

  it "can detect insert with wrong data type as error" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbUpdate db (SbeText "maison") (SBDataRow refTag (SbeText "comment to Haus"))
        `shouldBe` (Left SbErrorUpdateWrongDataType)

  it "can detect insert with key tag being in data as error" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbUpdate db (SbeText "maison") (SBDataRow leTag (SbeText "updated maison"))
        `shouldBe` (Left SbErrorUpdateDataRowWithKeyTag)

spec2 :: Spec
spec2 = describe "Query Interface" $ do


  it "can query data by key and tag" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      let (Right db') = dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
      dbQuery db' (SbeText "maison") coTag
        `shouldBe` (Right (Just (SbeText "comment to Haus")))

  it "can query data by key and tag with empty result set - 1" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      let (Right db') = dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
      dbQuery db' (SbeText "empty") coTag
        `shouldBe` (Right (Nothing))

  it "can query data by key and tag with empty result set - 2" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      dbQuery db (SbeText "maison") coTag
        `shouldBe` (Right (Nothing))

  it "can detect key type error during query" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      let (Right db') = dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
      dbQuery db' (SbeTextArray ["empty"]) coTag
        `shouldBe` (Left SbErrorQueryWrongKeyType)

  it "can detect tag error during query" $ do
      let (Right db) = dbUpdate lexDB (SbeText "maison") (SBDataRow meTag (SbeTextArray ["Haus"])) 
      let (Right db') = dbUpdate db (SbeText "maison") (SBDataRow coTag (SbeText "comment to Haus"))
      dbQuery db' (SbeText "maison") refTag
        `shouldBe` (Left SbErrorQueryWrongDataTag)
