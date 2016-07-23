{-# LANGUAGE OverloadedStrings #-}
module Nirum.VersionSpec where

import Data.Text (unpack)
import qualified Data.SemVer as SV
import Test.Hspec.Meta

import Nirum.Version (version, versionString, versionText)

spec :: Spec
spec = do
    describe "version" $ do
        it "must not error" $
            SV.toText version `shouldBe` versionText
            -- assertion is meaningless; it's just for testing whether
            -- `version` is evaluated without error
        it "is development version yet" $
            version `shouldSatisfy` SV.matches (SV.Lt (SV.SemVer 1 0 0 []))
        it "is the proper version" $
            -- is it a necessary test?
            version `shouldBe` SV.semver 0 1 0
    describe "versionText" $ do
        it "is equivalent to version" $
            versionText `shouldBe` SV.renderSV version
        it "is equivalent to versionString" $
            unpack versionText `shouldBe` versionString
    describe "versionString" $
        it "is equivalent to version" $
            versionString `shouldBe` SV.renderSV' version
