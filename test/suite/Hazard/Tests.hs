module Hazard.Tests
  ( tests ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

import Hazard

tests :: [Test]
tests = [ testDummy
        ]

testDummy :: Test
testDummy = testCase "dummy" $ do
    hazard
    assertEqual "dummy test" "foo" "foo"
