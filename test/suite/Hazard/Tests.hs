module Hazard.Tests
  ( tests ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, path)

tests :: [Test]
tests = [ testDummy
        ]

testDummy :: Test
testDummy = testCase "dummy" $ do assertEqual "dummy test" "foo" "foo"
