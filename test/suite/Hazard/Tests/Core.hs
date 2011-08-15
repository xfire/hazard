module Hazard.Tests.Core
  ( tests ) where

import Test.Framework
import Test.Framework.Providers.HUnit

import Hazard

import Hazard.Tests.TestUtils

tests :: [Test]
tests = [ normalContentIsCopied
        ]

normalContentIsCopied :: Test
normalContentIsCopied = testCase
    "normal files and directories are copied from the input to the output directory" $ do
        let outdir = "output"
        initOutputDirectory outdir
        hazard $ HConfig { inputDir = "data/simple"
                         , outputDir = outdir
                         }
        assertDirectoryContents "some of the normal files or directories are not copied to the output directory"
                                outdir
                                ["foo.txt", "bar.html", "spam.jpg", "eggs", "someDir", "someDir/someFile"]
