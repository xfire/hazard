module Hazard.Tests.Core
  ( tests ) where

import Test.Framework
import Test.Framework.Providers.HUnit

import Hazard

import Hazard.Tests.TestUtils

tests :: [Test]
tests = [ normalContentIsCopied
        , specialContentIsNotCopied
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

specialContentIsNotCopied :: Test
specialContentIsNotCopied = testCase
    "special files and directories beginning with an underscore are not copied to the output directory" $ do
        let outdir = "output"
        initOutputDirectory outdir
        hazard $ HConfig { inputDir = "data/mixed"
                         , outputDir = outdir
                         }
        assertDirectoryContents "some of the special files are not ignored by the copy process"
                                outdir
                                ["foo.txt", "bar.html", "someDir", "someDir/someFile"]
