module Main
where

import UnitTests
import Test.Framework.Runners.Console ( defaultMain )

main = defaultMain $ [UnitTests.tests]

