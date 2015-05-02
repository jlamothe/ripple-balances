-- Ripple Balances
-- Copyright (C) 2015 Jonathan Lamothe <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main (main) where

import qualified Network.Ripple as Ripple
import Network.Ripple.Balances
import System.Exit (exitSuccess, exitFailure)
import Test.HUnit ( Test (..)
                  , Counts (..)
                  , runTestTT
                  , (@=?)
                  )

main :: IO ()
main = runTestTT tests >>= checkCounts

tests = TestList [filterBalancesTest]

filterBalancesTest = TestLabel "filterBalances" $
  TestCase $ [nonzero] @=? filterBalances [zero, nonzero]
  where
    zero = Ripple.Balance 0 "" Nothing
    nonzero = Ripple.Balance 100 "" Nothing

checkCounts :: Counts -> IO ()
checkCounts counts = if (errors counts > 0 || failures counts > 0)
  then exitFailure
  else exitSuccess

-- jl
