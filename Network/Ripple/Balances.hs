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

module Network.Ripple.Balances ( getAccount
                               , filterBalances
                               , identifyBalances
                               , showBalances
                               ) where

import qualified Network.Ripple as Ripple
import System.Environment

getAccount :: IO Ripple.Account
getAccount = getAddr >>= return . Ripple.buildAccount

filterBalances :: [Ripple.Balance] -> [Ripple.Balance]
filterBalances = filter (\bal -> Ripple.balanceValue bal /= 0)

identifyBalances :: [Ripple.Balance] -> IO [Ripple.Balance]
identifyBalances = sequence . map identifyBalance

showBalances :: [Ripple.Balance] -> IO ()
showBalances = undefined

getAddr :: IO String
getAddr = do
  args <- getArgs
  progName <- getProgName
  case args of
    [x] -> return x
    _   -> error $ "usage: " ++ progName ++ " <address/username>"

identifyBalance :: Ripple.Balance -> IO Ripple.Balance
identifyBalance = undefined

-- jl
