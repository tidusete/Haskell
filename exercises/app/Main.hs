{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}
module Main where

import Lib

import Control.Monad
import Data.List 
import Data.List.Split 
import Data.Set hiding ( take,drop )
import System.Environment
import System.IO



timeConversion :: String -> String
timeConversion time = t 
  where
    [h,m,s] = splitOn ":" time
    ls = drop 2 s
    h' = read $ h :: Int
    hr = h' `mod` 12
    hr' = if ls == "AM" then hr else hr+12 
    fh = if (hr' `div` 10 < 1) then "0" else ""
    t = fh++(show hr')++":"++m++":"++(take 2 s)

main :: IO()
main = do
    print $ timeConversion "08:00:00AM"
    print $ timeConversion "10:00:00AM"
    print $ timeConversion "10:00:00PM"
    print $ timeConversion "12:00:00AM"   


