module Main where

import Intro01

main :: IO ()
main = putStrLn . show $ luhn 5594589764218858
