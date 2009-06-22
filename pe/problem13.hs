#!/usr/bin/env runghc
module Main (main) where

problem13 cs = take 10 (show (foldr1 (+) (map (\x -> (read x :: Integer)) (lines cs))))

main = do
      cs <- readFile "problem13.txt"
      print $ problem13 cs


