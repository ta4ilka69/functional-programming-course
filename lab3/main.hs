module Main where

import CommandLineParser (Settings (..), parseArgs)
import Control.Monad (void, when)
import Data.List (sort)
import Interpolation (Algorithm (..), Point, lagrangeInterpolation, linearInterpolation)
import StreamProcessing (parsePoint, processStream)
import System.Environment (getArgs)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> void (putStrLn err)
    Right settings -> loop [] settings

loop :: [Point] -> Settings -> IO ()
loop points (Settings algorithms rate) = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      let newPoint = parsePoint line
      putStrLn ""
      case newPoint of
        Nothing -> loop points (Settings algorithms rate)
        Just pt -> do
          let updatedPoints = pt : points
          when ((Linear == algorithms || Both == algorithms) && length updatedPoints >= 2) $ do
            putStrLn "Линейная (по последним двум):"
            printInterpolation (linearInterpolation rate (sort $ take 2 updatedPoints))
            putStrLn ""

          when ((Lagrange == algorithms || Both == algorithms) && length updatedPoints >= 4) $ do
            putStrLn "Лагранж (по последним четырём):"
            printInterpolation (lagrangeInterpolation rate (sort $ take 4 updatedPoints))
            putStrLn ""
          loop updatedPoints (Settings algorithms rate)

printInterpolation :: [Point] -> IO ()
printInterpolation = mapM_ (putStrLn . formatPoint)

formatPoint :: Point -> String
formatPoint (x, y) = show x ++ "\t" ++ show y
