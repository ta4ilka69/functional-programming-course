module Main where

import CommandLineParser (Settings (..), parseArgs)
import Interpolation (Algorithm (..), Point, lagrangeInterpolation, linearInterpolation)
import StreamProcessing (processStream, parsePoint)
import System.Environment (getArgs)
import System.IO (isEOF)

main :: IO ()
main = do
  args <- getArgs
  let (algorithms, rate) = parseArgs args
  loop [] algorithms rate

loop :: [Point] -> [String] -> Double -> IO ()
loop points algorithms rate = do
  end <- isEOF
  if end
    then return ()
    else do
      line <- getLine
      let newPoint = parsePoint line
      let updatedPoints = points ++ [newPoint]
      when ("linear" `elem` algorithms && length updatedPoints == 2) $
        printInterpolation (linearInterpolation rate updatedPoints)

      when ("lagrange" `elem` algorithms && length updatedPoints >= 4) $
        printInterpolation (lagrangeInterpolation rate updatedPoints)

      loop updatedPoints algorithms rate

printInterpolation :: [Point] -> IO ()
printInterpolation = mapM_ (putStrLn . formatPoint)

formatPoint :: Point -> String
formatPoint (x, y) = show x ++ "\t" ++ show y