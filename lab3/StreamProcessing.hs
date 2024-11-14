module StreamProcessing (processStream, parsePoint) where

import CommandLineParser (Settings (..))
import Interpolation
  ( Algorithm (..),
    Point,
    lagrangeInterpolation,
    linearInterpolation,
  )
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)

processStream :: Settings -> IO ()
processStream = streamLoop []

streamLoop :: [Point] -> Settings -> IO ()
streamLoop points settings = do
  eof <- isEOF
  if eof
    then return ()
    else do
      line <- getLine
      case parsePoint line of
        Nothing -> streamLoop points settings
        Just pt -> do
          let newPoints = points ++ [pt]
          processPoints settings newPoints
          let windowSize = if algorithm settings == Linear then 2 else 5
          let updatedPoints = drop (length newPoints - windowSize) newPoints
          streamLoop updatedPoints settings

parsePoint :: String -> Maybe Point
parsePoint input =
  case words input of
    [xStr, yStr] -> do
      x <- readMaybe xStr
      y <- readMaybe yStr
      return (x, y)
    _ -> Nothing

processPoints :: Settings -> [Point] -> IO ()
processPoints (Settings alg rate) pts = do
  let linearResults =
        if alg == Linear || alg == Both
          then linearInterpolation rate (take 2 pts)
          else []
  let lagrangeResults =
        if alg == Lagrange || alg == Both
          then lagrangeInterpolation rate (take 5 pts)
          else []
  mapM_ printPoint (linearResults ++ lagrangeResults)

printPoint :: Point -> IO ()
printPoint (x, y) = putStrLn $ show x ++ "\t" ++ show y
