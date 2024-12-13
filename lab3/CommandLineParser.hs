module CommandLineParser (parseArgs, Settings (..)) where

import Interpolation (Algorithm (..))
import Text.Read (readMaybe)

data Settings = Settings
  { algorithm :: Algorithm,
    rate :: Double
  }
  deriving (Show, Eq)

parseArgs :: [String] -> Either String Settings
parseArgs args = case args of
  [alg, rateStr] -> do
    alg' <- parseAlgorithm alg
    rate' <- maybe (Left "Invalid rate") Right (readMaybe rateStr)
    return $ Settings alg' rate'
  _ -> Left "Usage: program <algorithm> <rate>"

parseAlgorithm :: String -> Either String Algorithm
parseAlgorithm alg = case alg of
  "linear" -> Right Linear
  "lagrange" -> Right Lagrange
  "both" -> Right Both
  _ -> Left "Algorithm must be 'linear', 'lagrange', or 'both'"
