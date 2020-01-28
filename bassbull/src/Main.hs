module Main where

import Bassbull

main :: IO ()
main = do
  summed <- getAtBatsSum "batting.csv"
  putStrLn $ "Total atBats was: " ++ (show summed)

-- module Main where

-- import qualified Data.ByteString.Lazy as BL

-- -- No more in-memory collections with streaming
-- -- import qualified Data.Vector as V
-- import qualified Data.Foldable as F

-- -- (1) from cassava default
-- -- import Data.Csv

-- -- to streaming module
-- import Data.Csv.Streaming

-- -- a simple type alias for data
-- type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

-- -- Rewrite as independent function
-- fourth :: (a, b, c, d) -> d
-- fourth (_, _, _, d) = d

-- -- Rewrite as independent function
-- -- (1) baseballStats :: BL.ByteString -> Either String (V.Vector BaseballStats)
-- -- type change to streaming
-- baseballStats :: BL.ByteString -> Records BaseballStats
-- baseballStats = decode NoHeader


-- main :: IO ()
-- main = do
--     csvData <- BL.readFile "batting.csv"

--     -- (1)let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)

--     -- (1) let summed = fmap(V.foldr summer 0) v

--     -- We are incrementally processing the results, not loading the entire dataset into a Vector.

--     let summed = F.foldr summer 0 (baseballStats csvData)

--     putStrLn $ "Total atBats was: " ++ (show summed)

--     -- we only care about `atBats`.  pattern matching!
--     -- (1) where summer (name, year, team, atBats) n = n + atBats
--     -- (2) where summer (_, _, _, atBats) sum = sum + atBats

--     -- (4) where summer r n = n + fourth r

--     -- eta reduction  => (f . g) x = f (g x)
--     where summer = (+) . fourth
