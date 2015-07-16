module Markov (initialState, yield, chain) where
import Control.Monad.State
import System.Random

chop :: Int -> [a] -> [[a]]
chop _     []       = []
chop order x@(_:xs) = take order x : chop order xs

parse :: Int -> [a] -> [[a]]
parse order xs = filter (\x -> length x == order) $ chop order xs

data MarkovState a = MarkovState [[a]] [a] StdGen

initialState :: Int -> [a] -> IO (MarkovState a)
initialState order xs = do
  g <- newStdGen
  let bank    = parse order xs
      (i, g') = randomR (0, length bank - 1) g
  return $ MarkovState bank (bank !! i) g'

yield :: (Eq a) => StateT (MarkovState a) IO a
yield = do
  (MarkovState bank current g) <- get
  let order     = length current - 1
      eligible  = let e = filter (\x -> take order x == tail current) bank
                  in if length e > 0 then e else bank
      (i, g')   = randomR (0, length eligible - 1) g
      next      = eligible !! i
  put (MarkovState bank next g')
  return $ head current
  
chain :: (Eq a) => Int -> [a] -> Int -> IO [a]
chain order xs len = do
  start <- initialState order xs
  evalStateT (replicateM len yield) start
