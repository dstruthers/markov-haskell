module Markov (initialState, yield, chain) where
import Control.Monad.State
import System.Random

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n x@(_:xs) = take n x : chop n xs

parse :: Int -> [a] -> [[a]]
parse n xs = filter (\x -> length x == n) $ chop n xs

data MarkovState a = MarkovState [[a]] [a] StdGen

initialState :: Int -> [a] -> IO (MarkovState a)
initialState n xs = do
  g <- newStdGen
  let bank    = parse n xs
      (i, g') = randomR (0, length bank - 1) g
  return $ MarkovState bank (bank !! i) g'

yield :: (Eq a) => StateT (MarkovState a) IO a
yield = do
  (MarkovState bank current g) <- get
  let n         = length current - 1
      eligible  = let e = filter (\x -> take n x == tail current) bank
                  in if length e > 0 then e else bank
      (i, g')   = randomR (0, length eligible - 1) g
      next      = eligible !! i
  put (MarkovState bank next g')
  return $ head current
  
chain :: (Eq a) => Int -> [a] -> Int -> IO [a]
chain n xs l = do
  start <- initialState n xs
  liftM fst $ runStateT (replicateM l yield) start
