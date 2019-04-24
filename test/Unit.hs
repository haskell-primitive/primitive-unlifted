{-# language BangPatterns #-}

import Data.Primitive.Unlifted.TVar
import Data.Primitive
import Control.Monad.STM

main :: IO ()
main = do
  putStrLn "Start"
  putStrLn "A"
  testA
  -- putStrLn "B"
  -- testB
  putStrLn "Finished"

testA :: IO ()
testA = do
  tv <- newUnliftedTVarIO =<< newByteArray 16
  marr <- newByteArray 8
  !r <- atomically $ do
    writeUnliftedTVar tv marr
    !s <- readUnliftedTVar tv
    pure s
  if sameMutableByteArray marr r
    then pure ()
    else fail ""

testB :: IO ()
testB = do
  tv <- newUnliftedTVarIO =<< newByteArray 16
  !_ <- readUnliftedTVarIO tv
  pure ()
