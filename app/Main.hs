module Main where

import Lib

main :: IO ()
main = do
  Command connectCmd cli <- captureArgs
  case connectCmd of
      SSH -> putStrLn $ "You are going to SSH"
      Tunnel -> putStrLn "You are going to tunnel"
