module Main where
import Chess

main :: IO ()
main = putStrLn $ show $ movesOf (0,1) start
