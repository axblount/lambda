module Main where

import Lambda

main :: IO ()
main = putStrLn $
         case parseString "λx. λy. x" of
           Left err -> "PARSE ERROR: " ++ (show err)
           Right x -> show $ evalLambda x