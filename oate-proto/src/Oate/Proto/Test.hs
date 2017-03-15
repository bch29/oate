module Oate.Proto.Test where

import qualified Data.Text.IO              as Text
import           Text.Megaparsec

import           Oate.Proto.Parser
import           Oate.Proto.Printer.Opc
import           Oate.Proto.Types

test :: IO ()
test = do
  fileContents <- readFile "editor.opc"
  res <- either (fail . parseErrorPretty) return (parse parseProto' "editor.opc" fileContents)

  putStrLn "Parsed. AST representation:"
  putStrLn "---------------------------"
  print res
  putStrLn "---------------------------"
  putStrLn "Pretty printed:"
  putStrLn "---------------------------"
  Text.putStr (pprProto res)
  putStrLn "---------------------------"

parseProto' :: Parsec Dec String Proto
parseProto' = parseProto
