module Main where
import Test.HUnit
import Scion.Utils (camelCaseMatch)
import Scion.Config.Sh (ShParserState(..), shConfigParser)

import Control.Exception
import qualified Data.Map as M
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

testShConfigParser = TestList [ test1, test2, test3 ]
  where
    runTest :: String -> [(String, String)] -> ShParserState -> Test
    runTest inText inEnv result =
        case runParser shConfigParser (ShParserState M.empty []) "test-string" inText of
          Left e -> TestCase $ assertFailure $ show e
          Right r -> result ~=? r

    test1 = "sh config parser 1" ~: runTest
      "a=7\nb=8" [] 
      (ShParserState (M.fromList [("a", "7"), ("b", "8")]) [] )

    test2 = "sh config parser 2" ~: runTest 
      "cmd\nfoo" [] 
      (ShParserState M.empty [["cmd"], ["foo"]])

    test3 = "sh config parser 2" ~: runTest 
      "CABAL_SETUP=bar\n$CABAL_SETUP" [] 
      (ShParserState (M.fromList [("CABAL_SETUP","bar")]) [["bar"]])

testCamelCaseMatch = TestList $ zipWith (~?)
  [ camelCaseMatch "sH" "simpleHTTP"
  , camelCaseMatch "siH" "simpleHTTP"
  , camelCaseMatch "sHTTP" "simpleHTTP"
  , camelCaseMatch "pSL" "putStrLn"
  , camelCaseMatch "lM" "liftM"
  , camelCaseMatch "DS" "Data.Set"
  , camelCaseMatch "DatS" "Data.Set"
  ] $ map show [1..]

main =
  runTestTT $ TestList 
    [ testShConfigParser 

    , testCamelCaseMatch

    ]
