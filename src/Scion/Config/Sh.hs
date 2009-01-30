{-# LANGUAGE ScopedTypeVariables, CPP, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances,
    FlexibleInstances, OverlappingInstances #-}
module Scion.Config.Sh where
-- |
-- Module      : Scion.Config.Sh
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : experimental
-- Portability : portable
--
-- read cabal configuration information from a simple .sh file
-- Read the Configuration section in README.markdown to get more information
--
-- this is a quick and dirty implementation.
-- Probably there are some bugs (See test cases in TestVarious.hs)

import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.ReadP (skipSpaces)

import qualified Data.Map as M

import Control.Monad (when, liftM)
import Data.List (concat)

-- simple parser state 
data ShParserState = ShParserState
  { env :: M.Map String String
  , commands :: [[String]]
  }
  deriving (Show, Eq) -- Show, Eq used in TestVarious test case 

-- returns a list of commands and the new environment 
shConfigParser :: CharParser ShParserState ShParserState
shConfigParser = do
  sepBy (try assignment <|> command) (satisfy (== '\n'))
  getState
  where
    assignment = do
      var <- many1 (satisfy (not . (`elem` "\n= \t") ))
      char '='
      val <- arg
      updateState $ \s -> s{env = M.insert var val (env s)}
    command = do
      args <- sepBy1 arg $ satisfy (`elem` " \t")
      when (not . Prelude.null $ args) $
        updateState $ \s -> s{commands = commands s ++ [args]}

endBy' p end = do
  r <- p
  end
  return r

-- "fo"'bar' is one arg parsed by two arg' runs 
arg :: CharParser ShParserState String
arg = try $ do
  liftM concat $ many1 arg'
  where arg' :: CharParser ShParserState String
        arg' = try var <|> (try $ do
          c <- anyToken
          case c of
            '\'' -> do
              str <- endBy' (many $ satisfy (/= '\'')) (char '\'')
                    <?> "ending \' expected"
              return str
            '"' -> do
              str <- liftM concat $
                  many $ choice 
                        [ char '\\' >> liftM (:[]) anyToken
                        , liftM (:[]) $ satisfy $ not . (`elem` "\"$")
                        , try var
                        ]
              char '"' <?> "ending \" expected"
              return str
            '\n' -> fail "no new line expceted"
            ' ' -> fail "no space expected"
            '\t' -> fail "no tab expected"
            -- just a word.. 
            c -> liftM (c:) $ many (satisfy (not . (`elem` " \t'\"'\n")))
          )

var :: CharParser ShParserState String
var = do
  char '$'
  name <- choice
    [ char '{' >> endBy' (many1 $ satisfy (not . (`elem` "\n}"))) (char '}')
    , many1 $ satisfy $ not . (`elem` "\n} \t")
    ]
  map <- liftM env getState
  case M.lookup name map of
    Nothing -> fail $ "name " ++ name ++ " not found in environment"
    Just s -> return s
