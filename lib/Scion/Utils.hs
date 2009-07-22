{-# OPTIONS_GHC -fno-warn-orphans -XPatternGuards -XScopedTypeVariables #-}
-- |
-- Module      : Scion.Utils
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Various utilities.
--
module Scion.Utils where

import Scion.Types

import GHC              ( GhcMonad, ModSummary, spans, getLoc, Located
                        , depanal, topSortModuleGraph, TypecheckedMod
                        , mkPrintUnqualifiedForModule, moduleInfo )
import Digraph          ( flattenSCCs )
import Outputable

import Control.Monad
import Data.Maybe ( fromMaybe )

import Data.Char (isLower, isUpper)

import Text.JSON

import Data.Foldable (foldlM)

import System.FilePath

import System.Directory (doesFileExist)

thingsAroundPoint :: (Int, Int) -> [Located n] -> [Located n]
thingsAroundPoint pt ls = [ l | l <- ls, spans (getLoc l) pt ]

modulesInDepOrder :: GhcMonad m => m [ModSummary]
modulesInDepOrder = do
  gr <- depanal [] False
  return $ flattenSCCs $ topSortModuleGraph False gr Nothing
  
-- in dep-order
foldModSummaries :: GhcMonad m =>
                    (a -> ModSummary -> m a) -> a
                 -> m a
foldModSummaries f seed =
  modulesInDepOrder >>= foldM f seed


expectJust :: String -> Maybe a -> a
expectJust _ (Just a) = a
expectJust msg Nothing = 
    dieHard $ "Just x expected.\n  grep for \"" ++ msg ++ "\""

unqualifiedForModule :: TypecheckedMod m => m -> ScionM PrintUnqualified
unqualifiedForModule tcm = do
  fromMaybe alwaysQualify `fmap` mkPrintUnqualifiedForModule (moduleInfo tcm)

second :: (a -> b) -> (c, a) -> (c, b)
second f (x,y) = (x, f y)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cm tm em = do
  c <- cm
  if c then tm else em


------------------------------------------------------------------------
-- JSON helper functions

lookupKey :: JSON a => JSObject JSValue -> String -> Result a
lookupKey = flip valFromObj

makeObject :: [(String, JSValue)] -> JSValue
makeObject = makeObj

------------------------------------------------------------------------------


-- an alternative to the broken Fuzzy module
-- match sH simpleHTTP
-- match siH simpleHTTP
-- match sHTTP simpleHTTP
-- match pSL putStrLn
-- match lM liftM
-- match DS Data.Set
camelCaseMatch :: String -> String -> Bool
camelCaseMatch (c:cs) (i:is)
  | c == i = (camelCaseMatch cs $ dropWhile (\c' -> isLower c' || c' == '.') . dropWhile isUpper $ is)
          || camelCaseMatch cs is -- to allow siH match simpleHTTP
  | otherwise = False
camelCaseMatch [] [] = True
camelCaseMatch [] _ = False
camelCaseMatch _ [] = False


instance JSON CabalConfiguration where
  readJSON (JSObject obj)
    | Ok "bulid-configuration" <- lookupKey obj "type"
    , Ok distDir' <- lookupKey obj "type"
    , Ok args <- lookupKey obj "extra-args"
    , Ok args2        <- readJSONs args
    = return $ CabalConfiguration distDir' args2
  readJSON _ = fail "CabalConfiguration"
  showJSON _ = error "TODO showJSON CabalConfiguration"

projectConfigFromCabalFile :: FilePath -> ScionM ScionProjectConfig
projectConfigFromCabalFile = parseScionProjectConfig . (</> ".scion-config") . takeDirectory

-- TODO ensure file handle is closed! 
parseScionProjectConfig :: FilePath -> ScionM ScionProjectConfig
parseScionProjectConfig path = do
    de <- liftIO $ doesFileExist path
    if de
      then do
        (lines' :: [String] ) <- liftIO $ liftM lines $ readFile path
        jsonParsed <- mapM parseLine lines'
        foldlM parseJSON emptyScionProjectConfig jsonParsed
      else return emptyScionProjectConfig
  where 
    parseLine :: String -> ScionM JSValue    
    parseLine l = case decodeStrict l of
      Ok r -> return r
      Error msg -> fail $  "error parsing configuration line" ++ l ++ " error : " ++ msg
    parseJSON :: ScionProjectConfig -> JSValue -> ScionM ScionProjectConfig
    parseJSON pc json = case readJSON json of
      Ok bc -> return $ pc { buildConfigurations = bc : buildConfigurations pc }
      Error msg -> fail $ "invalid JSON object " ++ (show json) ++ " error :" ++ msg
