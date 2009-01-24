module Main where

import GHC
import Scion
import MonadUtils ( liftIO )
import qualified StringBuffer as SB
import Outputable
import HscTypes

import Data.Maybe
import Control.Monad
import System.Environment

import Scion.Types (CabalComponent(..))

main = runScion $ do
  [working_dir, dist_dir] <- liftIO $ getArgs
  setWorkingDir working_dir
  liftIO $ print working_dir
  liftIO $ print dist_dir
  openCabalProject working_dir dist_dir
  setDynFlagsFromCabal Library
  setTargetsFromCabal Library
  --load LoadAllTargets
  flip foldModSummaries () $ \n ms -> do 
    liftIO $ putStrLn $ 
       showSDoc $ hang (ppr (ms_mod ms) <+> text (hscSourceString (ms_hsc_src ms)))
                       4 (ppr (ms_imps ms))
    return ()
  --liftIO $ print ("total", cs)
