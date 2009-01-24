import GHC
import Scion
import Scion.Utils
import Outputable
import GHC.SYB.Utils

main = runScion $ do
  addTarget =<< guessTarget "./tests/Test001.hs" Nothing
  Scion.load LoadAllTargets
  [ms] <- modulesInDepOrder
  mod <- typecheckModule =<< parseModule ms
  let Just (grp, _, _, _, _) = renamedSource mod
  let bnds = typecheckedSource mod
  --io $ putStrLn $ showSDoc $ ppr $ grp
  let tyclds = thingsAroundPoint (15,12) (hs_tyclds grp)
  let ValBindsOut valds _ = hs_valds grp

  liftIO $ putStrLn $ showData TypeChecker 2 bnds
  return ()

