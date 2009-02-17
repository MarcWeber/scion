{-# LANGUAGE BangPatterns, DeriveDataTypeable, ScopedTypeVariables,
             TypeFamilies, PatternGuards, CPP #-}
-- |
-- Module      : Main
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : pre-alpha
-- Portability : portable
--
-- the executable run by main in this file can be either daemon or instance
--
--
--
-- goals:
--   * make the client interface as simple as possible.
--     thus: all logic etc should be implemented on server side
--   * it should be possible to provide IDE like features for
--     - editing single file projects without .cabal overhead (eg Setup.hs)
--     - editing cabal files taking into account different
--         configurations
--         flags settings
--
-- problems:
--   the ghc API is using some internal IORefs. This means we must start a
--   different process for each target.
--
-- Conclusion
--
-- [editor]  (emacs,vim, someday Eclipse?)
--   <-> connects to  [scion daemon] lazily launching scion instances
--      <-> [scion intsance] (single project file)
--      <-> [scion instance] (cabal project target executable:foo flags:none)
--      <-> [scion instance] (cabal project target executable:foo flags:different)
--
-- The daemon keeps a list of settings per connection:
--  per connection and file:
--    * project type
--       single file project
--    * cabal project
--        - dist dir
--        - cabal target
--
--  last selected dist dir, cabal target
--  so that when opening another file the last settings can be applied automatically

module Main where
import Prelude hiding ( log )
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (stdin, stdout, hSetBuffering, BufferMode(..))
import qualified System.Log.Logger as HL
import qualified System.Log.Handler.Simple as HL
import qualified System.Log.Handler as HL
import qualified Data.ByteString.Char8 as S
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.List (isPrefixOf, break)
import Data.Foldable (foldrM)
import qualified Data.Map as M
import qualified Control.Exception as E
import ScionInstance ( instanceHandleRequests )


import Control.Monad ( when, forever )

import System.Console.GetOpt

import MonadUtils ( liftIO )
import qualified Scion.Server.ProtocolEmacs as Emacs
import qualified Scion.Server.ProtocolVim as Vim
import qualified Scion.Server.ConnectionIO as CIO
import Scion.Types.Server (ConnectionMode(..))
import Scion (runScion)

#ifdef IS_UNIX
import System.Posix.Files (readSymbolicLink, fileExist)
#endif

log = HL.logM __FILE__
logInfo = log HL.INFO
logDebug = log HL.DEBUG
logError = log HL.ERROR

data StartupConfig = StartupConfig
   { connectionMode :: ConnectionMode
   , showHelp :: Bool
   , scionInstancePath :: Maybe String
   , scionInstance :: Bool
   } deriving Show

-- scion instance = this execucutable started with --scion-instance
thisExecutablePath = do
#ifdef IS_UNIX
  de <- fileExist "/proc/self/exe"
  if de
      then fmap Just $ readSymbolicLink "/proc/self/exe"
      else return Nothing
#else
  -- windows (?) 
  return Nothing
#endif

defaultDaemonStartupConfig :: IO StartupConfig
defaultDaemonStartupConfig = do
  instancePath <- thisExecutablePath
  return $ StartupConfig ( TCPIP (fromInteger 4005))
                False
                Nothing
                False

instance HL.LogHandler () where -- doh! find a better way to pass an empty list below 
  setLevel = error "should never be rearched"
  getLevel = error "should never be rearched"
  emit = error "should never be rearched"
  close = error "should never be rearched"

-- options :: [OptDescr (Options -> Options)]
options =
     [ Option ['p']     ["port"]
       (ReqArg (\o opts -> return $ opts { connectionMode = (TCPIP . fromInteger) (read o) }) "8010")
       "listen on this TCP port"
     , Option ['i'] ["stdinout"]
       (NoArg (\opts -> return $ opts { connectionMode = StdInOut}))
       "client must connect to stdin and stdout"
#ifndef mingw32_HOST_OS
     , Option ['s'] ["socketfile"]
       (ReqArg (\o opts -> return $ opts { connectionMode = Socketfile o}) "/tmp/scion-io")
       "listen on this socketfile"
#endif
     , Option [] ["path-to-this-executable"] 
       (ReqArg (\o opts -> return $ opts { scionInstancePath = Just o } ) "")
       ("tell scion about the path of this executable which is executed multiple times"
        ++ " as scion instance. On linux it should know this by looking up /proc/self/exe")

     , Option ['h'] ["help"] (NoArg (\opts -> return $ opts { showHelp = True } ))
       "show this help"

     , Option ['f'] ["log-file"] (ReqArg (\f opts -> do
          fh <- HL.fileHandler f HL.DEBUG
          -- remove default stderr logger 
          HL.updateGlobalLogger "" (HL.addHandler fh . HL.setHandlers ([] :: [()]) )
          return opts ) "/tmp/scion-log") 
       "log to the given file"
     , Option [] ["scion-instance"] (NoArg (\opts -> return $ opts {scionInstance = True} )) 
       ("scion daemon internal use only. when run using this option the daemon "
        ++ " truns into an instance serving the daemon")
     ]

initializeLogging :: IO ()
initializeLogging = return ()
  -- by default log everything to stderr

helpText = do
    pN <- getProgName
    let header = unlines [ "usage of scion server (executable :"  ++ pN  ++ ")" ]
    return $ usageInfo header options

serve :: ConnectionMode -> IO ()
serve (TCPIP nr) = do
  sock <- liftIO $ listenOn (PortNumber nr)
  forever $ E.handle (\(e::E.IOException) -> logInfo ("caught :" ++ (show e) ++ "\n\nwaiting for next client")) $ do
    (sock', _addr) <- liftIO $ accept sock
    handleClient sock'
serve StdInOut = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  handleClient (stdin, stdout)
#ifndef mingw32_HOST_OS
serve (Socketfile file) = do
  sock <- liftIO $ listenOn (UnixSocket file)
  forever $ do
    -- no multithreading for now (I don't know yet when it may be used.. the
    -- ghc library is using some IO refs)
    (sock', _addr) <- liftIO $ accept sock
    handleClient sock'
#endif


-- does the handshaking and then runs the protocol implementation
handleClient :: (CIO.ConnectionIO con) => con -> IO ()
handleClient con = do
  logDebug $ "waiting for greeting"
  greeting <- CIO.getLine con
  logDebug $ "got greeting " ++ show greeting
  let prefix = S.pack "select scion-server protocol:"
      quit :: String -> IO ()
      quit msg = do
        CIO.putLine con (S.pack msg)
        logError msg
      handle :: String -> String -> IO ()
      handle "vim" v = runScion $ Vim.handle con v
      handle "emacs" v = runScion $ Emacs.handle con v
      handle name _ = quit $ "unkown protocol type : " ++ name

  if S.isPrefixOf prefix greeting
    then let (a,b) =  S.break (== ' ') (S.drop (S.length prefix) greeting)
         in handle (S.unpack a) (tail $ S.unpack b)
    else quit $ "prefix " ++ (show $ (S.unpack prefix)) ++ " expected, but got : " ++ (S.unpack greeting)

startScionInstance = do
  -- instance logging: we just keep logging to stderr..

  -- We have to use line buffering cause the ghc library occasionally writes to
  -- stdout. To not let that mess up the communication every scion communication line is prefixed by "scion:"
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  mapM_ putStr . map ("scion:" ++ ) =<< instanceHandleRequests . lines =<<  getContents


main = do
  -- logging
  initializeLogging

  -- cmd opts
  (opts, nonOpts, err_msgs) <- fmap (getOpt Permute options) getArgs

  when ((not . null) nonOpts) $ logError $ "no additional arguments expected, got: " ++ (show nonOpts)

  dCfg <- defaultDaemonStartupConfig 
  cfgUnchecked <- foldrM ($) dCfg opts

  -- help
  when (showHelp cfgUnchecked) $ helpText >>= putStrLn >> exitSuccess
  
  if (scionInstance cfgUnchecked)
    then -- scion instance started by daemon
         startScionInstance
    else do
        -- start server
        logInfo "starting server"
        -- E.handle (\(e :: SomeException) ->  "shutting down server due to exception "  ++ show e) $
        do
            log HL.DEBUG $ "opts: " ++ (show cfgUnchecked)
            serve (connectionMode cfgUnchecked)
