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
import qualified System.Log.Handler.Syslog as HL
import qualified Data.ByteString.Char8 as S
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.List (isPrefixOf, break)
import Data.Foldable (foldrM)
import Data.Map as M
import qualified Control.Exception as E


import Control.Monad ( when, forever )

import System.Console.GetOpt

import MonadUtils ( liftIO )
import qualified Scion.Server.ProtocolEmacs as Emacs
import qualified Scion.Server.ProtocolVim as Vim
import qualified Scion.Server.ConnectionIO as CIO
import Scion (runScion)

log = HL.logM __FILE__
logInfo = log HL.INFO
logDebug = log HL.DEBUG
logError = log HL.ERROR

-- how should the client connect to the server?
-- if you're paranoid about your code Socketfile or StdInOut
-- will be the most secure choice.. (Everyone can connect via TCP/IP at the
-- moment)
data ConnectionMode = TCPIP PortNumber
                  | StdInOut
#ifndef mingw32_HOST_OS
                  | Socketfile FilePath
#endif
  deriving Show

data StartupConfig = StartupConfig {
     connectionMode :: ConnectionMode,
     showHelp :: Bool
  } deriving Show
defaultStartupConfig = StartupConfig ( TCPIP (fromInteger 4005)) False

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
     , Option ['h'] ["help"] (NoArg (\opts -> return $ opts { showHelp = True } )) "show this help"

     , Option ['f'] ["log-file"] (ReqArg (\f opts -> do
          fh <- HL.fileHandler f HL.DEBUG
          HL.updateGlobalLogger "" (HL.addHandler fh)
          return opts ) "/tmp/scion-log") "log to the given file"
     ]

initializeLogging = do
  -- by default log everything to stdout
  HL.updateGlobalLogger "" (HL.setLevel HL.DEBUG)

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

main = do

  -- logging
  initializeLogging

  -- cmd opts
  (opts, nonOpts, err_msgs) <- fmap (getOpt Permute options) getArgs

  when ((not . null) nonOpts) $ logError $ "no additional arguments expected, got: " ++ (show nonOpts)

  startupConfig <- foldrM ($) defaultStartupConfig opts

  -- help
  when (showHelp startupConfig) $ helpText >>= putStrLn >> exitSuccess

  -- start server
  logInfo "starting server"
  -- E.handle (\(e :: SomeException) ->  "shutting down server due to exception "  ++ show e) $
  do
      log HL.DEBUG $ "opts: " ++ (show startupConfig)
      serve (connectionMode startupConfig)



