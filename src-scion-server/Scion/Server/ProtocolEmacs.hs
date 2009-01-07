-- |
-- Module      : Scion.Server.ProtocolEmacs
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : experimental
-- Portability : portable
--
-- talk to emacs

module Scion.Server.ProtocolEmacs where
import Scion.Server.ConnectionIO
import Scion.Types (ScionM)

handle :: (ConnectionIO con) => con -> String -> ScionM ()
handle con "0" = fail "TODO: emacs protocol not yet implemented, run the scion_emacs executable please!"
handle con _ = fail "unkown vim protocol version"
