{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Channels where

import ClientMessages
import ServerMessages


import Control.Lens
import Control.Concurrent.Chan
import qualified Network.WebSockets as WS
import Database.Schema (User)

data ClientChan = ClientChan { inChan :: Chan ServerMessage, outChan :: Chan UserMessage } deriving Eq

data Client = Client { _user :: User, _channels :: ClientChan } deriving Eq

makeLenses ''Client

type LobbyManagerChan = Chan Client

data Connection = ConnectMsg ClientChan
                | DisconnectMsg Client

type AuthenticationChan = Chan Connection
type AdminChan = Chan WS.Connection

data ServerChans = ServerChans {
  _authChan :: AuthenticationChan,
  _lobbyChan :: LobbyManagerChan
}

makeLenses ''ServerChans
