{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Channels where

import ServerMessages
import ClientMessages
import ServerDB

import Control.Lens
import Control.Concurrent.Chan
import qualified Network.WebSockets as WS

data ClientChan = ClientChan { _inChan :: Chan ServerMessage, _outChan :: Chan UserMessage }

makeLenses ''ClientChan

data Client = Client { _user :: User, _channels :: ClientChan }

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
