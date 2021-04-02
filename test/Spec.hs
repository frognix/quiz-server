import Test.Hspec
import UserTests
import AdminTests

import Extra.Tools
import Server
import ServerDB
import Control.Concurrent.Async (cancel)

main :: IO ()
main = do
  deleteDB
  initDB
  server <- linkAsync runServer
  fillTables
  testUserAPI
  testAdminAPI
  cancel server
