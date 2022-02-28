module Main (main) where

import Control.Concurrent
import Control.Exception
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import Network.HTTP.Client (Manager, defaultManagerSettings, httpLbs, newManager, parseRequest, responseStatus)
import Network.HTTP.Types (status200, statusCode)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.RateLimit as R
import Network.Wai.RateLimit.Postgres
import qualified Network.Wai.RateLimit.Strategy as R
import System.Environment.Blank
import System.IO.Error
import Test.Tasty
import Test.Tasty.HUnit

-- | Example value = "postgres://postgres:postgres@localhost:5432/postgres"
testPGEnv :: String
testPGEnv = "PG_DB_URI"

mkConnPool :: IO (Pool PG.Connection)
mkConnPool = do
  connURI <- getEnv testPGEnv >>= maybe (throwIO $ userError "PG_DB_URI not set") return
  connectInfo <- maybe (throwIO $ userError "invalid uri") return $ parseDatabaseUrl connURI
  createPool
    (PG.connect connectInfo)
    PG.close
    1
    5
    10

-- | The 'key' argument specified the PGBackend key to be used for every
-- request made to this test app.
mkTestWaiApp :: Integer -> Integer -> ByteString -> IO Wai.Application
mkTestWaiApp seconds limit key = do
  pool <- mkConnPool
  pgBackend <- postgresBackend pool "rate_limiter_1"
  let app _ respond = respond $ Wai.responseLBS status200 [] "Ok!"
      strategy = R.fixedWindow pgBackend seconds limit (const $ return key)
      middleware = R.rateLimiting strategy
  return $ middleware app

mkReq :: Manager -> IO Int
mkReq manager = do
  req <- parseRequest "http://localhost:11222"
  resp <- httpLbs req manager
  return $ statusCode $ responseStatus resp

launchApp :: Wai.Application -> IO ThreadId
launchApp app = forkIO $ Warp.run 11222 app

tearDownApp :: ThreadId -> IO ()
tearDownApp = killThread

main :: IO ()
main =
  defaultMain $
    testCaseSteps
      "Rate Limiting Tests"
      $ \step ->
        do
          step "Limit excessive requests (1)"

          bracket
            (mkTestWaiApp 1 2 "key1" >>= launchApp)
            tearDownApp
            $ \_ -> do
              mgr <- newManager defaultManagerSettings
              rs <- replicateM 3 $ mkReq mgr
              when (rs /= [200, 200, 429]) $ do
                assertFailure "Not ratelimited!"

          step "Limit excessive requests (2)"

          bracket
            (mkTestWaiApp 1 3 "key2" >>= launchApp)
            tearDownApp
            $ \_ -> do
              mgr <- newManager defaultManagerSettings
              rs <- replicateM 10 $ mkReq mgr
              when (rs /= replicate 3 200 ++ replicate 7 429) $
                assertFailure "Unexpected result!"

          step "Allow non-excessive requests"

          bracket
            (mkTestWaiApp 1 3 "key3" >>= launchApp)
            tearDownApp
            $ \_ -> do
              mgr <- newManager defaultManagerSettings
              rs <- replicateM 3 $ mkReq mgr
              when (rs /= [200, 200, 200]) $
                assertFailure "Unexpected result!"
