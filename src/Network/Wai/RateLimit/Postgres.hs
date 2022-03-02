-- |
-- Copyright: (c) 2022 Aditya Manthramurthy
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Aditya Manthramurthy <aditya.mmy@gmail.com>
--
-- A wai-rate-limit backend using PostgreSQL.
module Network.Wai.RateLimit.Postgres
  ( PGBackendError (..),
    postgresBackend,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (Handler (..), catches, throwIO, try)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import Network.Wai.RateLimit.Backend (Backend (..))

-- | Represents reasons for why requests made to Postgres backend have failed.
data PGBackendError
  = PGBackendErrorInit PG.SqlError
  | PGBackendErrorBugFmt PG.FormatError
  | PGBackendErrorBugQry PG.QueryError
  | PGBackendErrorBugRes PG.ResultError
  | PGBackendErrorBugSql PG.SqlError
  | PGBackendErrorAtMostOneRow
  | PGBackendErrorExactlyOneRow
  | PGBackendErrorExactlyOneUpdate
  deriving stock (Eq, Show)

instance Exception PGBackendError

initPostgresBackend :: Pool PG.Connection -> Text -> IO ()
initPostgresBackend p tableName = withResource p $ \c -> do
  res <- try $ PG.execute_ c createTableQuery
  either
    (throwIO . PGBackendErrorInit)
    (const $ return ())
    res
  where
    createTableQuery =
      fromString $
        toString $
          T.intercalate
            " "
            [ "CREATE TABLE IF NOT EXISTS",
              tableName,
              "(key BYTEA PRIMARY KEY,",
              "usage INT8 NOT NULL,",
              "expires_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP + '1 week'::INTERVAL)"
            ]

sqlHandlers :: [Handler a]
sqlHandlers =
  [ Handler (throwIO . PGBackendErrorBugFmt),
    Handler (throwIO . PGBackendErrorBugQry),
    Handler (throwIO . PGBackendErrorBugRes),
    Handler (throwIO . PGBackendErrorBugSql)
  ]

pgBackendGetUsage :: Pool PG.Connection -> Text -> ByteString -> IO (Either PGBackendError Integer)
pgBackendGetUsage p tableName key = withResource p $ \c ->
  do
    res <-
      try $
        PG.query c getUsageQuery (PG.Only $ PG.Binary key) `catches` sqlHandlers
    return $ do
      rows <- res
      case rows of
        [] -> Right 0
        [PG.Only a] -> Right a
        _ -> Left PGBackendErrorAtMostOneRow
  where
    getUsageQuery =
      fromString $
        toString $
          T.intercalate
            " "
            [ "SELECT usage FROM",
              tableName,
              "WHERE key = ?",
              "AND expires_at > CURRENT_TIMESTAMP"
            ]

pgBackendIncAndGetUsage :: Pool PG.Connection -> Text -> ByteString -> Integer -> IO (Either PGBackendError Integer)
pgBackendIncAndGetUsage p tableName key usage = withResource p $ \c -> do
  res <- try $ PG.query c incAndGetQuery (PG.Binary key, usage) `catches` sqlHandlers
  return $ do
    rows <- res
    case rows of
      [PG.Only a] -> Right a
      _ -> Left PGBackendErrorExactlyOneRow
  where
    incAndGetQuery =
      fromString $
        toString $
          T.intercalate
            " "
            [ "INSERT INTO",
              tableName,
              "as rl",
              "(key, usage) VALUES (?, ?)",
              "ON CONFLICT (key) DO UPDATE SET",
              "usage = CASE WHEN rl.expires_at > CURRENT_TIMESTAMP THEN rl.usage + EXCLUDED.usage ELSE EXCLUDED.usage END,",
              "expires_at = CASE WHEN rl.expires_at > CURRENT_TIMESTAMP THEN rl.expires_at ELSE CURRENT_TIMESTAMP + '1 week'::INTERVAL END",
              "RETURNING usage"
            ]

pgBackendExpireIn :: Pool PG.Connection -> Text -> ByteString -> Integer -> IO (Either PGBackendError ())
pgBackendExpireIn p tableName key seconds = withResource p $ \c -> do
  res <- try $ PG.execute c expireInQuery (seconds, PG.Binary key) `catches` sqlHandlers
  return $ do
    count <- res
    if count /= 1
      then Left PGBackendErrorExactlyOneUpdate
      else Right ()
  where
    expireInQuery =
      fromString $
        toString $
          T.intercalate
            " "
            [ "UPDATE",
              tableName,
              "SET expires_at = CURRENT_TIMESTAMP + '? second'::interval",
              "WHERE key = ?"
            ]

pgBackendCleanup :: Pool PG.Connection -> Text -> IO ()
pgBackendCleanup p tableName = void $
  forkIO $ do
    forever $ do
      res <- withResource p $ \c -> do
        tryDBErr $ PG.execute_ c removeExpired `catches` sqlHandlers
      case res of
        Left _ -> threadDelay d10s
        Right n -> delay n
  where
    d10s = 10_000_000
    d1s = 1_000_000
    d100ms = 100_000

    -- Try to ensure we cleanup as fast as garbage is created.
    delay n
      | n == 5000 = threadDelay d100ms
      | n > 0 = threadDelay d1s
      | otherwise = threadDelay d10s

    tryDBErr :: IO a -> IO (Either PGBackendError a)
    tryDBErr a = try a

    removeExpired =
      fromString $
        toString $
          T.intercalate
            " "
            [ "DELETE FROM",
              tableName,
              "WHERE key IN (SELECT key FROM",
              tableName,
              "WHERE expires_at < CURRENT_TIMESTAMP LIMIT 5000)"
            ]

-- | Initialize a postgres backend for rate-limiting. Takes a connection pool
-- and table name to use for storage. The table will be created if it does not
-- exist. A thread is also launched to periodically clean up expired rows from
-- the table.
postgresBackend :: Pool PG.Connection -> Text -> IO (Backend ByteString PGBackendError)
postgresBackend p tableName = do
  initPostgresBackend p tableName
  pgBackendCleanup p tableName
  return $
    MkBackend
      { backendGetUsage = pgBackendGetUsage p tableName,
        backendIncAndGetUsage = pgBackendIncAndGetUsage p tableName,
        backendExpireIn = pgBackendExpireIn p tableName
      }
