{-|
Module:      Network.JobProtocol
Description: Types for interactions between the Job Server and Trebuchet.
Copyright:   Michael Swan, Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

module Network.JobProtocol where

import Control.Applicative

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString as B

-- | A request for a worker to execute a job.
data JobReq = JobReq {
    -- | 'jobID' of the 'Job' we're requesting.
    jobReqID          :: Word64
    -- | 'jobTemplateKey' of the 'JobTemplate' of the 'Job' we're requesting.
    --   This identifies the "job image" to the worker.
  , jobReqTemplateKey :: T.Text
    -- | Binary representation of the job configuration.
  , jobReqConfig      :: B.ByteString
  } deriving (Eq, Ord, Show)

-- | A request for an update on a job's state.
data JobPing = JobPing {
    -- 'jobID' of the 'Job' we're inquiring about.
    jobPingID :: Word64
  } deriving (Eq, Ord, Show)

-- | A request for a worker to abort a job.
data JobAbort = JobAbort {
    -- | 'jobID' of the 'Job' we're aborting.
    jobAbortID :: Word64
  } deriving (Eq, Ord, Show)

-- | A message from a worker indicating an error condition in job execution.
data JobState = JobRunning {
                  -- | 'jobID' of the 'Job' that is running.
                  jobRunningID :: Word64
                }
              | JobSucceeded {
                  -- | 'jobID' of the 'Job' that succeeded.
                  jobSucceededID :: Word64
                }
              | JobFailed {
                  -- | 'jobID' of the 'Job' that died.
                  jobDiedID     :: Word64
                  -- | The job's non-zero return code.
                , jobDiedCode   :: Int
                  -- | The job's error, i.e. stdout.
                , jobDiedStderr :: T.Text
                } deriving (Eq, Ord, Show)

instance Binary JobReq where
    put jr = do
        putWord64le (fromIntegral $ 25 + B.length tk + B.length conf)
        putWord8 0x01
        putWord64le (jobReqID jr)
        putWord64le $ fromIntegral $ B.length tk
        putByteString tk
        putWord64le $ fromIntegral $ B.length conf
        putByteString conf
            where tk   = T.encodeUtf8 $ jobReqTemplateKey jr
                  conf = jobReqConfig jr

    get = do
        skip 8
        0x01     <- getWord8
        jrid     <- getWord64le
        tklen    <- getWord64le
        tk       <- getByteString $ fromIntegral tklen
        conflen  <- getWord64le
        conf     <- getByteString $ fromIntegral conflen
        return $ JobReq jrid (T.decodeUtf8 tk) conf

instance Binary JobPing where
    put (JobPing i) = do
        putWord64le 9
        putWord8 0x02
        putWord64le i

    get = do
        9    <- getWord64le
        0x02 <- getWord64le
        JobPing <$> getWord64le

instance Binary JobAbort where
    put (JobAbort i) = do
        putWord64le 9
        putWord8 0x03
        putWord64le i

    get = do
        9    <- getWord64le
        0x03 <- getWord64le
        JobAbort <$> getWord64le

instance Binary JobState where
    put (JobRunning i) = do
        putWord64le (fromIntegral 9)
        putWord8 0x04
        putWord64le i
    put (JobSucceeded i) = do
        putWord64le (fromIntegral 9)
        putWord8 0x05
        putWord64le i
    put (JobFailed i c e) = do
        putWord64le (fromIntegral $ 25 + B.length err)
        putWord8 0x06
        putWord64le i
        putWord64le $ fromIntegral c
        putWord64le $ fromIntegral $ B.length err
        putByteString err
            where err = T.encodeUtf8 e

    get = skip 8 *> jobRunning <|> jobSucceeded <|> jobFailed
        where jobRunning = do
                0x04   <- getWord8
                JobRunning <$> getWord64le
              jobSucceeded = do
                0x05   <- getWord8
                JobSucceeded <$> getWord64le
              jobFailed = do
                0x06   <- getWord8
                i      <- getWord64le
                c      <- fromIntegral <$> getWord64le
                errlen <- getWord64le
                err    <- getByteString $ fromIntegral errlen
                return $ JobFailed i c (T.decodeUtf8 err)
