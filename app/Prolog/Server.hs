{-# LANGUAGE OverloadedStrings #-}
module Prolog.Server (Server, start, call) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text, unpack)
import System.IO
import System.Process

data Server = Server Handle Handle

start :: IO Server
start = do
  (Just input, Just output, _, _) <- createProcess
    (proc "swipl" ["prolog/server/main.pl"])
      { std_in = CreatePipe, std_out = CreatePipe, std_err = NoStream }
  hSetBuffering input LineBuffering
  pure (Server input output)

call :: FromJSON a => Server -> Value -> IO (Either String a)
call (Server input output) request = do
  B.hPutStrLn input (BL.toStrict (encode request))
  hFlush input
  reply <- B.hGetLine output
  pure (eitherDecodeStrict reply >>= fromReply)

fromReply :: FromJSON a => Value -> Either String a
fromReply value = case parseEither (withObject "reply" (.: "error")) value of
  Right message -> Left (unpack (message :: Text))
  Left _        -> parseEither parseJSON value
