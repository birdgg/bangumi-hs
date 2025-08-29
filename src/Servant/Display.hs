{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Display where

import Data.Text.Display
import Servant.Client

instance Display ClientError where
  displayBuilder (FailureResponse _ _) = "FailureResponse"
  displayBuilder (ConnectionError exception) = "ConnectionError:" <> displayBuilder exception
  displayBuilder (DecodeFailure reason _) = "DecodeFailure:" <> displayBuilder reason
  displayBuilder (UnsupportedContentType mediaType _) = "UnsupportedContentType:" <> displayBuilder (show mediaType)
  displayBuilder (InvalidContentTypeHeader _) = "InvalidContentTypeHeader"
