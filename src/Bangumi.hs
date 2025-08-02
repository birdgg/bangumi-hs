{-# LANGUAGE OverloadedRecordDot #-}

module Bangumi (
    someFunc,
) where

import BgmTV.Api.Subject
import BgmTV.Client
import BgmTV.Types.Subject
import Network.HTTP.Client qualified as Client
import Network.HTTP.Client.TLS qualified as TLS
import Servant.Client (runClientM)
import Text.Pretty.Simple

someFunc :: IO ()
someFunc = do
    manager <- Client.newManager TLS.tlsManagerSettings
    let env = mkBgmClientEnv manager
    let action = searchSubject (mkSubjectQuery "怪兽8号" Anime)
    res <- runClientM action env
    case res of
        Right d -> pPrint d.pData
        Left e -> pPrint e
