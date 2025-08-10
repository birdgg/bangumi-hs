module Moe.Middlewares (addMiddlewares) where

import Moe.Config
import Network.HTTP.Types.Status
import Network.Wai
import RIO

logStartRequest :: Request -> RIO LogFunc ()
logStartRequest req =
    logDebug $
        mconcat
            [ "REQUEST - "
            , displayBytesUtf8 $ requestMethod req
            , " "
            , displayBytesUtf8 $ fromMaybe "" (requestHeaderHost req)
            , displayBytesUtf8 $ rawPathInfo req
            , displayBytesUtf8 $ rawQueryString req
            ]

logEndRequest :: Response -> RIO LogFunc ()
logEndRequest res =
    logDebug $
        mconcat
            [ "RESPONSE - STATUS "
            , displayShow $ statusCode status
            , " "
            , displayBytesUtf8 $ statusMessage status
            ]
  where
    status = responseStatus res

loggingMiddleware :: LogFunc -> Middleware
loggingMiddleware logFunc app req sendRes = do
    runRIO logFunc (logStartRequest req)
    app req \res -> do
        runRIO logFunc (logEndRequest res)
        sendRes res

middlewares :: Config -> [Middleware]
middlewares config = case config.env of
    Development -> defaultMdlws <> [loggingMiddleware logFunc]
    Production -> defaultMdlws
  where
    logFunc :: LogFunc
    logFunc = fst config.logFunc

    defaultMdlws :: [Middleware]
    defaultMdlws = []

addMiddlewares :: Config -> Application -> Application
addMiddlewares env app = foldr ($) app (middlewares env)
