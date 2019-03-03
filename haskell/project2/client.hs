#!/usr/bin/env stack
-- stack --resolver lts-6.15 script
{-# LANGUAGE OverloadedStrings #-}

module Client where
 
import Network.Socket
import System.IO
import Control.Monad.Fix (fix)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0    -- create socket
    setSocketOption sock ReuseAddr 1   -- make socket immediately reusable - eases debugging.
    localhost <- inet_addr "127.0.0.1"
    let sktAddr = SockAddrInet 4242 localhost
    connect sock sktAddr

    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    line <- getLine
    hPutStrLn hdl line
    fix $ \loop -> do
               line <- hGetLine hdl
               print line
	       loop
    hClose hdl


