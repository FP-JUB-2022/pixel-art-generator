{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Spock
import Web.Spock.Config

import Web.Spock.Lucid (lucid)
import Lucid

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import PixelImage as Pixel

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- main :: IO()
-- main = do
--     Pixel.test "./test-images/logs.jpg"

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8081 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = get root $ lucid $ do
    h1_ "Test."
    p_ "Test <p>."
    input_ [type_ "file"]