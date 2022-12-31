{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Spock
import Web.Spock.Config
import Network.Wai.Middleware.Static

import Web.Spock.Lucid (lucid)
import Lucid

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import qualified PixelImage as Pixel
import Transformation


data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

-- main :: IO()
-- main = do
--     Pixel.test "./test-images/logs.jpg"

main :: IO ()
main = do 
    foo <- Pixel.test
    ref <- newIORef 0
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
    runSpock 8086 (spock spockCfg app)

app :: SpockCtxM () () MySession MyAppState ()
app = do
    middleware (staticPolicy (addBase "static"))
    get root $ lucid $ do
        head_ $ do
            link_ [rel_ "stylesheet", href_ "/css/style.css"]
        body_ $ do
            h1_ "Test."
            p_ "Test <p>."
            form_ [method_ "post"] $ do
                label_ $ do
                    "Select image: "
                    input_ [type_ "file", name_ "image"]
                input_ [type_ "submit"]
    post root $ do
        --image <- param "image"
        redirect "/"
        
