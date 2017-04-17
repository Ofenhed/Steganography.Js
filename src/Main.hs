module Main where

import Data.Maybe (fromJust)
import Control.Monad (when)

import Steganography (doEncrypt, doDecrypt)

import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (HTMLImageElement(..), unsafeCastTo, JSString(..), toJSString, HTMLElement(..))
import GHCJS.DOM.Document (getBodyUnsafe, getHeadUnsafe, createTextNode, createElement)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild, getParentElement, setTextContent)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8
import qualified GHCJS.DOM.HTMLElement as HTML

import WebPbkdf2 (hmacSha512Pbkdf2)

import qualified Data.Text as T
import qualified Data.JSString.Text as T

import HelperFunctions
import Random

foreign import javascript unsafe "window.performance.now ? window.performance.now() : Date.now()" timeEntropy :: IO (Double)
foreign import javascript unsafe "(window.crypto && window.crypto.getRandomValues) ? function() { var buf = new ArrayBuffer(8); var ret = new Float64Array(buf); var rnd = new Uint8Array(buf); window.crypto.getRandomValues(rnd); return ret[0]; }() : Math.random()" randomEntropy :: IO (Double)
foreign import javascript unsafe "alert($1)" alert :: JSString -> IO ()

main = do
  setTitle "Steganography applet"
  Just doc <- currentDocument
  body <- getBodyUnsafe doc
  setInnerHTML body (Just "")
  --E.setAttribute body "style" "width: 100% ; height: 100%"
  Just html <- getParentElement body
  createRandomState $ showMenu [(T.pack "Encrypt", encrypter),
                                (T.pack "Decrypt", decrypter)]

encrypter :: IO ()
encrypter = do
  setTitle "Encrypt and hide data"
  receiveFile (FileUploadOptions Nothing Nothing (Just $ T.pack "Upload the shared key")) $ \(Just keyData) ->
    receiveFile (FileUploadOptions Nothing (Just $ T.pack "image/png") (Just $ T.pack "Upload file to hide data in")) $ \(Just imageData) ->
      receiveFile (FileUploadOptions (Just $ T.pack "Continue without extra encryption") Nothing (Just $ T.pack "Upload the targets public key for extra encryption")) $ \maybeTargetKey ->
        receiveFile (FileUploadOptions (Just $ T.pack "Continue without signing") Nothing (Just $ T.pack "Upload your private key to sign the result")) $ \maybeSignKey -> return ()
  return ()

decrypter :: IO ()
decrypter = do
  setTitle "Decrypt hidden data"
  receiveFile (FileUploadOptions Nothing Nothing (Just $ T.pack "Upload the shared key")) $ \(Just keyData) ->
    receiveFile (FileUploadOptions Nothing (Just $ T.pack "image/png") (Just $ T.pack "Upload file to find data in")) $ \(Just imageData) ->
      receiveFile (FileUploadOptions (Just $ T.pack "The file is not encrypted against my public key") Nothing (Just $ T.pack "Upload your private key if the file is encrypted against your public key")) $ \maybePrivateKey ->
        receiveFile (FileUploadOptions (Just $ T.pack "The file is not signed") Nothing (Just $ T.pack "Upload the senders public key to verify that the file is correctly signed")) $ \maybeSignedKey -> return ()
  return ()

--doEncrypt imageFile secretFile loops inputData salt pkiFile signFile fastMode = do
--thirdState hashData (Just imageData) = do
--  Just doc <- currentDocument
--  body <- fullScreenBody
--  setInnerHTML body $ Just "<h1>Third state</h1>"
--  text <- createTextNode doc $ "Hash: " ++ show hashData
--  text2 <- createTextNode doc $ ""
--  appendChild body text
--  appendChild body text2
--  image <- createElement doc "img" >>= unsafeCastTo HTMLImageElement
--  E.setAttribute image "src" $ T.append (base64prefix $ Just $ T.pack "image/png") $ base64encode imageData
--  Right returnedData <- doEncrypt (LC8.pack $ T.unpack imageData) DummyContainer (LC8.pack "nyckel") 10 (LC8.pack "Min hemliga data") (LC8.pack "salt") LC8.empty C8.empty
--  --returnedData <- doEncrypt (LC8.pack $ T.unpack imageData) PngImageSpawner (LC8.pack "nyckel") 10 (LC8.pack "Min hemliga data") (LC8.pack "salt") LC8.empty C8.empty
--  setTextContent text2 $ Just $ toJSString $ LC8.unpack returnedData
--  appendChild body image
--  --case returnedData of
--  --  Left msg -> setTextContent (fromJust text) $ Just msg
--  --  Right hidden -> setTextContent (fromJust text) $ Just $ LC8.unpack hidden
--  return ()

