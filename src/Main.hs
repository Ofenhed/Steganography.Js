module Main where

import Data.Maybe (fromJust)
import Control.Monad (when)

import PngCanvasContainer

import Steganography (doEncrypt, doDecrypt)
import DummyContainer (DummyContainer(..))
import EccKeys (generateKeyPair, encodeSecretKey, encodePublicKey)

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
import PngCanvasContainer (CanvasPngImageType(..))
import Random

foreign import javascript unsafe "window.performance.now ? window.performance.now() : Date.now()" timeEntropy :: IO (Double)
foreign import javascript unsafe "alert($1)" alert :: JSString -> IO ()

main = do
  setTitle "Steganography applet"
  Just doc <- currentDocument
  body <- getBodyUnsafe doc
  setInnerHTML body (Just "")
  --E.setAttribute body "style" "width: 100% ; height: 100%"
  Just html <- getParentElement body
  createRandomState $ mainMenu

mainMenu = do
  Just doc <- currentDocument
  body <- fullScreenBody
  showMenu body [(T.pack "Encrypt", encrypter),
                 (T.pack "Decrypt", decrypter),
                 (T.pack "Generate keys", generateKeys)]

t2lb = LC8.pack . T.unpack
mt2lb (Just x) = t2lb x
mt2lb Nothing = LC8.empty
mt2b (Just x) = C8.pack $ T.unpack x
mt2b Nothing = C8.empty

encrypter :: IO ()
encrypter = do
  setTitle "Encrypt and hide data"
  receiveFile (FileUploadOptions Nothing Nothing (Just $ T.pack "Upload the shared key")) $ \(Just keyData) ->
    receiveFile (FileUploadOptions Nothing (Just (T.pack "image/png", True)) (Just $ T.pack "Upload file to hide data in")) $ \(Just imageData) ->
      receiveFile (FileUploadOptions (Just $ T.pack "Continue without extra encryption") (Just (T.pack ".pstk", False)) (Just $ T.pack "Upload the targets public key for extra encryption")) $ \maybeTargetKey ->
        receiveFile (FileUploadOptions (Just $ T.pack "Continue without signing") (Just (T.pack ".sstk", False)) (Just $ T.pack "Upload your private key to sign the result")) $ \maybeSignKey ->
          receiveFile (FileUploadOptions Nothing Nothing (Just $ T.pack "Upload the file you want to hide")) $ \(Just hiddenData) ->
            prompt "Add a public text which will be used as salt" $ \salt -> do
              Just doc <- currentDocument
              body <- fullScreenBody
              setTextContent body $ Just $ T.pack "Working"
              encrytedData <- doEncrypt (t2lb imageData) CanvasPngImageSpawner (t2lb keyData) 5 (t2lb hiddenData) (t2lb salt) (mt2lb maybeTargetKey) (mt2b maybeSignKey)
              case encrytedData of
                Right result -> appendDownloadLink body (T.pack "Download") (T.pack "image.png") $ LinkDataRaw $ T.pack $ LC8.unpack result
                Left error -> setTextContent body $ Just $ T.pack error
              return ()
  return ()
--
--  Right returnedData <- doEncrypt (LC8.pack $ T.unpack imageData) DummyContainer (LC8.pack "nyckel") 10 (LC8.pack "Min hemliga data") (LC8.pack "salt") LC8.empty C8.empty

decrypter :: IO ()
decrypter = do
  setTitle "Decrypt hidden data"
  receiveFile (FileUploadOptions Nothing Nothing (Just $ T.pack "Upload the shared key")) $ \(Just keyData) ->
    receiveFile (FileUploadOptions Nothing (Just (T.pack "image/png", True)) (Just $ T.pack "Upload file to find data in")) $ \(Just imageData) ->
      receiveFile (FileUploadOptions (Just $ T.pack "The file is not encrypted against my public key") (Just (T.pack ".sstk", False)) (Just $ T.pack "Upload your private key if the file is encrypted against your public key")) $ \maybePrivateKey ->
        receiveFile (FileUploadOptions (Just $ T.pack "The file is not signed") (Just (T.pack ".pstk", False)) (Just $ T.pack "Upload the senders public key to verify that the file is correctly signed")) $ \maybeSignedKey ->
          prompt "Input the salt that was used" $ \salt -> do
            Just doc <- currentDocument
            body <- fullScreenBody
            setTextContent body $ Just $ T.pack "Working"
            decryptedData <- doDecrypt (t2lb imageData) CanvasPngImageSpawner (t2lb keyData) 5 (t2lb salt) (mt2b maybePrivateKey) (mt2lb maybeSignedKey)
            case decryptedData of
              Right result -> appendDownloadLink body (T.pack "Download") (T.pack "") $ LinkData $ T.pack $ LC8.unpack result
              Left error -> setTextContent body $ Just $ T.pack error
            return ()
  return ()

generateKeys = do
  setTitle "Generate keys"
  body <- fullScreenBody
  appendText body $ T.pack "This action requires that you have a good source of random data. Would you like to add more random data to your entropy pool?"
  appendLineBreak body
  showMenu body [(T.pack "Yes", createRandomState generateKeys),
                 (T.pack "No", do
                   (secretKey, publicKey) <- generateKeyPair
                   newBody <- fullScreenBody
                   appendText newBody $ T.pack "Download these files. The secret file is yours and only yours and should NEVER be shared. The public key is the key you send to people you which to communicate with."
                   appendLineBreak newBody
                   appendDownloadLink newBody (T.pack "Secret key") (T.pack "steganography_secret.sstk") $ LinkData $ T.pack $ C8.unpack $ encodeSecretKey secretKey
                   appendLineBreak newBody
                   appendDownloadLink newBody (T.pack "Public key") (T.pack "steganography_public.pstk") $ LinkData $ T.pack $ C8.unpack $ encodePublicKey publicKey
                   return ())]
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

