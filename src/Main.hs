module Main where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar, modifyMVar, readMVar, newMVar, withMVar)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Control.Monad (when, forever, liftM)
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.Trans.Class (lift)
import Data.Word (Word32)
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.ByteString.Char8 as C8

import Steganography (doEncrypt, doDecrypt)
--import Png.PngContainer (PngImageType(..))
import DummyContainer (DummyContainer(..))

import GHCJS.Marshal (fromJSVal)
import GHCJS.DOM (syncPoint, currentDocument, currentWindow)
import GHCJS.DOM.Types
       (HTMLParagraphElement(..), HTMLSpanElement(..), HTMLDivElement(..), HTMLInputElement(..), HTMLImageElement(..), unsafeCastTo, castTo, JSString(..), toJSString, fromJSString, ToJSString(..), FromJSString(..), HTMLButtonElement(..), unStringOrArrayBuffer, toJSVal, fromJSVal, JSVal(..), TouchList(..), unTouchList, objectToString)
import GHCJS.DOM.Document (getBodyUnsafe, createTextNode, createElement)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild, getParentElement, setTextContent, removeChild)
import GHCJS.DOM.NodeList (item)
import GHCJS.DOM.EventM (on, mouseClientXY, mouseButton, uiKeyCode, newListenerSync, addListener, releaseListener, removeListener, preventDefault, event)
import GHCJS.DOM.TouchEvent (getTouches)
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLImageElement as Image
import qualified GHCJS.DOM.HTMLButtonElement as Button
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.HTMLElement as E
import qualified GHCJS.DOM.GlobalEventHandlers as G
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.File as File
import qualified Data.Digest.Pure.SHA as SHA
import qualified Crypto.Hash as Hash
import qualified Crypto.Hash.Algorithms as Hash
import qualified Data.Binary.Get as Decoder
import qualified Data.BitString as BiS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8

import Crypto.RandomMonad (runRndT, getRandomM, RndStateList(RndStateListSequencial))

import WebPbkdf2 (hmacSha512Pbkdf2)

import qualified Data.Text as T
import qualified Data.JSString.Text as T
foreign import javascript unsafe "window.btoa($1)" btoa :: JSString -> JSString
foreign import javascript unsafe "window.atob($1)" atob :: JSString -> JSString
foreign import javascript unsafe "window.performance.now ? window.performance.now() : Date.now()" timeEntropy :: IO (Double)
foreign import javascript unsafe "(window.crypto && window.crypto.getRandomValues) ? function() { var buf = new ArrayBuffer(8); var ret = new Float64Array(buf); var rnd = new Uint8Array(buf); window.crypto.getRandomValues(rnd); return ret[0]; }() : Math.random()" randomEntropy :: IO (Double)
foreign import javascript unsafe "alert($1)" alert :: JSString -> IO ()
foreign import javascript unsafe "MyRandom.addSalt($1)" improveRandom :: JSString -> IO ()
foreign import javascript unsafe "MyRandom.getRandom($1)" getRandom :: Int -> IO (JSString)
foreign import javascript unsafe "function(list) { var ret = \"\" ; for (var i = 0; i < list.length; ++i) { var item = list.item(i); ret += item.identifier + \":\" + item.screenX + \",\" + item.screenY + \";\" + item.force + \".\"; } return ret; }($1)" printTouchList :: TouchList -> IO (JSString)

foreign export javascript testsomething :: IO Int
testsomething = return 1337

base64encode :: (FromJSString a, ToJSString a) => a -> a
base64encode x = fromJSString $ btoa $ toJSString x

base64decode :: (FromJSString a, ToJSString a) => a -> a
base64decode x = fromJSString $ atob $ toJSString x

fullScreenBody = do
  Just doc <- currentDocument
  body <- createElement doc "div"
  E.setAttribute body "style" "width: 100% ; height: 100%"
  body_ <- getBodyUnsafe doc
  setInnerHTML body_ $ Just ""
  appendChild body_ body
  return body


main = do
  Just doc <- currentDocument
  body <- getBodyUnsafe doc
  setInnerHTML body (Just "")
  E.setAttribute body "style" "width: 100% ; height: 100%"
  Just html <- getParentElement body
  E.setAttribute html "style" "height: 100%"
  createRandom

testSha text = do
  let hash = Hash.hash $ C8.pack text :: Hash.Digest Hash.SHA1
  let hash2 = Hash.hash $ C8.replicate 1000000 'a' :: Hash.Digest Hash.SHA1
  alert $ toJSString $ "Your hash: " ++ show hash
  alert $ toJSString $ "Big hash: " ++ show hash2

showRandomData = do
  Just doc <- currentDocument
  --body <- fullScreenBody
  --text <- createTextNode doc "Whatevs"
  --input <- createElement doc (Just "input") >>= unsafeCastTo HTMLInputElement
  --button <- createElement doc (Just "button") >>= unsafeCastTo HTMLButtonElement
  --setTextContent button $ Just "OK"
  --appendChild body text
  --appendChild body $ Just input
  --appendChild body $ Just button
  --runRndT [(BiS.bitStringLazy $ hmacSha512Pbkdf2 (LC8.pack "hej") (LC8.pack "hej igen") 100)] $ do
    --lift $ on button E.click $ do
    --  setTextContent (fromJust text) $ Just $ show random
    --  return ()
  syncPoint
  return ()

base64prefix (Just t) = T.concat [T.pack "data:", t, T.pack ";base64,"]
trimBase64prefix Nothing haystack = case T.findIndex ((==) ',') haystack of
                                      Just i -> Just $ T.drop (i + 1) haystack
                                      _ -> Nothing
trimBase64prefix t'@(Just _) haystack = let t = base64prefix t' ; (before, after) = T.splitAt (T.length t) haystack
                                         in if before == t
                                             then Just after
                                             else Nothing

--doEncrypt imageFile secretFile loops inputData salt pkiFile signFile fastMode = do
thirdState hashData imageData = do
  Just doc <- currentDocument
  body <- fullScreenBody
  setInnerHTML body $ Just "<h1>Third state</h1>"
  text <- createTextNode doc $ "Hash: " ++ show hashData
  text2 <- createTextNode doc $ ""
  appendChild body text
  appendChild body text2
  image <- createElement doc "img" >>= unsafeCastTo HTMLImageElement
  E.setAttribute image "src" $ T.append (base64prefix $ Just $ T.pack "image/png") $ base64encode imageData
  Right returnedData <- doEncrypt (LC8.pack $ T.unpack imageData) DummyContainer (LC8.pack "nyckel") 10 (LC8.pack "Min hemliga data") (LC8.pack "salt") LC8.empty C8.empty
  --returnedData <- doEncrypt (LC8.pack $ T.unpack imageData) PngImageSpawner (LC8.pack "nyckel") 10 (LC8.pack "Min hemliga data") (LC8.pack "salt") LC8.empty C8.empty
  setTextContent text2 $ Just $ toJSString $ LC8.unpack returnedData
  appendChild body image
  --case returnedData of
  --  Left msg -> setTextContent (fromJust text) $ Just msg
  --  Right hidden -> setTextContent (fromJust text) $ Just $ LC8.unpack hidden
  return ()

receiveFile fileType nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  button <- createElement doc "input" >>= unsafeCastTo HTMLInputElement
  E.setAttribute button "type" "file"
  when (isJust fileType) $ E.setAttribute button "accept" $ fromJust fileType
  appendChild body button
  on button G.change $ do
    Just files <- Input.getFiles button
    len <- FileList.getLength files
    files <- mapM (fmap (fromMaybe (Prelude.error "fileInput: fileList.item returned null")) . FileList.item files) [0 .. len-1]
    reader <- FileReader.newFileReader
    liftIO $ on reader FileReader.load $ do
      Just s <- FileReader.getResult reader
      s' <- lift $ toJSVal s
      let fileData = T.textFromJSVal s'
      let decodedData = trimBase64prefix fileType fileData
      when (isNothing fileType || isJust decodedData) $ do liftIO $ nextState $ base64decode $ fromJust decodedData
      return ()
    mapM (\x -> FileReader.readAsDataURL reader (Just x)) files
    --fileReader <- newFileReader
    --show $ getResult $ fileReader
    return ()
  syncPoint
  return ()

prompt question nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  text <- createTextNode doc question
  input <- createElement doc "input" >>= unsafeCastTo HTMLInputElement
  button <- createElement doc "button" >>= unsafeCastTo HTMLButtonElement
  setTextContent button $ Just "OK"
  appendChild body text
  appendChild body input
  appendChild body button
  on button G.click $ do
    Just val <- Input.getValue input
    liftIO $ nextState val
  on input G.keyUp $ do
    code <- uiKeyCode
    when (code == 13) $ do
      Just val <- Input.getValue input
      liftIO $ nextState val
      return ()
    return ()
  return ()

createRandom = do
  Just doc <- currentDocument
  body <- fullScreenBody
  newParagraph <- createElement doc "p" >>= unsafeCastTo HTMLParagraphElement
  button <- createElement doc "button" >>= unsafeCastTo HTMLButtonElement
  E.setAttribute button "disabled" "True"
  setTextContent button $ Just "OK"
  text <- createTextNode doc "Before this program can start we need to initialize our random number generator. Since we're handling potentially extremely sensitive data, we do not fully trust the random data the browser provides. To enhance the random data from the browser, we need some random data from you. Moving the mouse around and pressing buttons will add to the random data pool. In each iteration, mouse pointer position, time and a (according to the browser) cryptographically secure random value will be included. Once 256 iterations has been added you will be allowed to continue."
  text2 <- createTextNode doc ""
  appendChild newParagraph $ text
  appendChild newParagraph $ text2
  appendChild body $ newParagraph
  appendChild body $ button
  iterCount <- newMVar 0
  let addData d = do
        liftIO $ improveRandom $ toJSString d
        current <- liftIO $ modifyMVar iterCount (\prev -> return (1 + prev, 1 + prev))
        when (current == 256) $ E.removeAttribute button "disabled"
        setTextContent text2 $ Just ("Current iteration count: " ++ show current ++ " (added " ++ d ++ ")")

  releaser <- do
    body_ <- getBodyUnsafe doc
    keyup <- newListenerSync $ do
        preventDefault
        key <- uiKeyCode
        addData $ show ("kup", key)
        return ()
    addListener body_ G.keyUp keyup True
    keydown <- newListenerSync $ do
        preventDefault
        key <- uiKeyCode
        addData $ show ("kdown", key)
        return ()
    addListener body_ G.keyDown keydown True
    touchup <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ printTouchList touch
        addData $ show ("tup", touch')
        return ()
    addListener body_ G.touchEnd touchup False
    touchdown <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ printTouchList touch
        addData $ show ("tdown", touch')
        return ()
    addListener body_ G.touchStart touchdown False
    touchcancel <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ printTouchList touch
        addData $ show ("tcancel", touch')
        return ()
    addListener body_ G.touchCancel touchcancel False
    touchmove <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ printTouchList touch
        addData $ show ("tmove", touch')
        return ()
    addListener body_ G.touchMove touchmove False
    return $ do
      removeListener body_ G.keyUp keyup True
      removeListener body_ G.keyDown keydown True
      removeListener body_ G.touchStart touchdown True
      removeListener body_ G.touchEnd touchup True
      removeListener body_ G.touchCancel touchcancel True
      removeListener body_ G.touchMove touchmove True
      releaseListener keyup
      releaseListener keydown
      releaseListener touchdown
      releaseListener touchup
      releaseListener touchcancel
      releaseListener touchmove
      setTextContent text $ Just "Boom"
      syncPoint
  liftIO $ on button G.click $ do
    liftIO $ releaser
    key <- liftIO $ getRandom 20
    liftIO $ receiveFile (Just $ T.pack "image/png") $ thirdState key
  on doc G.mouseMove $ do
      (x, y) <- mouseClientXY
      button <- mouseButton
      addData $ show (x, y, button)
      return ()
  return ()

