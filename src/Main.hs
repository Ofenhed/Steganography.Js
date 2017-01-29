module Main (
    main
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar, modifyMVar, readMVar, newMVar, withMVar)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import Control.Monad (when)
import Data.Word (Word32)
import qualified Data.ByteString.Lazy.Char8 as LBS

import GHCJS.Marshal (fromJSVal)
import GHCJS.DOM (syncPoint, currentDocument, currentWindow)
import GHCJS.DOM.Types
       (HTMLParagraphElement(..), HTMLSpanElement(..), HTMLDivElement(..), HTMLInputElement(..), HTMLImageElement(..), unsafeCastTo, castTo, WindowBase64(..), JSString(..), toJSString, fromJSString, ToJSString(..), FromJSString(..), HTMLButtonElement(..))
import GHCJS.DOM.Document (getBodyUnsafe, createElementUnsafe, createTextNode)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild, getParentElement, setTextContent, removeChild)
import GHCJS.DOM.NodeList (item)
import GHCJS.DOM.EventM (on, mouseClientXY, mouseButton, uiKeyCode, newListenerSync, addListener, releaseListener, removeListener)
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.HTMLImageElement as Image
import qualified GHCJS.DOM.HTMLButtonElement as Button
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.FileList as FileList 
import qualified GHCJS.DOM.File as File
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Binary.Get as Decoder

import qualified Data.Text as T

foreign import javascript unsafe "window.btoa($1)" btoa :: JSString -> JSString
foreign import javascript unsafe "window.atob($1)" atob :: JSString -> JSString
foreign import javascript unsafe "Date.now()" dateNow :: IO (Word32)

base64encode :: (FromJSString a, ToJSString a) => a -> a
base64encode x = fromJSString $ btoa $ toJSString x
 
base64decode :: (FromJSString a, ToJSString a) => a -> a
base64decode x = fromJSString $ atob $ toJSString x
 
fullScreenBody = do
  Just doc <- currentDocument
  body <- createElementUnsafe doc (Just "div") >>= unsafeCastTo HTMLDivElement
  E.setAttribute body "style" "width: 100% ; height: 100%"
  body_ <- getBodyUnsafe doc
  setInnerHTML body_ $ Just ""
  appendChild body_ (Just body)
  return body
  

main = do
  Just doc <- currentDocument
  body <- getBodyUnsafe doc
  setInnerHTML body (Just "")
  E.setAttribute body "style" "width: 100% ; height: 100%"
  Just html <- getParentElement body
  E.setAttribute html "style" "height: 100%"
  prompt "Give me a good word, or mash the keyboard!" createRandom

base64prefix (Just t) = T.concat [T.pack "data:", t, T.pack ";base64,"]
trimBase64prefix Nothing haystack = case T.findIndex ((==) ',') haystack of
                                      Just i -> Just $ T.drop (i + 1) haystack
                                      _ -> Nothing
trimBase64prefix t'@(Just _) haystack = let t = base64prefix t' ; (before, after) = T.splitAt (T.length t) haystack
                                         in if before == t
                                             then Just after
                                             else Nothing

thirdState hashData imageData = do
  Just doc <- currentDocument
  body <- fullScreenBody
  setInnerHTML body $ Just "<h1>Third state</h1>"
  text <- createTextNode doc $ "Hash: " ++ show hashData
  appendChild body text
  image <- createElementUnsafe doc (Just "img") >>= unsafeCastTo HTMLImageElement
  E.setAttribute image "src" $ T.append (base64prefix $ Just $ T.pack "image/png") $ base64encode imageData
  appendChild body (Just image)
  return ()

receiveFile fileType nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  button <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  E.setAttribute button "type" "file"
  when (isJust fileType) $ E.setAttribute button "accept" $ fromJust fileType
  appendChild body (Just button)
  on button E.change $ do
    Just files <- Input.getFiles button
    len <- FileList.getLength files
    files <- mapM (fmap (fromMaybe (Prelude.error "fileInput: fileList.item returned null")) . FileList.item files) [0 .. len-1]
    reader <- FileReader.newFileReader
    liftIO $ on reader FileReader.load $ do
      s <- FileReader.getResult reader
      s' <- liftIO $ fromJSVal s
      let Just fileData = fmap T.pack $ s'
      let decodedData = trimBase64prefix fileType fileData
      when (isNothing fileType || isJust decodedData) $ do liftIO $ nextState $ base64decode $ fromJust decodedData
      return ()
    mapM (\x -> FileReader.readAsDataURL reader (Just x)) files
    --fileReader <- newFileReader
    --show $ getResult $ fileReader
    return ()
  return ()

prompt question nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  text <- createTextNode doc question
  input <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  button <- createElementUnsafe doc (Just "button") >>= unsafeCastTo HTMLButtonElement
  setTextContent button $ Just "OK"
  appendChild body text
  appendChild body $ Just input
  appendChild body $ Just button
  liftIO $ on button E.click $ do
    Just val <- Input.getValue input
    liftIO $ nextState val
  liftIO $ on input E.keyUp $ do
    code <- uiKeyCode
    when (code == 13) $ do
      Just val <- Input.getValue input
      liftIO $ nextState val
      return ()
    return ()
  return ()

createRandom secret = do
  Just doc <- currentDocument
  body <- fullScreenBody
  newParagraph <- createElementUnsafe doc (Just "p") >>= unsafeCastTo HTMLParagraphElement
  button <- createElementUnsafe doc (Just "button") >>= unsafeCastTo HTMLButtonElement
  setTextContent button $ Just "OK"
  Just text <- createTextNode doc $ ""
  Just text2 <- createTextNode doc $ ""
  appendChild newParagraph $ Just text
  appendChild newParagraph $ Just text2
  appendChild body $ Just newParagraph
  appendChild body $ Just button
  shaKey <- newMVar (SHA.sha512 $ LBS.pack secret, 0)

  let addData d = do
        time <- liftIO $ dateNow
        currentData <- liftIO $ modifyMVar shaKey (\(prev, count) -> return ((SHA.sha512 $ LBS.pack $ show (SHA.bytestringDigest prev, d, time), count + 1), (show count) ++ " iterations, last added: " ++ show (SHA.showDigest prev, d, time)))
        setTextContent text2 $ Just currentData

  releaser <- do
    body_ <- getBodyUnsafe doc
    keyup <- newListenerSync $ do
        key <- uiKeyCode
        addData $ show ("up", key)
        return ()
    addListener body_ E.keyUp keyup True
    keydown <- newListenerSync $ do
        key <- uiKeyCode
        addData $ show ("down", key)
        return ()
    addListener body_ E.keyDown keydown True
    return $ do
      removeListener body_ E.keyUp keyup True
      removeListener body_ E.keyDown keydown True
      releaseListener keyup
      releaseListener keydown
      setTextContent text $ Just "Boom"
  liftIO $ on button E.click $ do
    liftIO $ releaser
    key <- liftIO $ withMVar shaKey (\(prev, count) -> return $ SHA.bytestringDigest prev)
    liftIO $ receiveFile (Just $ T.pack "image/png") $ thirdState key
  on body E.mouseMove $ do
      (x, y) <- mouseClientXY
      button <- mouseButton
      addData $ show (x, y, button)
      return ()
  return ()

