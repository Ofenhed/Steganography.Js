module Random (createRandomState, getRandom, improveRandom) where

import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.EventM (on, mouseClientXY, mouseButton, uiKeyCode, newListenerSync, addListener, releaseListener, removeListener, preventDefault, event)
import GHCJS.DOM.Node (appendChild, setTextContent)
import GHCJS.DOM.TouchEvent (getTouches)
import GHCJS.DOM.Types (HTMLParagraphElement(..), unsafeCastTo, toJSString, HTMLButtonElement(..), JSString, TouchList(..))

import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.GlobalEventHandlers as G
import qualified GHCJS.DOM.HTMLElement as E

import HelperFunctions (fullScreenBody, appendLineBreak)

foreign import javascript unsafe "MyRandom.addSalt($1)" improveRandom :: JSString -> IO ()
foreign import javascript unsafe "MyRandom.getRandom($1)" getRandom :: Int -> IO (JSString)
foreign import javascript unsafe "function(list) { var ret = \"\" ; for (var i = 0; i < list.length; ++i) { var item = list.item(i); ret += item.identifier + \":\" + item.screenX + \",\" + item.screenY + \";\" + item.force + \".\"; } return ret; }($1)" showTouchList :: TouchList -> IO (JSString)

createRandomState nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  newParagraph <- D.createElement doc "p" >>= unsafeCastTo HTMLParagraphElement
  button <- D.createElement doc "button" >>= unsafeCastTo HTMLButtonElement
  E.setAttribute button "disabled" "True"
  setTextContent button $ Just "OK"
  text <- D.createTextNode doc "Before this program can start we need to initialize our random number generator. Since we're handling potentially extremely sensitive data, we do not fully trust the random data the browser provides. To enhance the random data from the browser, we need some random data from you. Moving the mouse around and pressing buttons will add to the random data pool. In each iteration, mouse pointer position, time and a (according to the browser) cryptographically secure random value will be included. Once 256 iterations has been added you will be allowed to continue."
  text2 <- D.createTextNode doc ""
  appendChild newParagraph $ text
  appendLineBreak newParagraph
  appendChild newParagraph $ text2
  appendChild body $ newParagraph
  appendLineBreak body
  appendChild body $ button
  iterCount <- newMVar 0
  let addData d = do
        liftIO $ improveRandom $ toJSString d
        current <- liftIO $ modifyMVar iterCount (\prev -> return (1 + prev, 1 + prev))
        when (current == 256) $ E.removeAttribute button "disabled"
        setTextContent text2 $ Just ("Current iteration count: " ++ show current ++ " (added " ++ d ++ ")")

  releaser <- do
    body_ <- D.getBodyUnsafe doc
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
        touch' <- liftIO $ showTouchList touch
        addData $ show ("tup", touch')
        return ()
    addListener body_ G.touchEnd touchup False
    touchdown <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ showTouchList touch
        addData $ show ("tdown", touch')
        return ()
    addListener body_ G.touchStart touchdown False
    touchcancel <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ showTouchList touch
        addData $ show ("tcancel", touch')
        return ()
    addListener body_ G.touchCancel touchcancel False
    touchmove <- newListenerSync $ do
        touch <- event >>= getTouches
        touch' <- liftIO $ showTouchList touch
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
  liftIO $ on button G.click $ liftIO $ do
    releaser
    nextState
  on doc G.mouseMove $ do
      (x, y) <- mouseClientXY
      button <- mouseButton
      addData $ show (x, y, button)
      return ()
  return ()

