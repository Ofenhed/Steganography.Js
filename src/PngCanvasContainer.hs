{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PngCanvasContainer (CanvasPngImageType(..)) where

import Container.LosslessImage.ImageBindings ()
import Container.LosslessImage.ImageContainer (Pixel, ImageContainer(..), MutableImageContainer(..))
import Container.LosslessImage.ImageContainer (WithPixelInfoType(..), WithPixelInfoTypeM(..))
import Container.LosslessImage.ImageHandler (createCryptoState)
import SteganographyContainer (SteganographyContainerOptions(..))
import HelperFunctions (base64encode, base64prefix)

import GHCJS.DOM (currentDocument, syncPoint, waitForAnimationFrame)
import GHCJS.DOM.Document (getBodyUnsafe, createElement)
import GHCJS.DOM.Types (unsafeCastTo, HTMLImageElement(..), HTMLCanvasElement(..), CanvasRenderingContext2D(..), RenderingContext(..), toJSString, fromJSString, unUint8ClampedArray, fromJSVal, JSString(..))
import GHCJS.DOM.ImageData (getData)
import GHCJS.DOM.Node (appendChild, removeChild_, contains)
import GHCJS.DOM.EventM (on)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import System.IO.Unsafe (unsafePerformIO)
import JavaScript.TypedArray (index)
import Data.Bits ((.&.))

import qualified GHCJS.DOM.HTMLImageElement as Image
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import qualified GHCJS.DOM.CanvasRenderingContext2D as Canvas
import qualified Data.JSString.Text as T
import qualified Data.Text as T
import qualified GHCJS.DOM.Element as E
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified GHCJS.DOM.GlobalEventHandlers as G

data CanvasPngImage = CanvasPngImage HTMLCanvasElement CanvasRenderingContext2D
data CanvasPngImageType = CanvasPngImageSpawner
                  | CanvasPngImageSpawnerFast
data MutableCanvasPngImage s = MutableCanvasPngImage ()

getContext canvas = do
  Just context <- Canvas.getContext canvas "2d" ([] :: [String])
  context' <- unsafeCastTo CanvasRenderingContext2D context
  return context'

foreign import javascript unsafe "console.log($1)" console_log :: JSString -> IO ()

instance SteganographyContainerOptions CanvasPngImageType (WithPixelInfoType CanvasPngImage) where
  createContainer options imagedata = do
    (canvas, context) <- unsafeIOToST $ do
        Just doc <- currentDocument
        body <- getBodyUnsafe doc
        img <- createElement doc "img" >>= unsafeCastTo HTMLImageElement
        canvas <- createElement doc "canvas" >>= unsafeCastTo HTMLCanvasElement
        appendChild body img
        appendChild body canvas
        context <- getContext canvas
        on img G.load $ do
          Image.getNaturalHeight img >>= (Canvas.setHeight canvas) . fromIntegral
          Image.getNaturalWidth img >>= (Canvas.setWidth canvas) . fromIntegral
          Canvas.drawImage context img 0 0
          removeChild_ body img
        Image.setSrc img $ T.concat [base64prefix $ Just $ T.pack $ "image/png", base64encode $ T.pack $ LBS.unpack imagedata]
        let waitForImage = do
              waitForAnimationFrame
              hasImg <- contains body $ Just img
              console_log $ toJSString $ T.concat [T.pack "Body contains img: ", T.pack $ show $ hasImg]
              when (hasImg) waitForImage
        waitForImage
        return (canvas, context)
    let image = (CanvasPngImage canvas context)
    state <- createCryptoState False image
    return $ Right $ WithPixelInfoType image state


instance ImageContainer (CanvasPngImage) where
  getBounds (CanvasPngImage canvas context) = (fromIntegral $ unsafePerformIO $ Canvas.getWidth canvas, fromIntegral $ unsafePerformIO $ Canvas.getHeight canvas, 3)
  getPixelLsb state pos = if ((getPixel state pos) .&. 1) == 1 then True else False
  getPixel (CanvasPngImage canvas context) (x, y, c) = unsafePerformIO $ do
    pixel <- Canvas.getImageData context (fromIntegral x) (fromIntegral y) 1 1
    pixelData <- getData pixel
    Just pixelData' <- fromJSVal $ unUint8ClampedArray pixelData :: IO (Maybe [Word])
    let color = fromIntegral $ pixelData' !! (fromIntegral c)
    console_log $ toJSString $ T.concat [T.pack "Read pixel: ", T.pack $ show $ color]
    return $ color
  withThawedImage = error "With thawed image not implemented"

instance MutableImageContainer MutableCanvasPngImage where
  getBoundsM = error "getBoundsM not implemented"
  getPixelLsbM = error "getPixelLsbM not implemented"

  setPixelLsb = error "Not implemented"
