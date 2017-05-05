{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PngCanvasContainer (CanvasPngImageType(..)) where

import Container.LosslessImage.ImageBindings ()
import Container.LosslessImage.ImageContainer (Pixel, ImageContainer(..), MutableImageContainer(..))
import Container.LosslessImage.ImageContainer (WithPixelInfoType(..), WithPixelInfoTypeM(..))
import Container.LosslessImage.ImageHandler (createCryptoState)
import SteganographyContainer (SteganographyContainerOptions(..))
import HelperFunctions (base64encode, base64prefix)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.Word (Word8, Word32)
import GHCJS.DOM (currentDocument, syncPoint, waitForAnimationFrame)
import GHCJS.DOM.Document (getBodyUnsafe, createElement)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Node (appendChild, removeChild_, contains)
import GHCJS.DOM.Types (unsafeCastTo, HTMLImageElement(..), HTMLCanvasElement(..), CanvasRenderingContext2D(..), RenderingContext(..), toJSString, fromJSString, Uint8ClampedArray(..), fromJSVal, JSString(..), toUint8Array)
import System.IO.Unsafe (unsafePerformIO)

import qualified GHCJS.DOM.HTMLImageElement as Image
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import qualified GHCJS.DOM.CanvasRenderingContext2D as Canvas
import qualified GHCJS.DOM.ImageData as ImageData
import qualified Data.JSString.Text as T
import qualified Data.Text as T
import qualified JavaScript.TypedArray as TA
import qualified GHCJS.DOM.Element as E
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified GHCJS.DOM.GlobalEventHandlers as G

data CanvasPngImage = CanvasPngImage HTMLCanvasElement Uint8ClampedArray (Word32, Word32, Word8)
data CanvasPngImageType = CanvasPngImageSpawner
                  | CanvasPngImageSpawnerFast
data MutableCanvasPngImage s = MutableCanvasPngImage ()

getContext canvas = do
  Just context <- Canvas.getContext canvas "2d" ([] :: [String])
  context' <- unsafeCastTo CanvasRenderingContext2D context
  return context'

foreign import javascript unsafe "console.log($1)" console_log :: JSString -> IO ()
foreign import javascript unsafe "(new Date()).getSeconds()" secondsNow :: IO (Word)
foreign import javascript unsafe "$1[$2]" index :: Uint8ClampedArray -> Word -> IO (Word8)

instance SteganographyContainerOptions CanvasPngImageType (WithPixelInfoType CanvasPngImage) where
  createContainer options imagedata = do
    (canvas, pixelData, size) <- unsafeIOToST $ do
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
        width <- Canvas.getWidth canvas
        height <- Canvas.getHeight canvas
        pixel <- Canvas.getImageData context 0 0 (fromIntegral width) (fromIntegral height)
        pixelData <- ImageData.getData pixel
        return (canvas, pixelData, (fromIntegral width, fromIntegral height, 4))
    let image = (CanvasPngImage canvas pixelData size)
    state <- createCryptoState False image
    return $ Right $ WithPixelInfoType image state


instance ImageContainer (CanvasPngImage) where
  getBounds (CanvasPngImage _ _ size) = size
  getPixelLsb state pos = if ((getPixel state pos) .&. 1) == 1 then True else False
  getPixel (CanvasPngImage canvas pixels (width, _, colors)) (x, y, c) = unsafePerformIO $ do
    color <- index pixels (fromIntegral y * fromIntegral width * fromIntegral colors + fromIntegral x * fromIntegral colors + fromIntegral c)
    return $ fromIntegral color
  withThawedImage (CanvasPngImage canvas pixels size) state func = error "With thawed image not implemented"

instance MutableImageContainer MutableCanvasPngImage where
  getBoundsM = error "getBoundsM not implemented"
  getPixelLsbM = error "getPixelLsbM not implemented"

  setPixelLsb = error "Not implemented"
