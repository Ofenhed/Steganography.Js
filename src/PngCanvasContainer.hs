{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PngCanvasContainer (CanvasPngImageType(..)) where

import Container.LosslessImage.ImageBindings ()
import Container.LosslessImage.ImageContainer (Pixel, ImageContainer(..), MutableImageContainer(..))
import Container.LosslessImage.ImageContainer (WithPixelInfoType(..), WithPixelInfoTypeM(..))
import Container.LosslessImage.ImageHandler (createCryptoState)
import SteganographyContainer (SteganographyContainerOptions(..))
import HelperFunctions (base64encode, base64prefix)

import Control.DeepSeq (force)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Bits ((.&.), (.|.), complement)
import Data.Word (Word8, Word32)
import GHCJS.DOM (currentDocument, syncPoint, waitForAnimationFrame)
import GHCJS.DOM.Document (getBodyUnsafe, createElement)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Node (appendChild, removeChild_, contains)
import GHCJS.DOM.Types (unsafeCastTo, HTMLImageElement(..), HTMLCanvasElement(..), CanvasRenderingContext2D(..), RenderingContext(..), toJSString, fromJSString, Uint8ClampedArray(..), fromJSVal, JSString(..), toUint8Array, ImageData(..))
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

import Debug.Trace

data CanvasPngImage = CanvasPngImage HTMLCanvasElement ImageData (Word32, Word32, Word8)
data CanvasPngImageType = CanvasPngImageSpawner
                  | CanvasPngImageSpawnerFast
data MutableCanvasPngImage s = MutableCanvasPngImage CanvasPngImage

getContext canvas = do
  Just context <- Canvas.getContext canvas "2d" ([] :: [String])
  context' <- unsafeCastTo CanvasRenderingContext2D context
  return context'

foreign import javascript unsafe "console.log($1)" console_log :: JSString -> IO ()
foreign import javascript unsafe "(new Date()).getSeconds()" secondsNow :: IO (Word)
foreign import javascript unsafe "$1[$2]" index :: Uint8ClampedArray -> Word -> IO (Word8)
foreign import javascript unsafe "$1[$2] = $3" indexSet :: Uint8ClampedArray -> Word -> Word8 -> IO ()

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
          width <- Image.getNaturalWidth img
          height <- Image.getNaturalHeight img
          Canvas.setWidth canvas $ fromIntegral width
          Canvas.setHeight canvas $ fromIntegral height
          Canvas.clearRect context 0 0 (fromIntegral width) (fromIntegral height)
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
        pixels <- Canvas.getImageData context 0 0 (fromIntegral width) (fromIntegral height)
        return (canvas, pixels, (fromIntegral width, fromIntegral height, 4))
    let image = (CanvasPngImage canvas pixelData size)
    state <- createCryptoState (case options of CanvasPngImageSpawnerFast -> True ; CanvasPngImageSpawner -> False) image
    return $ Right $ WithPixelInfoType image state

pixelPos (width, _, colors) x y c = (fromIntegral y * fromIntegral width * fromIntegral colors + fromIntegral x * fromIntegral colors + fromIntegral c)

instance ImageContainer (CanvasPngImage) where
  getBounds (CanvasPngImage _ _ (width, height, colors)) = (width, height, colors - 1)
  getPixelLsb state pos = if ((getPixel state pos) .&. 1) == 1 then True else False
  getPixel (CanvasPngImage canvas pixels bounds) (x, y, c) = unsafePerformIO $ do
    pixels' <- ImageData.getData pixels
    color <- index pixels' $ pixelPos bounds x y c
    return $ fromIntegral $ force $ color
  withThawedImage img@(CanvasPngImage canvas pixels _) state func = do
    result <- func $ WithPixelInfoTypeM (MutableCanvasPngImage img) state
    case result of
      Left err -> return $ Left err
      Right _ -> lift $ unsafeIOToST $ do
        context <- getContext canvas
        Canvas.putImageData context pixels 0 0
        result <- Canvas.toDataURL canvas $ Just "image/png"
        console_log $ toJSString $ T.pack "Image ready"
        return $ Right $ LBS.pack $ fromJSString result


instance MutableImageContainer MutableCanvasPngImage where
  getBoundsM (MutableCanvasPngImage static) = return $ getBounds static
  getPixelLsbM (MutableCanvasPngImage img) = return . getPixelLsb img

  setPixelLsb (MutableCanvasPngImage (CanvasPngImage canvas pixels bounds)) (x, y, c) b = do
    let pos = pixelPos bounds x y c
        --change = (\old -> if b then old .|. 1 else old .&. complement 1)
        change = (\old -> if b then complement 0 else 0)
    unsafeIOToST $ do
      pixels' <- ImageData.getData pixels
      before <- index pixels' pos
      let after = change before
      when (before /= after) $ indexSet pixels' pos after
