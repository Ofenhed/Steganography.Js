module HelperFunctions (receiveFile, prompt, fullScreenBody, base64encode, base64decode, base64prefix, FileUploadOptions(..), defaultFileUploadOptions, appendLineBreak, showMenu, setTitle, appendText, appendDownloadLink, LinkData(..)) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (getBodyUnsafe, getHeadUnsafe, createTextNode, createElement)
import GHCJS.DOM.EventM (on, uiKeyCode)
import GHCJS.DOM.Node (appendChild, setTextContent)
import GHCJS.DOM.Types (HTMLInputElement(..), unsafeCastTo, toJSVal, JSString, toJSString, fromJSString, ToJSString, FromJSString, HTMLButtonElement(..), HTMLAnchorElement(..))
import GHCJS.DOM.HTMLHyperlinkElementUtils (setHref)
import GHCJS.DOM.HTMLAnchorElement (setDownload)
import Control.Monad.Trans.Class (lift)

import qualified Data.Text as T
import qualified Data.JSString.Text as T
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.FileList as FileList
import qualified GHCJS.DOM.GlobalEventHandlers as G
import qualified GHCJS.DOM.HTMLElement as E
import qualified GHCJS.DOM.HTMLInputElement as Input
import qualified GHCJS.DOM.FileReader as FileReader
import qualified GHCJS.DOM.HTMLCollection as C

foreign import javascript unsafe "window.btoa($1)" btoa :: JSString -> JSString
foreign import javascript unsafe "window.atob($1)" atob :: JSString -> JSString

base64encode :: (FromJSString a, ToJSString a) => a -> a
base64encode x = fromJSString $ btoa $ toJSString x

base64decode :: (FromJSString a, ToJSString a) => a -> a
base64decode x = fromJSString $ atob $ toJSString x

base64prefix (Just t) = T.concat [T.pack "data:", t, T.pack ";base64,"]
trimBase64prefix Nothing haystack = case T.findIndex ((==) ',') haystack of
                                      Just i -> Just $ T.drop (i + 1) haystack
                                      _ -> Nothing
trimBase64prefix t'@(Just _) haystack = let t = base64prefix t' ; (before, after) = T.splitAt (T.length t) haystack
                                         in if before == t
                                             then Just after
                                             else Nothing

appendLineBreak at = do
  Just doc <- currentDocument
  linebreak <- createElement doc "br"
  appendChild at linebreak

appendText at content = do
  Just doc <- currentDocument
  text <- createTextNode doc content
  appendChild at text

data FileUploadOptions = FileUploadOptions { skipCaption :: Maybe T.Text, fileType :: Maybe (T.Text, Bool), text :: Maybe T.Text } deriving (Show)

defaultFileUploadOptions = FileUploadOptions Nothing Nothing Nothing

data LinkData = LinkData T.Text | LinkDataRaw T.Text

appendDownloadLink at title filename content = do
  Just doc <- currentDocument
  downloadLink <- createElement doc "a" >>= unsafeCastTo HTMLAnchorElement
  setHref downloadLink $ case content of
                           LinkData text -> T.concat [base64prefix $ Just $ T.pack "application/octet-stream", base64encode text]
                           LinkDataRaw text -> text
  setDownload downloadLink filename
  setTextContent downloadLink $ Just title
  appendChild at downloadLink
  return ()

receiveFile :: FileUploadOptions -> (Maybe T.Text -> IO ()) -> IO ()
receiveFile options nextState = do
  Just doc <- currentDocument
  body <- fullScreenBody
  when (isJust $ text options) $ do
    textNode <- createTextNode doc $ fromJust $ text options
    appendChild body textNode
    appendLineBreak body
    return ()
  button <- createElement doc "input" >>= unsafeCastTo HTMLInputElement
  E.setAttribute button "type" "file"
  when (isJust $ fileType options) $ E.setAttribute button "accept" $ fst $ fromJust $ fileType options
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
      let decodedData = if (isJust $ fileType options) && (snd $ fromJust $ fileType options)
                           then trimBase64prefix (Just $ fst $ fromJust $ fileType options) fileData
                           else trimBase64prefix Nothing fileData
      when (isNothing (fileType options) || isJust decodedData) $ do liftIO $ nextState $ Just $ base64decode $ fromJust decodedData
      return ()
    mapM (\x -> FileReader.readAsDataURL reader (Just x)) files
    --fileReader <- newFileReader
    --show $ getResult $ fileReader
    return ()
  when (isJust $ skipCaption options) $ do
    skipButton <- createElement doc "button" >>= unsafeCastTo HTMLButtonElement
    setTextContent skipButton $ skipCaption options
    appendChild body skipButton
    on skipButton G.click $ liftIO $ nextState Nothing
    return ()
  return ()

setTitle :: String -> IO ()
setTitle newTitle = do
  Just doc <- currentDocument
  head <- getHeadUnsafe doc
  titles <- E.getElementsByTagName head "title"
  length <- C.getLength titles
  case length of
    0 -> do
      title <- createElement doc "title"
      appendChild head title
      setTitle newTitle
    _ -> do
      Just title <- C.item titles 0
      setTextContent title $ Just newTitle
  return ()

type MenuItem = (T.Text, IO ())

showMenu target items = do
  Just doc <- currentDocument
  let showElements [] = return ()
      showElements ((text, action):xs) = do
        button <- createElement doc "button" >>= unsafeCastTo HTMLButtonElement
        setTextContent button $ Just text
        liftIO $ on button G.click $ liftIO action
        appendChild target button
        appendLineBreak target
        showElements xs
  showElements items

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

fullScreenBody = do
  Just doc <- currentDocument
  body <- createElement doc "div"
  --E.setAttribute body "style" "width: 100% ; height: 100%"
  body_ <- getBodyUnsafe doc
  E.setInnerHTML body_ $ Just ""
  appendChild body_ body
  return body

