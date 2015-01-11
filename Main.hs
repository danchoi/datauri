{-# LANGUAGE OverloadedStrings, Arrows #-}  
module Main where
import Text.XML.HXT.Core
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Text.XML.HXT.Arrow.XmlState.TypeDefs

type ImgState = [Image]
type ImgArrow = IOSLA (XIOState ImgState) XmlTree XmlTree

data Image = Image {
    mimeType :: String
  , base64Data :: BL.ByteString
  } deriving (Show)

extractInlineImages :: ImgArrow
extractInlineImages = processTopDown ( extractImg `when` isInlineImg)

isInlineImg = isElem 
              >>> hasName "img" 
              >>> hasAttr "src"
              >>> getAttrValue "src"
              >>> isA isDataURI
  where isDataURI = isPrefixOf "data:image/"

extractImg = 
    processAttrl (
        (changeAttrValue . const $< createImage)
        `when` hasName "src"
      )


-- We are on the src attribute node at this point, so the children is the src value
createImage :: IOSLA (XIOState ImgState) XmlTree String
createImage = 
    (saveImg $< xshow getChildren)
    >>> changeUserState (\x imgs -> (Image x ""):imgs )


saveImg :: String -> IOSLA (XIOState ImgState) XmlTree String
saveImg string = arrIO0 (putStrLn "TEST" >> return "TESTFILE")

    

main = do
  html <- getContents
  let doc = readString [withParseHTML yes, withWarnings no] html
  (s, xs) <- runIOSLA 
              ( doc 
                >>> extractInlineImages
                >>> writeDocument [withIndent yes ,withOutputEncoding utf8 ] "-"
              ) (initialState []) undefined
  print $ xioUserState s
  return ()
 
