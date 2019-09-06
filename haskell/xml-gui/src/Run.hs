{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Xeno.DOM
import qualified Brick as B
import Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as BC
import qualified RIO.List as L

getRootNodes :: Node -> [ByteString]
getRootNodes node =
  name <$> children node

getNodeGroups :: [ByteString] -> [(ByteString, Int)]
getNodeGroups names = (\xs -> 
  case L.headMaybe xs of
    Nothing -> (mempty, 0)
    Just x -> (x, L.length xs)
  ) <$> L.group names

run :: RIO App ()
run = do
  file <- readFileBinary "example.xml"
  let content = Xeno.DOM.parse file
    app = B.App {}
    initialState = 

  B.withBorderStyle BS.unicode $
    BB.borderWithLabel (B.str "Hello!") $
    (BC.center (B.str "Left") B.<+> BB.vBorder B.<+> BC.center (B.str "Right"))

  -- logInfo $ displayShow $ getNodeGroups . getRootNodes <$> content
