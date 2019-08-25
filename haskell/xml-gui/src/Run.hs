{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Xeno.DOM
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
  logInfo $ displayShow $ getNodeGroups . getRootNodes <$> content
