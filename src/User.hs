{-# LANGUAGE OverloadedStrings #-}
module User
(User
,userFormHandler  ) where

import qualified Data.Text as T
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Control.Applicative
import           Snap.Core (writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Heist (heistLocal, render)
import           Application

data User = User {
  username :: T.Text
} deriving (Show)
