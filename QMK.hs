{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QMK where

import Data.Text as T
import Formatting
import Shh
import Path

newtype QMKHome = QMKHome { unQmkHome :: Path Abs Dir }
  deriving newtype (PathLike Abs Dir)

newtype QMKKeymapHome = QMKKeymapHome {unQmkKeymapHome :: Path Abs Dir}
  deriving newtype (PathLike Abs Dir)

newtype QMKBoard = QMKBoard {unQmkBoard :: Text}
  deriving (Eq, Ord, Show)

qmkBoardf :: Format r (QMKBoard -> r)
qmkBoardf = mapf unQmkBoard stext

newtype QMKConfig = QMKConfig {unQmkConfig :: Text}
  deriving (Eq, Ord, Show)

qmkConfigf :: Format r (QMKConfig -> r)
qmkConfigf = mapf unQmkConfig stext

qmkKeymapHome :: QMKHome -> QMKBoard -> QMKConfig -> QMKKeymapHome
qmkKeymapHome q k c =
  let Right k' = parseRelDir $ T.unpack $ unQmkBoard k
      Right c' = parseRelDir $ T.unpack $ unQmkConfig c
   in QMKKeymapHome $ toPath q </> $(mkRelDir "keyboards") </> k' </> $(mkRelDir "keymaps") </> c'

reflashQmk :: QMKHome -> QMKBoard -> QMKConfig -> IO ()
reflashQmk q k c = do
  cd $ toFilePath . unQmkHome $ q
  exe "nix-shell" "--command" $ formatToString ("make " % qmkBoardf % ":" % qmkConfigf) k c
  exe "wally-cli" $ formatToString (qmkBoardf % "_" % qmkConfigf % ".bin") k c
  cd "-"
