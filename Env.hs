{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Env where

import Path
import QMK
import Shh

home :: Path Abs Dir
home = $(mkAbsDir "/home/lc")

source :: Path Abs Dir
source = home </> $(mkRelDir "Source")

github :: Path Abs Dir
github = source </> $(mkRelDir "github.com")

gitlab :: Path Abs Dir
gitlab = source </> $(mkRelDir "gitlab.com")

homotopicgl :: Path Abs Dir
homotopicgl = gitlab </> $(mkRelDir "homotopic-tech")

qmk_home :: QMKHome
qmk_home = QMKHome $ github </> $(mkRelDir "qmk/qmk_firmware")

qmk_keyboard :: QMKBoard
qmk_keyboard = QMKBoard "moonlander"

qmk_config :: QMKConfig
qmk_config = QMKConfig "locallycompact"

qmk_myconf :: QMKKeymapHome
qmk_myconf = qmkKeymapHome qmk_home qmk_keyboard qmk_config

editor x = exe "vim" x
