{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Composite.Record
import Env
import Data.Kind
import Data.Map (fromList)
import Dhall
import Data.Semigroup
import Data.Text as T
import Graphics.X11.ExtraTypes.XF86
import Formatting
import Formatting.Combinators
import Path
import Polysemy
import QMK
import qualified XMonad.StackSet as W
import Shh
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.Volume
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.AutoMaster
import XMonad.Layout
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Input
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import Data.Vinyl
import Data.Vinyl.Functor

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect 0 0 1 0.4)

newtype TermTitle = TermTitle { unTermTitle :: Text }
  deriving (Eq, Show)

newtype TermWD = TermWD { unTermWD :: Path Abs Dir }
  deriving (Eq, Show)

newtype TermCommand = TermCommand { unTermCommand :: Text }
  deriving (Eq, Show)

newtype ShellCommand (s :: ShellId) = ShellCommand { unShellCommand :: Text }
  deriving (Eq, Show)

data ShellId = Nu | Fish | Bash | Shh

data TermData = TermData {
  termTitle   :: Maybe TermTitle
, termWD      :: Maybe TermWD
, termHold    :: TermHold
, termCommand :: TermCommand
} 

data Alacritty m a where
  Alacritty :: TermData -> Alacritty m ()

class Terminal m where
  runTerm :: TermData -> Sem (m ': r) ()

data RShell s m a where
  Shell :: ShellCommand s -> RShell s m ()

data TermHold = Hold | NoHold

makeSem ''Alacritty
makeSem ''RShell

instance Terminal Alacritty where
  runTerm = alacritty

nu :: Member (RShell Nu) r => ShellCommand Nu -> Sem r ()
nu = shell @Nu

shh :: Member (RShell Shh) r => ShellCommand Shh -> Sem r ()
shh = shell @Shh

fish :: Member (RShell Fish) r => ShellCommand Fish -> Sem r ()
fish = shell @Fish

nuTC :: ShellCommand Nu -> TermCommand
nuTC = TermCommand . sformat ("nu -c " % dquoted shellCommandf)

shhTC :: ShellCommand Shh -> TermCommand
shhTC = TermCommand . sformat ("/home/lc/.xmonad/shh " % dquoted shellCommandf)

fishTC :: ShellCommand Fish -> TermCommand
fishTC = TermCommand . sformat ("fish -c " % dquoted shellCommandf)

class RShellC s where
  toTermCommand :: ShellCommand s -> TermCommand

instance RShellC Nu where
  toTermCommand = nuTC

instance RShellC Shh where
  toTermCommand = shhTC

instance RShellC Fish where
  toTermCommand = fishTC

termCommandf :: Format r (TermCommand -> r)
termCommandf = mapf unTermCommand stext

shellCommandf :: Format r (ShellCommand x -> r)
shellCommandf = mapf unShellCommand stext

holdToFlag :: TermHold -> Text
holdToFlag Hold = "--hold"
holdToFlag _ = ""

termHoldf :: Format r (TermHold -> r)
termHoldf = mapf k stext
  where 
    k Hold = "--hold"
    k _    = ""

pathf :: Format r (Path b t -> r)
pathf = mapf (T.pack . toFilePath) stext

termWDf :: Format r (TermWD -> r)
termWDf = "--working-directory " % dquoted (mapf unTermWD pathf)

termTitlef :: Format r (TermTitle -> r)
termTitlef = "--title " % dquoted (mapf unTermTitle stext)

alacrittyb :: Format r r
alacrittyb = now "alacritty"

alacrittyf :: Format r (Maybe TermTitle -> Maybe TermWD -> TermHold -> TermCommand -> r)
alacrittyf = alacrittyb % " " % optioned termTitlef % " " % optioned termWDf % " " % termHoldf % " -e " % termCommandf

runShell :: forall m n r a. (RShellC n, Terminal m) => Maybe TermTitle -> Maybe TermWD -> TermHold -> Sem (RShell n ': r) a -> Sem (m ': r) a
runShell t d h = reinterpret $ \case
  Shell c -> runTerm $ TermData t d h $ toTermCommand c

runAlacrittyInX :: Sem (Alacritty ': r) a -> Sem (Embed IO ': r) a
runAlacrittyInX = reinterpret $ \case
  Alacritty (TermData t w h x) -> unsafeSpawn $ formatToString alacrittyf t w h x

runShh :: MonadIO m => Maybe TermTitle -> Maybe TermWD -> TermHold -> Sem '[RShell Shh] () -> m ()
runShh x d h = liftIO . runM . runAlacrittyInX . runShell @Alacritty @Shh x d h

runFish :: MonadIO m => Maybe TermTitle -> Maybe TermWD -> TermHold -> Sem '[RShell Fish] () -> m ()
runFish x d h = liftIO . runM . runAlacrittyInX . runShell @Alacritty @Fish x d h 

term :: MonadIO m => Maybe TermTitle -> Maybe TermWD -> TermHold -> Sem '[RShell Nu] () -> m ()
term x d h = liftIO . runM . runAlacrittyInX . runShell @Alacritty @Nu x d h

runLibraryScript :: MonadIO m => Maybe TermTitle -> TermHold -> Text -> m ()
runLibraryScript x h k = runShh x (Just $ TermWD $(mkAbsDir "/home/lc/.xmonad")) h $ shh $ ShellCommand $ "./" <> k

nixpkgs :: Path Abs Dir
nixpkgs = github </> $(mkRelDir "NixOS/nixpkgs")

nixosRebuildSwitch :: ShellCommand a
nixosRebuildSwitch = ShellCommand "sudo nixos-rebuild switch"

nixosRebuildSwitchUpgrade :: ShellCommand a
nixosRebuildSwitchUpgrade = ShellCommand "sudo nixos-rebuild switch --upgrade"

nixosRebuildSwitchUpgradeLocal :: ShellCommand a
nixosRebuildSwitchUpgradeLocal = ShellCommand "sudo nixos-rebuild switch --upgrade -Inixpkgs=/home/lc/Source/github.com/NixOS/nixpkgs"

xNixosRebuildSwitch :: X ()
xNixosRebuildSwitch = term (Just $ TermTitle "NixOS Rebuild") Nothing NoHold $ nu $ nixosRebuildSwitch

xNixosRebuildSwitchUpgrade :: X ()
xNixosRebuildSwitchUpgrade = term (Just $ TermTitle "NixOS Rebuild Upgrade") Nothing NoHold $ nu $ nixosRebuildSwitchUpgrade

xNixosRebuildSwitchUpgradeLocal :: X ()
xNixosRebuildSwitchUpgradeLocal = term (Just $ TermTitle "NixOS Rebuild Switch Local") Nothing NoHold $ nu $ nixosRebuildSwitchUpgradeLocal

xEditMoonlander :: X () 
xEditMoonlander = term (Just $ TermTitle "Edit Moonlander") (Just $ TermWD $ unQmkKeymapHome $ qmk_myconf) NoHold $ nu $ vimCurrentDir

xReflashMoonlander :: X ()
xReflashMoonlander = runLibraryScript (Just $ TermTitle $ "Reflash Moonlander") Hold "reflash-moonlander"

vimCurrentDir :: ShellCommand a
vimCurrentDir = ShellCommand "vim ."

xNixosVim :: X ()
xNixosVim = term (Just $ TermTitle "Edit NixOS Config") Nothing Hold $ nu $ vimCurrentDir

xNixpkgsVim :: X ()
xNixpkgsVim = term (Just $ TermTitle "Edit Nixpkgs") (Just $ TermWD nixpkgs) Hold $ nu $ vimCurrentDir

nixosMenu :: [(String, X ())]
nixosMenu = [ ("NixOS Rebuild", xNixosRebuildSwitch)
            , ("NixOS Rebuild Upgrade", xNixosRebuildSwitchUpgrade)
            , ("NixOS Rebuild Upgrade Local", xNixosRebuildSwitchUpgradeLocal)
            , ("Edit NixOS Config", xNixosVim)
            , ("Edit Local Nixpkgs", xNixpkgsVim)]

qmkMenu :: [(String, X ())]
qmkMenu = [ ("Edit Moonlander", xEditMoonlander)
          , ("Reflash Moonlander", xReflashMoonlander)]

stackNew :: String -> X ()
stackNew x = term (Just $ TermTitle "New Stack Library") (Just $ TermWD (gitlab </> $(mkRelDir "homotopic-tech"))) Hold $ nu $ ShellCommand $ "stack new " <> T.pack x

load SearchPath ["touch"]

stackMenu :: [(String, X ())]
stackMenu = [ ("New Stack Library", sDo)]
--stackMenu = [ ("New Stack Library", inputPrompt def "Name" ?+ stackNew)]

sDo :: X ()
sDo = liftIO $ touch "/home/lc/foooooo"

newtype User = User { unUser :: Text } 
  deriving (Eq, Show)

userf :: Format r (User -> r)
userf = mapf unUser stext

newtype Hostname = Hostname { unHostname :: Text }
  deriving (Eq, Show)

hostnamef :: Format r (Hostname -> r)
hostnamef = mapf unHostname stext

sshf :: Format r (AgentB -> User -> Hostname -> r) 
sshf = "ssh " % agentf % " " % userf % "@" % hostnamef

data AgentB = AgentOn | AgentOff
  deriving (Eq, Show)

agentf :: Format r (AgentB -> r)
agentf = mapf k stext where
  k AgentOn  = "-A"
  k AgentOff = ""

runSsh :: AgentB -> User -> Hostname -> X ()
runSsh b u h = runFish Nothing Nothing NoHold $ fish $ ShellCommand $ sformat sshf b u h

lc = User "lc"
centos = User "centos"
root = User "root"
pijul = User "pijul"
guest = User "guest"

aiur = Hostname "aiur"
argus = Hostname "argus"
icecrown = Hostname "icecrown"
deathegg = Hostname "deathegg"
skettis = Hostname "skettis"
dalaran = Hostname "dalaran"
mobius = Hostname "mobius"
antiga = Hostname "antiga"

sshLocs :: [(User, Hostname)]
sshLocs = [(lc, aiur), (lc, argus), (lc, icecrown), (lc, mobius), (lc, skettis), (lc, dalaran), (pijul, mobius), (centos, deathegg), (centos,antiga)]

sshMenu :: AgentB -> [(String, X ())]
sshMenu b = (\(x,y) -> (formatToString sshf b x y, runSsh b x y)) <$> sshLocs

main = xmonad . ewmh $ def {
  borderWidth = 2
, modMask = mod4Mask
, focusedBorderColor = "#0099FF" 
, manageHook = manageScratchPad
, terminal = "alacritty"
, keys = keys def <>
    \c -> fromList [
      ((0, xF86XK_AudioRaiseVolume   ), spawn "pactl set-sink-volume 0 +1.5%")
    , ((0, xF86XK_AudioLowerVolume   ), spawn "pactl set-sink-volume 0 -1.5%")
    , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")
    , ((0, xK_F12), scratchpadSpawnActionTerminal "urxvt")
    , ((0, xK_F10), runSelectedAction def { gs_cellwidth = 400 } nixosMenu)
    , ((0, xK_F11), runSelectedAction def { gs_cellwidth = 400 } qmkMenu)
    , ((0, xK_F9), runSelectedAction def { gs_cellwidth = 400 } stackMenu)
    , ((0, xK_F8), runSelectedAction def { gs_cellwidth = 400 } (sshMenu AgentOn))
    , ((shiftMask, xK_F8), runSelectedAction def { gs_cellwidth = 400 } (sshMenu AgentOff))
    ]
}
