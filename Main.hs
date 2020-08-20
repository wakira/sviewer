{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main where

import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Address as Address
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Subscribe as Subscribe
import qualified Sound.ALSA.Sequencer as SndSeq
import Control.Concurrent (forkIO)
import Control.Monad (liftM4, join, forever, ap)
import Control.Monad.Trans.Reader (runReaderT)
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import Data.GI.Base
import qualified GI.Cairo as GI.Cairo
import qualified GI.Poppler as P
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)
import Data.Word (Word8)
import Data.IORef

import qualified Data.Text as T
import qualified GI.Gtk.Objects.DrawingArea as M

import System.Environment (getProgName, getArgs)

getRemote =
  SndSeq.withDefault SndSeq.Block $ \h ->
    ClientInfo.queryLoop (h :: SndSeq.T SndSeq.OutputMode) $ \cinfo -> do
      client <- ClientInfo.getClient cinfo
      PortInfo.queryLoop h client $ \pinfo -> do
        remoteClient <- PortInfo.getClient pinfo
        putStrLn $ show remoteClient
        remotePort <- PortInfo.getPort pinfo
        putStrLn $ show remotePort
        return (remoteClient, remotePort)

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = withManagedPtr ct $ \p ->
                         runReaderT (runRender r) (Cairo (castPtr p))

drawCb :: IORef Double -> P.Page -> GI.Cairo.Context -> IO Bool
drawCb r page ct = do
  sc <- readIORef r
  renderWithContext ct $ do
    scale sc sc
  P.pageRender page ct
  return False

main :: IO ()
main = do
  m <- newIORef 10

  progName <- T.pack <$> getProgName
  strArgs <- getArgs
  let args = map T.pack strArgs

  let [remoteClientId, remotePortId] = map (read :: String -> Word8) strArgs
  let remoteClient = Client.Cons remoteClientId
  let remotePort = Port.Cons remotePortId

  -- blocked by code above
  Gtk.init $ Just (progName : args)

  doc <- P.documentNewFromFile "file:///home/autrim64/Documents/sicp.pdf" Nothing

  page <- #getPage doc 77

  win <- new Gtk.Window [ #title := "Sheet Viewer" ]

  board <- new Gtk.DrawingArea [ ]

  #add win board

  #setSizeRequest board 100 100

  on board #draw (drawCb m page)

  on win #destroy Gtk.mainQuit

  -- blocking listen-and-print loop
  forkIO $ SndSeq.withDefault SndSeq.Block $ \h -> do
    clientId <- Client.getId h
    Port.withSimple (h :: SndSeq.T SndSeq.InputMode) "primary" (Port.caps [Port.capWrite, Port.capSubsWrite]) Port.typeMidiGeneric $ \p -> do
      let destAddr = Address.Cons clientId p
      let sourceAddr = Address.Cons remoteClient remotePort
      -- subscribe to my piano
      Subscribe.subscribe h sourceAddr destAddr False Nothing
      forever $ do
        putStrLn "waiting for an event:"
        ev <- Event.input h
        print ev
        Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
          modifyIORef m (* 0.9)
          #queueDraw board
          return False

  #showAll win

  Gtk.main
