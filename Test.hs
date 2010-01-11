module Main where

import System

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.WebKit

import Network.Soup

main = startBrowser

startBrowser :: IO ()
startBrowser = do
    -- Init -------------------------------------------------------------------

    initWebKit

    -- Load GUI ---------------------------------------------------------------

    Just xml  <- xmlNew "test.glade"

    wMain     <- xmlGetWidget xml castToWindow "wMain"

    vbBrowser <- xmlGetWidget xml castToVBox "vbBrowser"
    swBrowser <- xmlGetWidget xml castToScrolledWindow "swBrowser"

    eAddress  <- xmlGetWidget xml castToEntry "eAddress"

    sbMain    <- xmlGetWidget xml castToStatusbar "sbMain"

    tbQuit    <- xmlGetWidget xml castToToolButton "tbQuit"
    tbPrint   <- xmlGetWidget xml castToToolButton "tbPrint"
    tbGo      <- xmlGetWidget xml castToToolButton "tbGo"
    tbStop    <- xmlGetWidget xml castToToolButton "tbStop"
    tbReload  <- xmlGetWidget xml castToToolButton "tbReload"
    tbBack    <- xmlGetWidget xml castToToolButton "tbBack"
    tbForward <- xmlGetWidget xml castToToolButton "tbForward"

    imiQuit   <- xmlGetWidget xml castToImageMenuItem "imiQuit"

    -- Create WebView ---------------------------------------------------------

    wvBrowser <- webViewNew
    containerAdd swBrowser wvBrowser

    -- Events -----------------------------------------------------------------

    onDestroy wMain mainQuit

    onToolButtonClicked tbQuit $ widgetDestroy wMain

    onToolButtonClicked tbPrint $
        webViewGetMainFrame wvBrowser >>= webFramePrint

    onToolButtonClicked tbGo $ do
        widgetGrabFocus wvBrowser
        loadAddress eAddress wvBrowser

    onToolButtonClicked tbStop $ webViewStopLoading wvBrowser

    onToolButtonClicked tbReload $ webViewReload wvBrowser

    onToolButtonClicked tbBack $ do
        widgetGrabFocus wvBrowser
        webViewGoBackOrForward wvBrowser (-1)

    onToolButtonClicked tbForward $ do
        widgetGrabFocus wvBrowser
        webViewGoForward wvBrowser

    onEntryActivate eAddress $ do
        widgetGrabFocus wvBrowser
        loadAddress eAddress wvBrowser

    onActivateLeaf imiQuit $ widgetDestroy wMain

    onWebViewStatusbarTextChanged wvBrowser $
        \text -> putStrLn $ "SB: " ++ text

    onWebViewTitleChanged wvBrowser $
        \frame -> windowSetTitle wMain

    onWebViewCopyClipboard wvBrowser $
        putStrLn "Copy"

    onWebViewLoadStarted wvBrowser $
        \frame -> widgetSetSensitive tbStop True

    onWebViewLoadFinished wvBrowser $
        \frame -> widgetSetSensitive tbStop False

    -- Show and run GUI -------------------------------------------------------

    widgetSetSensitive tbStop False

    loadAddress eAddress wvBrowser

    widgetGrabFocus wvBrowser
    widgetShowAll wMain
    mainGUI

loadAddress :: Entry -> WebView -> IO ()
loadAddress eAddress wvBrowser = do
    uri <- entryGetText eAddress 
    wfBrowser <- webViewGetMainFrame wvBrowser
    webViewOpen wvBrowser uri
    -- webViewGetTitle wvBrowser >>= print
    -- webViewGetUri wvBrowser >>= print
    -- webViewGetProgress wvBrowser >>= print
    -- webViewGetLoadStatus wvBrowser >>= print
    -- entrySetText eAddress newUri

    ws <- webSettingsNew
    webSettingsGetUserAgent ws >>= print
    webViewSetSettings wvBrowser ws 

    return ()

