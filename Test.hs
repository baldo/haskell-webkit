module Main where

import System
import System.Glib.Signals
import System.Glib.GType

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.WebKit
import Graphics.UI.Gtk.WebKit.General.Types

import Language.JavaScript.JavaScriptCore


import Network.Soup

import Graphics.UI.Gtk.Abstract.Object
    ( makeNewObject
    )



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

    -- WebKit Events ----------------------------------------------------------    

    --onWebViewStatusbarTextChanged wvBrowser $
    --    \text -> putStrLn $ "SB: " ++ text

    onWebViewCopyClipboard wvBrowser $
        putStrLn "Copy"

    onWebViewResourceRequestStarting wvBrowser $ 
       \ wv wf wr nreq _ -> do 
            putStrLn "onWebViewResourceRequestStarting"
            nr <- networkRequestGetUri nreq 
            putStrLn $ "-- URI: " ++ nr
            
    onWebViewStatusbarTextChanged wvBrowser $ 
        (\ x -> putStrLn $ "onWebViewStatusbarTextChanged" ++ x)

    onWebViewScriptPrompt wvBrowser $
        \ wv wf message def text -> do
            putStrLn "onWebViewScriptPrompt"
            webViewGetUri wv >>= print
            putStrLn message
            putStrLn def
            putStrLn text
            return False

    onWebViewDownloadRequested wvBrowser $
        \ webView download -> do
            putStrLn "onWebViewDownloadRequested"
            return True

    onWebViewNavigationPolicyDecisionRequested wvBrowser $
        \ webView webFrame networkRequest webNavigationAction webPolicyDecision ->
            putStrLn "onWebViewNavigationPolicyDecisionRequested"

    
    onWebViewMimeTypePolicyDecisionRequested wvBrowser $
        \ webView webFrame networkRequest mimetype policyDecision -> do
            putStrLn "onWebViewMimeTypePolicyDecisionRequested" 
            putStrLn $ "-- MIME: " ++ mimetype
            webPolicyDecisionUse policyDecision 
            return True
        
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
    webViewLoadUri wvBrowser uri
    -- webViewGetTitle wvBrowser >>= print
    -- webViewGetUri wvBrowser >>= print
    -- webViewGetProgress wvBrowser >>= print
    -- webViewGetLoadStatus wvBrowser >>= print
    -- entrySetText eAddress newUri

    ws <- webSettingsNew
    webSettingsGetUserAgent ws >>= print
    webViewSetSettings wvBrowser ws 

    return ()

