{-# LANGUAGE ScopedTypeVariables #-}
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome
import XMonad.Util.EZConfig

-- Shell prompt FTW
import XMonad.Prompt
import XMonad.Prompt.Shell

-- I use xmonad in a Gnome session and I dedicate the top area of the screen to some windows managed
-- independently. ManageDocks is required for XMonad to respect the struts established for these
-- areas.
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- According to the FAQ this helps with gvim's poor behavior under xmonad
import XMonad.Layout.IM
import XMonad.Layout.LayoutHints
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders

import System.Exit
import System.Posix.Process
import System.Cmd (system)
import Data.IORef

import Graphics.X11
-- import Graphics.X11.Xlib.Extras

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe ( maybe )
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits
import Data.Maybe (fromJust)
    
{- My desktop needs some custom state information to be threaded throughout the various handlers.
 - Namely:
 -}
data DesktopState = DesktopState 
    {
        {- The set of workspaces that have had a GHCI xterm started on them. This prevents
         - duplicate GHCI xterms from being started and a GHCI xterm from being started if I've
         - already killed it.
         -}
        workspace_prompts :: Map WorkspaceId (Maybe Window),
        {- When a GHCI prompt is brought into focus the ID of the window that was in focus needs to
         - be retained. On desktop switch this needs to be cleared.
         -}
        previously_focused_window :: Maybe Window,
        {- The top area, which is occupied by the GHCi xtgerm and conky display can be displayed or
         - hidden as requested.
         -}
        top_area_visible :: Bool,
        -- Retain the conky window in order to toggle it's visibility later
        conky_window :: Maybe Window
    }
    deriving (Show, Read)

type DesktopStateRef = IORef DesktopState

top_area_height :: Integral i => i
top_area_height = 100

-- I like big borders and I do not lie.
border_size :: Integral i => i
border_size = 3

-- Mo' workspaces -> Mo' Better.
-- In my case 16 workspaces is just about right.
workspace_IDs =
    map show [0..15 :: Int]

data WorkspaceRow = WorkspaceRow Int
    deriving Show

row_for_workspace_ID :: String -> WorkspaceRow
row_for_workspace_ID workspace_ID = 
    let num :: Int = read workspace_ID
        base_index = num `div` 4
    in WorkspaceRow base_index

workspace_ID_for_row_index :: WorkspaceRow -> Int -> String
workspace_ID_for_row_index (WorkspaceRow base_index) offset =
    show $ (base_index * 4) + offset

-- The left side of the keyboad is used for switching between the various workspaces. 
-- I can't find a way to make this generic across keymaps. I'd love to be able to query a keyboard
-- layout as if it was a physical space. Alas... Hardcoding the keys is required.
workspace_keys :: [KeySym]
workspace_keys = 
    [xK_1 .. xK_4] 
    ++ [xK_q, xK_w, xK_e, xK_r]
    ++ [xK_a, xK_s, xK_d, xK_f]
    ++ [xK_z, xK_x, xK_c, xK_v]

keybindings :: DesktopStateRef -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
keybindings state_ref conf@(XConfig {XMonad.modMask = modMask}) = Map.fromList $
    {- Launching and Killing programs:
     - Aside from a shortcut to launch an XTerm or two I rely on the handy autocomplete of
     - shellPrompt to launch everything.
     -}
    [ ((modMask .|. shiftMask, xK_apostrophe ), spawn "xterm") -- @@ Launch an xterm
    , ((modMask .|. shiftMask, xK_u          ), spawn "cd Development && xterm") -- @@ Launch an xterm
    , ((modMask .|. controlMask, xK_apostrophe), shellPrompt defaultXPConfig)
    , ((modMask .|. controlMask, xK_k     ), kill) -- @@ Close the focused window

    -- Window layout manipulation
    , ((modMask,               xK_space ), sendMessage NextLayout) -- @@ Rotate through the available layout algorithms
    , ((modMask,               xK_n     ), refresh) -- @@ Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_j     ), windows W.focusDown) -- @@ Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- @@ Move focus to the previous window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- @@ Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- @@ Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- @@ Swap the focused window with the previous window

    -- increase or decrease number of windows in the master area
    , ((modMask .|. shiftMask, xK_h ), sendMessage (IncMasterN 1)) -- @@ Increment the number of windows in the master area
    , ((modMask .|. shiftMask, xK_l), sendMessage (IncMasterN (-1))) -- @@ Deincrement the number of windows in the master area

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- @@ Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- @@ Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- quit, or restart
    , ((modMask .|. controlMask, xK_slash), restart "xmonad" True) -- @@ Restart xmonad
    , ((modMask .|. controlMask .|. shiftMask, xK_slash), io (exitWith ExitSuccess)) -- @@ exit xmonad

    -- Toggle focus of the ghci prompt dedicated to this workspace.
    , ((modMask .|. controlMask, xK_p), toggle_ghci_prompt_focus state_ref)

    -- Show/Hide the top-are windows
    , ((controlMask .|. shiftMask, xK_p), toggle_top_area state_ref)
    ] ++ [ ((m .|. modMask, k), f i)
         | (i, k) <- zip (XMonad.workspaces conf) (map (\i -> workspace_keys !! i) [0..15])
         , (f, m) <- [(change_to_workspace state_ref, 0), (windows . W.shift, shiftMask)]
         ]

toggle_ghci_prompt_focus state_ref = do
    state <- liftIO $ readIORef state_ref
    workspace_ID <- do
        current_screen <- get >>= return . W.current . windowset
        return $ W.tag $ W.workspace current_screen
    display_ID <- ask >>= return . display
    case previously_focused_window state of
        Nothing -> do
            if (not $ Map.member workspace_ID (workspace_prompts state))
                then return ()
                else do
                    (old_window_ID,_) <- io $ getInputFocus display_ID
                    case (Map.lookup workspace_ID (workspace_prompts state)) of
                        Just (Just window_ID) -> setFocusX window_ID
                        _ -> return ()
                    liftIO $ writeIORef state_ref $ state { previously_focused_window = Just old_window_ID }
        Just w -> do
            setFocusX w
            liftIO $ writeIORef state_ref $ state { previously_focused_window = Nothing }

toggle_top_area state_ref = do
    state <- liftIO $ readIORef state_ref
    display_ID <- ask >>= return . display
    workspace_ID <- do
        current_screen <- get >>= return . W.current . windowset
        return $ W.tag $ W.workspace current_screen
    state' <- 
        case top_area_visible state of
            True -> do
                case conky_window state of
                    Nothing -> return ()
                    Just window_ID -> do
                        liftIO $ do
                            -- I believe this is to prevent anything that needs to repond to the shape of
                            -- the window to ignore the unmap.
                            -- selectInput display_ID window_ID (clientMask .&. complement structureNotifyMask)
                            remove_top_space_strut display_ID window_ID
                            unmapWindow display_ID window_ID
                            -- selectInput display_ID window_ID clientMask
                        setWMState window_ID iconicState
                case (Map.lookup workspace_ID (workspace_prompts state)) of
                    Just (Just window_ID) -> do
                        liftIO $ do
                            -- I believe this is to prevent anything that needs to repond to the shape of
                            -- the window to ignore the unmap.
                            -- selectInput display_ID window_ID (clientMask .&. complement structureNotifyMask)
                            remove_top_space_strut display_ID window_ID
                            unmapWindow display_ID window_ID
                            -- selectInput display_ID window_ID clientMask
                        setWMState window_ID iconicState
                    _ -> return ()
                return $ state { top_area_visible = False }
            False -> do
                case conky_window state of
                    Nothing -> return ()
                    Just window_ID -> do
                        liftIO $ mapWindow display_ID window_ID
                        setWMState window_ID normalState
                        add_conky_strut display_ID window_ID
                case (Map.lookup workspace_ID (workspace_prompts state)) of
                    Just (Just window_ID) -> do
                        liftIO $ do mapWindow display_ID window_ID
                        setWMState window_ID normalState
                        add_GHCI_prompt_strut display_ID window_ID
                    _ -> return ()
                return $ state { top_area_visible = True }
    liftIO $ writeIORef state_ref state'
    -- Need to refresh in order for the changes to the struts caused by showing or hiding the top
    -- area to have an effect.
    refresh
    return ()
    
-- Spawns a prompt on the workspace if none has been spawned already.
-- Does not insert the prompt into the map. That is done in the manage hook.
change_to_workspace state_ref workspace_ID = do
    state <- liftIO $ readIORef state_ref
    display_ID <- ask >>= return . display
    old_workspace_ID <- do
        current_screen <- get >>= return . W.current . windowset
        return $ W.tag $ W.workspace current_screen
    if (not $ Map.member old_workspace_ID (workspace_prompts state))
        then return ()
        else do
            case (Map.lookup old_workspace_ID (workspace_prompts state)) of
                Just (Just window_ID) -> do
                    liftIO $ do
                        selectInput display_ID window_ID (clientMask .&. complement structureNotifyMask)
                        remove_top_space_strut display_ID window_ID
                        unmapWindow display_ID window_ID
                        selectInput display_ID window_ID clientMask
                    setWMState window_ID iconicState
                _ -> return ()
    windows $ W.greedyView workspace_ID
    if Map.member workspace_ID (workspace_prompts state) 
        then if (not $ top_area_visible state) 
            then return ()
            else case (Map.lookup workspace_ID (workspace_prompts state)) of
                Just (Just window_ID) -> do
                    liftIO $ mapWindow display_ID window_ID
                    add_GHCI_prompt_strut display_ID window_ID
                    setWMState window_ID normalState
                _ -> return ()
        else do
            spawn_GHCI
            return ()
    -- Workaround. Assures the windows are layed out again.
    windows $ W.greedyView workspace_ID
    liftIO $ modifyIORef state_ref $ \s -> s { previously_focused_window = Nothing }
        
manage_new_prompt state_ref window_ID = do
    workspace_ID <- liftX $ do
        current_screen <- get >>= return . W.current . windowset
        return $ W.tag $ W.workspace current_screen
    display_ID <- liftX $ ask >>= return . display
    state <- liftIO $ readIORef state_ref
    maybe 
        ( return () )
        ( maybe (return ()) $ \v -> if v == window_ID 
            then return () 
            else liftIO $ destroyWindow display_ID v
        )
        ( Map.lookup workspace_ID (workspace_prompts state) )

    liftIO $ allocaSetWindowAttributes $ \attrs_ptr -> do
        set_override_redirect attrs_ptr True
        changeWindowAttributes display_ID window_ID cWOverrideRedirect attrs_ptr
    when (top_area_visible state) (liftX $ add_GHCI_prompt_strut display_ID window_ID)
    let workspace_prompts' = 
            foldl (\s i -> Map.insert i (Just window_ID) s) 
                  (workspace_prompts state)
                  workspace_IDs
    liftIO $ writeIORef state_ref $ state { workspace_prompts = workspace_prompts' }
    -- Ignore before unmaping/iconifying as ignore implicity reveals the window
    when (not $ top_area_visible state) $ liftX $ do
        liftIO $ do
            selectInput display_ID window_ID (clientMask .&. complement structureNotifyMask)
            unmapWindow display_ID window_ID
            selectInput display_ID window_ID clientMask
        setWMState window_ID iconicState
    when (top_area_visible state) $ liftIO $ do
        mapWindow display_ID window_ID
    idHook
    
-- Ripped from xmobar. Not exactly sure what it all means.
add_top_space_strut :: Position -> Position -> Display -> Window -> IO ()
add_top_space_strut start_x end_x d w = do
    a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
    c1 <- internAtom d "CARDINAL"                 False
    a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
    c2 <- internAtom d "ATOM"                     False
    v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
    changeProperty32 d w a1 c1 propModeReplace $ 
        [0, 0, -- left, right
        top_area_height - border_size,  -- top
        0,  -- bottom 
        0, 0, -- left start/end
        0, 0, -- right start/end
        fromIntegral start_x, fromIntegral end_x,  -- top start/end
        0,  0 -- bottom start/end
        ] 
    changeProperty32 d w a2 c2 propModeReplace [fromIntegral v]

remove_top_space_strut :: Display -> Window -> IO ()
remove_top_space_strut d w = do
    a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
    c1 <- internAtom d "CARDINAL"                 False
    a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
    c2 <- internAtom d "ATOM"                     False
    v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
    changeProperty32 d w a1 c1 propModeReplace $ 
        [0, 0, -- left, right
        0,  -- top
        0,  -- bottom 
        0, 0, -- left start/end
        0, 0, -- right start/end
        0, 0,  -- top start/end
        0,  0 -- bottom start/end
        ] 
    changeProperty32 d w a2 c2 propModeReplace [fromIntegral v]

add_conky_strut display_ID window_ID = do
    Rectangle _ _ width height <- gets $ screenRect . W.screenDetail . W.current . windowset
    let new_rect = Rectangle x' 0 width' (top_area_height - border_size) 
        width' = (fromIntegral width) `div` 2 
        x' = (fromIntegral width) `div` 2
    tileWindow window_ID new_rect
    liftIO $ add_top_space_strut (fromIntegral width') (fromIntegral width) display_ID window_ID

add_GHCI_prompt_strut display_ID window_ID = do
    Rectangle _ _ screen_width screen_height <- gets $ screenRect . W.screenDetail . W.current . windowset
    let new_rect = Rectangle 0 0 width' (top_area_height - border_size)
        width' = (fromIntegral screen_width) `div` 2 
    tileWindow window_ID new_rect
    liftIO $ add_top_space_strut 0 (fromIntegral width') display_ID window_ID

pre_manage_hook state_ref = composeAll
    [ className =? "xmonad_GHCITerm" --> do
        w <- ask
        manage_new_prompt state_ref w
    , className =? "xmonad_TopTerm" --> do
        window_ID :: Window <- ask
        display_ID <- liftX $ ask >>= return . display
        doOverrideRedirect True
        is_visible <- liftIO $ readIORef state_ref >>= return . top_area_visible
        liftX $ do
            liftIO $ modifyIORef state_ref $ \state -> state { conky_window = Just window_ID }
            when is_visible $ add_conky_strut display_ID window_ID
        when (not $ is_visible) $ liftX $ do
            liftIO $ do
                selectInput display_ID window_ID (clientMask .&. complement structureNotifyMask)
                unmapWindow display_ID window_ID
                selectInput display_ID window_ID clientMask
            setWMState window_ID iconicState
        when is_visible $ liftIO $ do
            mapWindow display_ID window_ID
        idHook
    , className =? "notify-osd" --> do
        doOverrideRedirect True
    ] 

doOverrideRedirect :: Bool -> ManageHook
doOverrideRedirect b = do
    window_ID :: Window <- ask
    display_ID <- liftX $ ask >>= return . display
    liftIO $ allocaSetWindowAttributes $ \attrs_ptr -> do
        set_override_redirect attrs_ptr b
        changeWindowAttributes display_ID window_ID cWOverrideRedirect attrs_ptr
    idHook

manage_hook state_ref = do
    className >>= (\name -> liftIO $ putStrLn $ "class: " ++ name)
    composeAll
        [ className =? "Pidgin" --> do
            w <- ask
            doF $ W.shiftWin "3" w
        , className =? "update-manager" --> do
            w <- ask
            doF $ W.shiftWin "0" w
        , className =? "xterm-project-root" --> do
            w <- ask
            workspace_ID <- liftX $ get >>= return . W.currentTag . windowset
            let row = row_for_workspace_ID workspace_ID
                new_ID = workspace_ID_for_row_index row 0
            doF $ W.shiftWin new_ID w
        , className =? "xterm-project-src" --> do
            w <- ask
            workspace_ID <- liftX $ get >>= return . W.currentTag . windowset
            let row = row_for_workspace_ID workspace_ID
                new_ID = workspace_ID_for_row_index row 1
            doF $ W.shiftWin new_ID w
        , className =? "xterm-project-test" --> do
            w <- ask
            workspace_ID <- liftX $ get >>= return . W.currentTag . windowset
            let row = row_for_workspace_ID workspace_ID
                new_ID = workspace_ID_for_row_index row 2
            doF $ W.shiftWin new_ID w
        , className =? "Pandora" --> do
            doIgnore
        , className =? "Gimp" --> doFloat
        , className =? "Lt-gimp-2.7" --> doFloat
        , className =? "Gimp-2.7" --> doFloat
        , className =? "Wine" --> doFloat
        , isFullscreen --> do
            -- doF W.focusDown <+> doFullFloat
            doFullFloat
        , manageHook gnomeConfig
        ] <+> manageDocks <+> pre_manage_hook state_ref
    
startup :: IORef DesktopState -> X ()
startup state_ref = do
    gnomeRegister
    spawn_startup_programs
    return ()

spawn_startup_programs :: X ()
spawn_startup_programs = do
    -- startup an xterm dedicated to a GHCI prompt. Use a specific class so we can identify it
    -- later.
    spawn_GHCI
    spawn "xterm +sb -class xmonad_TopTerm -e /bin/sh -lc 'top'"
    return ()

spawn_GHCI :: X ()
spawn_GHCI = do
    spawn "xterm +sb -class xmonad_GHCITerm -e \"ghci $HOME/Development/RCs/ghci_term_prelude.hs ; sleep 100\""
    return ()
    
main = do
    -- Start with some good initial defaults.
    -- Ideally these would be restored from a previous sessions state if xmonad had been restarted.
    -- I don't know how to do that yet, though I bet it's possible. 
    state_ref <- newIORef $ DesktopState 
        {
            -- There are initially no workspaces. This is incorrect as on xmonad restart there will
            -- be workspaces. 
            workspace_prompts = Map.empty,
            previously_focused_window = Nothing,
            top_area_visible = False,
            conky_window = Nothing
        }
    -- I always use a Mac keyboard. They are usually pretty nice and have an additional modifier key
    -- beyond alt/option and control. Which I think is required for good interaction with the window
    -- manager.
    let modMask = mod4Mask
    xmonad $ gnomeConfig
        {
            workspaces = workspace_IDs,
            layoutHook = desktopLayoutModifiers $ smartBorders 
                         ( 
                           onWorkspace "2" gimpLayout
                           $ onWorkspace "3" fullscreenLayout
                           $ avoidStruts $ Full ||| Tall 1 (5/100) (1/2)
                         )
                         ,
            borderWidth = border_size,
            keys = keybindings state_ref,
            modMask = modMask,
            manageHook = manage_hook state_ref,
            --preManageHook = pre_manage_hook state_ref,
            startupHook = startup state_ref,
            mouseBindings = mouse_bindings
        }

gimpLayout = withIM (0.11) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full
fullscreenLayout = noBorders Full

resize_delta = 10

expand_window :: Window -> X ()
expand_window w = whenX (isClient w) $ withDisplay $ \d -> do
    Rectangle sx sy sw sh <- gets $ screenRect . W.screenDetail . W.current . windowset
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    io $ moveWindow d w (fromIntegral $ (sw - (fromIntegral $ wa_width wa)) `div` 2)
                        (fromIntegral $ (sh - (fromIntegral $ wa_height wa)) `div` 2)
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa) `div` 2) 
                                      (fromIntegral (wa_height wa) `div` 2)
    sh <- io $ getWMNormalHints d w
    io $ do
        putStrLn "expanding"
        resizeWindow d w (fromIntegral $ wa_width wa + resize_delta)
                         (fromIntegral $ wa_height wa + resize_delta)
    float w

shrink_window :: Window -> X ()
shrink_window w = whenX (isClient w) $ withDisplay $ \d -> do
    Rectangle sx sy sw sh <- gets $ screenRect . W.screenDetail . W.current . windowset
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    io $ moveWindow d w (fromIntegral $ (sw - (fromIntegral $ wa_width wa)) `div` 2)
                        (fromIntegral $ (sh - (fromIntegral $ wa_height wa)) `div` 2)
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa) `div` 2) 
                                      (fromIntegral (wa_height wa) `div` 2)
    sh <- io $ getWMNormalHints d w
    io $ do
        putStrLn "shrinking"
        resizeWindow d w (fromIntegral $ wa_width wa - resize_delta)
                         (fromIntegral $ wa_height wa - resize_delta)
    float w

mouse_bindings :: XConfig Layout -> Map (KeyMask, Button) (Window -> X ())
mouse_bindings (XConfig {XMonad.modMask = modMask}) = Map.fromList $
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1)              , (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
     ,((modMask, button4)              , (\w -> focus w >> expand_window w
                                          >> windows W.shiftMaster))
     ,((modMask, button5)              , (\w -> focus w >> shrink_window w
                                          >> windows W.shiftMaster))
     ,((modMask .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    ]
