-- Same as the example from the README.
-- I'm working from the branch:
--  https://github.com/coreyoconnor/osxmonad

import XMonad
import OSXMonad.Core

main = osxmonad defaultConfig {
         modMask = mod1Mask .|. mod4Mask,
         keys = osxKeys
       }

