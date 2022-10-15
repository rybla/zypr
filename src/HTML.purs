module HTML where

import Prelude
import Effect (Effect)
import Web.HTML (Navigator)

foreign import navigator_clipboard_writeText :: String -> Effect Unit
