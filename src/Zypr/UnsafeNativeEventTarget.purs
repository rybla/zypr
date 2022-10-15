module Zypr.UnsafeNativeEventTarget where

import Prelude
import Effect (Effect)
import React.SyntheticEvent (NativeEventTarget)

foreign import set :: forall a. String -> a -> NativeEventTarget -> Effect Unit

foreign import classList_add :: String -> NativeEventTarget -> Effect Unit

foreign import classList_remove :: String -> NativeEventTarget -> Effect Unit

foreign import classList_toggle :: String -> NativeEventTarget -> Effect Unit
