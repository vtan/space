module App.Update.Events where

import App.Prelude

import qualified SDL
import qualified SDL.Internal.Numbered

import Data.Bits ((.&.))
import Data.Int (Int32)

pattern QuitEvent :: SDL.Event
pattern QuitEvent <-
  SDL.Event { SDL.eventPayload = SDL.QuitEvent }

pattern KeyPressEvent :: SDL.Scancode -> SDL.Event
pattern KeyPressEvent scancode <-
  SDL.Event 
    { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
      { SDL.keyboardEventKeyMotion = SDL.Pressed
      , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scancode }
      }
    }

pattern MousePressEvent :: SDL.MouseButton -> V2 Int32 -> SDL.Event
pattern MousePressEvent button pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent SDL.MouseButtonEventData
      { SDL.mouseButtonEventMotion = SDL.Pressed
      , SDL.mouseButtonEventButton = button
      , SDL.mouseButtonEventPos = SDL.P pos
      }
    }

pattern MouseReleaseEvent :: V2 Int32 -> SDL.Event
pattern MouseReleaseEvent pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent SDL.MouseButtonEventData
      { SDL.mouseButtonEventMotion = SDL.Released
      , SDL.mouseButtonEventButton = SDL.ButtonLeft
      , SDL.mouseButtonEventPos = SDL.P pos
      }
    }

pattern MouseMotionEvent :: V2 Int32 -> SDL.Event
pattern MouseMotionEvent relMotion <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseMotionEvent SDL.MouseMotionEventData
      { SDL.mouseMotionEventRelMotion = relMotion }
    }

pattern MouseWheelEvent :: Int32 -> SDL.Event
pattern MouseWheelEvent amount <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseWheelEvent SDL.MouseWheelEventData
      { SDL.mouseWheelEventPos = view _y >>> signum -> amount }
    }

pattern TextInputEvent :: Text -> SDL.Event
pattern TextInputEvent text <-
  SDL.Event
    { SDL.eventPayload = SDL.TextInputEvent SDL.TextInputEventData 
      { SDL.textInputEventText = text }
    }

isUnicodeKeyEvent :: SDL.Event -> Bool
isUnicodeKeyEvent = \case
  SDL.Event 
    { SDL.eventPayload = SDL.KeyboardEvent SDL.KeyboardEventData
      { SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymKeycode = keycode } -- TODO check for modifier keys
      }
    } -> SDL.Internal.Numbered.toNumber keycode .&. unicodeMask == 0
  _ -> False
  where
    unicodeMask = 0x40000000