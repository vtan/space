module App.Update.Events where

import App.Prelude

import qualified SDL

pattern QuitEvent :: SDL.Event
pattern QuitEvent <-
  SDL.Event { SDL.eventPayload = SDL.QuitEvent }

pattern KeyPressEvent :: SDL.Scancode -> SDL.Event
pattern KeyPressEvent scancode <-
  SDL.Event 
    { SDL.eventPayload = SDL.KeyboardEvent 
      ( SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scancode }
        }
      )
    }

pattern MouseReleaseEvent :: Num a => V2 a -> SDL.Event
pattern MouseReleaseEvent pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent
      ( SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Released
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (fmap fromIntegral -> pos)
        }
      )
    }
