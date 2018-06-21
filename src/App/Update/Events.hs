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

pattern MousePressEvent :: Num a => SDL.MouseButton -> V2 a -> SDL.Event
pattern MousePressEvent button pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent
      ( SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Pressed
        , SDL.mouseButtonEventButton = button
        , SDL.mouseButtonEventPos = SDL.P (fmap fromIntegral -> pos)
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

pattern MouseMotionEvent :: Num a => V2 a -> SDL.Event
pattern MouseMotionEvent relMotion <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseMotionEvent
      ( SDL.MouseMotionEventData
        { SDL.mouseMotionEventRelMotion = fmap fromIntegral -> relMotion }
      )
    }
pattern MouseWheelEvent :: Num a => a -> SDL.Event
pattern MouseWheelEvent amount <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseWheelEvent
      ( SDL.MouseWheelEventData
        { SDL.mouseWheelEventPos = view _y >>> signum >>> fromIntegral -> amount }
      )
    }