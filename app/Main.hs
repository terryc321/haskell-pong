
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- imported infix symbols need to be placed inside extra set of parens 
import qualified SDL as SDL
import SDL (($=))

import Foreign.C.Types (CInt)


import qualified Debug.Trace as Trace 

import Data.Set (Set)
import qualified Data.Set as Set

-- we can see all details ever wanted to know for SDL using :browse SDL 

-- import Linear (V4(..) , V2(..))
import Control.Monad (unless)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified SDL


data GameState = GameState
  { boxX :: CInt ,
    boxY :: CInt ,
    boxXCt :: Double ,
    boxYCt :: Double }


freshGameState :: GameState
freshGameState = GameState { boxX = 100 :: CInt ,
                             boxY = 100 :: CInt ,
                             boxXCt = 0.0 :: Double ,
                             boxYCt = 0.0 :: Double }


{-- 
-- Speed in pixels per second
batSpeed :: Double
batSpeed = 500.0

update :: Double -> [SDL.Scancode] -> GameState -> GameState
update dt keys state = 
  let move = (if SDL.ScancodeW `elem` keys then -1 else 0) + 
             (if SDL.ScancodeS `elem` keys then 1 else 0)
      delta = fromIntegral move * batSpeed * dt
      newY = batY state + delta
      clampedY = max 0 (min (screenHeight state - batHeight state) newY)
  in state { batY = clampedY }
--}

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  prevTime <- getCurrentTime
  let nframe = 0 
  appLoop renderer freshGameState nframe 
  SDL.destroyWindow window


data KeyboardState = KeyboardState
  { wPressed :: Bool
  , aPressed :: Bool
  , sPressed :: Bool
  , dPressed :: Bool
  , upPressed :: Bool
  , downPressed :: Bool
  , leftPressed :: Bool
  , rightPressed :: Bool
  , escapePressed :: Bool 
  } deriving (Show, Eq)

-- Helper to create a default "no keys pressed" state
emptyKeyboard :: KeyboardState
emptyKeyboard = KeyboardState False False False False False False False False False 


-- Function to check if W is pressed
isWPressed :: IO KeyboardState 
isWPressed = do
    -- Get the current state of all keys
    -- Returns a Set Scancode
    keys <- SDL.getKeyboardState
--  let wpressed = keys KeycodeW
--  whats difference between ScancodeW and KeycodeW ? 
    let wasd = KeyboardState {
      wPressed = keys SDL.ScancodeW
      ,aPressed = keys SDL.ScancodeA
      ,sPressed = keys SDL.ScancodeS
      ,dPressed = keys SDL.ScancodeD
      ,upPressed = keys SDL.ScancodeUp
      ,downPressed = keys SDL.ScancodeDown
      ,leftPressed = keys SDL.ScancodeLeft
      ,rightPressed = keys SDL.ScancodeRight
      ,escapePressed = keys SDL.ScancodeEscape
      }
    -- Trace.traceM ("pressed: " ++ show wasd)
    return wasd
    
 
{--
1 , open a window on screen and draw something ! 
2 , put a box on the screen - achieved ! 
3 , move a box around screen - achieved !
  moves extremely fast - too fast - how can we monitor it ?
  if the frames per second is extremely fast - adding 1 pixel will make it fly off screen
  if the frames per second is very slow - laggy  - adding 1 pixel will make it move very slow 
  how do we make a sweet spot where we can determine fps and thereby compute accurate time required 

4 , have an accurate time clock nanosecond or microsecond , however vague need it
 wall clock or what ? cpu time ?

5 , start lsp mode in emacs M-x lsp
6 , 
--}
appLoop :: SDL.Renderer -> GameState -> Integer -> IO ()
appLoop renderer game nframe = do
  currentTime <- getCurrentTime
  -- let dt = realToFrac (diffUTCTime currentTime prevTime) :: Double
  
  events <- SDL.pollEvents
  kb <- isWPressed
  
  let delta_y = if wPressed kb && sPressed kb then 0
           else if wPressed kb then (-1)
                else if sPressed kb then (1)
                     else 0
      delta_x = if aPressed kb && dPressed kb then 0
           else if aPressed kb then (-1)
                else if dPressed kb then (1)
                     else 0
  --- what ai calls clamping to keep within reasonable screen dimensions 
  let newX = let dx = boxX game + delta_x
             in if dx < 0 then 0 else if dx > 400 then 400 else dx 
  let newY = let dy = boxY game + delta_y
             in if dy < 0 then 0 else if dy > 400 then 400 else dy 

  -- let nframe = if nframe > 1000 then 0 else nframe 
  -- let game = if nframe == 0 then GameState { boxX = newX , boxY = newY } else game
  let game = GameState { boxX = newX , boxY = newY } 
  let dx = boxX game
  let dy = boxY game 

  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  SDL.rendererDrawColor renderer $= SDL.V4 255 0 255 255  
  SDL.clear renderer
  -- for the box color 
  SDL.rendererDrawColor renderer $= SDL.V4 0 255 0 255
  let pos = SDL.V2 (dx::CInt) (dy::CInt) 
  -- Define the rectangle (Position V2 x y, Size V2 width height)
  -- Example: x=100, y=100, width=50, height=50
  -- wow ! SDL.P took ages to work out !! only needed (Point V2 a) (V2 a)
  let myRect = SDL.Rectangle (SDL.P pos) (SDL.V2 50 50) -- 50 50
  -- -- -- 4. Draw the filled rectangle -- why is it just myRect ??
  SDL.fillRect renderer (Just myRect)
  SDL.drawLine renderer (SDL.P (SDL.V2 dx dy)) (SDL.P (SDL.V2 100 100))
  
  SDL.present renderer
  unless qPressed (appLoop renderer game (nframe + 1))

{--
emacs C-c C-k clears interactive repl buffer
M-x lsp -- starts emacs lsp mode with haskell , sometimes hit and miss 
--}

