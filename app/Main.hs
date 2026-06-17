
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}


module Main where

import System.Clock (Clock(..), TimeSpec(..) , getTime, toNanoSecs , diffTimeSpec , getRes )
import Data.IORef 

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
-- import Data.Time.Clock (getCurrentTime, diffUTCTime)
-- import qualified SDL


data GameState = GameState
  { pixelX :: CInt ,
    pixelY :: CInt ,
    positionX :: Double ,
    positionY :: Double , 
    velocityX :: Double ,
    velocityY :: Double }


freshGameState :: GameState
freshGameState = GameState { pixelX = 100 :: CInt ,
                             pixelY = 100 :: CInt ,
                             positionX = 100.0 :: Double ,
                             positionY = 100.0 :: Double ,
                             velocityX = -40 :: Double ,
                             velocityY = -50 :: Double }

windowWidth = 1024
windowHeight = 768

boxWidth = 80
boxHeight = 80 


-- how do we detect if window has changed size ? 
initialWindow = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.Windowed
  , SDL.windowGraphicsContext = SDL.NoGraphicsContext
  , SDL.windowPosition        = SDL.Wherever
  , SDL.windowResizable       = True
  , SDL.windowInitialSize     = SDL.V2 windowWidth windowHeight
  , SDL.windowVisible         = True
  }

{-- 
sdl2 createWindow  
WindowConfig	 
windowBorder :: Bool	
Defaults to True.

windowHighDPI :: Bool	
Defaults to False. Can not be changed after window creation.

windowInputGrabbed :: Bool	
Defaults to False. Whether the mouse shall be confined to the window.

windowMode :: WindowMode	
Defaults to Windowed.

windowGraphicsContext :: WindowGraphicsContext	
Defaults to NoGraphicsContext. Can not be changed after window creation.

windowPosition :: WindowPosition	
Defaults to Wherever.

windowResizable :: Bool	
Defaults to False. Whether the window can be resized by the user. It is still possible to programatically change the size by changing windowSize.

windowInitialSize :: V2 CInt	
Defaults to (800, 600). If you set windowHighDPI flag, window size in screen coordinates may differ from the size in pixels. Use glGetDrawableSize or vkGetDrawableSize to get size in pixels.

windowVisible :: Bool	
Defaults to True.
--}

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
  window <- SDL.createWindow "My SDL Application" initialWindow 
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  -- prevTime <- getCurrentTime
  let nframe = 0
  now <- getTime Monotonic
  appLoop renderer freshGameState now
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
isWasdPressed :: IO KeyboardState 
isWasdPressed = do
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

accelDelta :: Double 
accelDelta = 0.01

-- mutVelocityX :: IO (IORef Double)
-- mutVelocityX = newIORef 100.0
-- mutVelocityY :: IO (IORef Double)
-- mutVelocityY = newIORef 100.0

-- incFrame :: Double -> IO Double
-- incFrame ref = do  vel <- readIORef ref
--                    if vel < 0 then return (0 - vel)
--                    else return vel

incFrame kbd n 
  | wPressed kbd && n < 0 = n - accelDelta
  | wPressed kbd && n > 0 = n + accelDelta
  | sPressed kbd && n < 0 = n + accelDelta
  | sPressed kbd && n > 0 = n - accelDelta
  | otherwise  = n


{--
GameState { pixelX :: CInt ,
    pixelY :: CInt ,
    positionX :: Double ,
    positionY :: Double , 
    velocityX :: Double ,
    velocityY :: Double } =
--}
bounce :: KeyboardState -> GameState -> Double -> IO GameState
bounce kbd game secPerFrame =  
  let maxx = fromIntegral (windowWidth - boxWidth)
      maxy = fromIntegral (windowHeight - boxHeight)
      vx = velocityX game
      vy = velocityY game
      px = positionX game 
      py = positionY game      
  in let px2 = px + (velocityX game) * secPerFrame
         py2 = py + (velocityY game) * secPerFrame
     in do
    let velx = incFrame kbd vx 
        vely = incFrame kbd vy 
    let game2 = game {pixelX = floor px2 , pixelY = floor py2 , positionX = px2 , positionY = py2 , velocityX = velx , velocityY = vely }
    let game3 = if positionY game2 > maxy then game2 { pixelY = floor maxy , positionY = maxy ,  velocityY = negate vy + 1 } else game2
    let game4 = if positionX game3 > maxx then game3 { pixelX = floor maxx , positionX = maxx , velocityX = negate vx + 1 } else game3
    let game5 = if positionY game4 < 0 then game4 { pixelY = floor 0.0 , positionY = 0.0 , velocityY = negate vy + 1 } else game4
    let game6 = if positionX game5 < 0 then game5 { pixelX = floor 0.0 , positionX = 0.0 , velocityX = negate vx + 1 } else game5
    {--
    putStrLn $ "vx = " ++ show (velocityX game6) ++ " vy = " ++ show (velocityY game6)
    if (abs (velocityX game6)) < 50.0 then putStrLn $ "velocity X failed : vx = " ++ show (velocityX game6) ++ " vy = " ++ show (velocityY game6) else return ()
    if (abs (velocityY game6)) < 50.0 then putStrLn $ "velocity Y failed : vx = " ++ show (velocityX game6) ++ " vy = " ++ show (velocityY game6) else return ()
    --}
    return game6 


{--           
in let px2 = if px < 0 then 0 else if px > 400 then 400 else px :: Double 
            py2 = if py < 0 then 0 else if py > 400 then 400 else py :: Double 
            vx2 = if px < 0 || px > 400 then (- vx) else vx :: Double 
            vy2 = if py < 0 || py > 400 then (- vy) else vy :: Double 
--}              



 
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
appLoop :: SDL.Renderer -> GameState -> TimeSpec -> IO ()
appLoop renderer game now = do
  -- currentTime <- getCurrentTime
  -- let dt = realToFrac (diffUTCTime currentTime prevTime) :: Double
  now2 <- getTime Monotonic
  let nanos = toNanoSecs (diffTimeSpec now now2)
  let secondsPerFrame = fromIntegral nanos / 1000000000 :: Double 
  let fps = 1 / secondsPerFrame :: Double
  -- let fpsRounded = realToFrac (round (fps * 100) :: Integer) / 100 :: Double
  
  events <- SDL.pollEvents
  kb <- isWasdPressed
  
  -- let delta_y = if wPressed kb && sPressed kb then 0
  --          else if wPressed kb then (-1)
  --               else if sPressed kb then (1)
  --                    else 0
  --     delta_x = if aPressed kb && dPressed kb then 0
  --          else if aPressed kb then (-1)
  --               else if dPressed kb then (1)
  --                    else 0
  {-- 
  let game = let dx = positionX game + (velocityX game * secondsPerFrame)   
              in if dx < 0 || dx > 400 then game { velocityX = (- (velocityX game)) }
                 else game
                      
  let game = let dy = positionY game + (velocityY game * secondsPerFrame)   
              in if dy < 0 || dy > 400 then game { velocityY = (- (velocityY game)) }
                 else game
  --}
        
  -- --- what ai calls clamping to keep within reasonable screen dimensions 
  -- let posX  = let dx = positionX game + (velocityX game * secondsPerFrame)   
  --             in if dx < 0 then 0 else if dx > 400 then 400 else dx
  -- let posY  = let dy = positionY game + (velocityY game * secondsPerFrame)   
  --             in if dy < 0 then 0 else if dy > 400 then 400 else dy

  
  -- velocity is tied up with game state 
  -- let velocityX = if posX < 0 then (- velocityX) else velocityX
  -- let velocityY = if posY < 0 then (- velocityY) else velocityY
  
  -- putStrLn $ "posX = " ++ show posX ++ " posY = " ++ show posY
  
  -- let pixX  = fromIntegral (floor posX) :: CInt
  -- let pixY  = fromIntegral (floor posY) :: CInt

  -- let velX = velocityX game :: Double
  -- let velY = velocityY game :: Double
  
  -- let nframe = if nframe > 1000 then 0 else nframe 
  -- let game = if nframe == 0 then GameState { boxX = newX , boxY = newY } else game
  -- let game = GameState { pixelX = pixX , pixelY = pixY , positionX = posX , positionY = posY ,
  --                        velocityX = velX , velocityY = velY } 
  let dx = pixelX game
  let dy = pixelY game 
  -- putStrLn $ "nanos = " ++ show nanos ++ " fps = " ++ show fps ++ " fpsRounded = " ++ show fpsRounded
  -- putStrLn $ "dx = " ++ show dx ++ " dy = " ++ show dy

  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  --- SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255  -- r0 g0 b0 a255 is black
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255  
  SDL.clear renderer
  -- for the box color 
  -- SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255 -- blue
  SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255 -- white all 255 
  
  let pos = SDL.V2 (dx::CInt) (dy::CInt) 
  let myRect = SDL.Rectangle (SDL.P pos) (SDL.V2 boxWidth boxHeight) -- 50 50
  SDL.fillRect renderer (Just myRect)
  SDL.rendererDrawColor renderer $= SDL.V4 125 0 0 50
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 (dx + 20) (dy + 20))) (SDL.V2 10 10)))
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 (dx + boxWidth - (3*10)) (dy + 20))) (SDL.V2 10 10)))
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (SDL.V2 (dx + 10) (dy + boxHeight - 30))) (SDL.V2 (boxWidth - 20) 20)))
  -- draw it
  SDL.present renderer
  -- next frame 
  game2 <- bounce kb game secondsPerFrame  
  unless qPressed (appLoop renderer game2 now2 ) -- (bounce game secondsPerFrame) now2)

{--
emacs C-c C-k clears interactive repl buffer
M-x lsp -- starts emacs lsp mode with haskell , sometimes hit and miss 
--}

