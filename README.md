# Pong

ghcup installed version of cabal 

```
> cabal update 
```
lets make the pong directory files required , a blank haskell project. we do this using 
```
> cabal init pong 
```

now first thing is i need to get access to SDL libraries for haskell , we add sdl2 with a 
comma separated build-depends 

```
pong.cabal 

executable pong
  -- 
  build-depends:    base ^>=4.18.3.0,
                    sdl2, 
					linear 

```

when we try to build the empty project that just says hello 

```
> cabal build 
```

it will tell us it is downloading and installing required libraries for sdl2 , this will take some time .

next we can run 

```
> cabal run
Hello, Haskell!
```

we can see it prints hello haskell to console .

if copy program on [hoogle sdl2](https://hackage.haskell.org/package/sdl2-2.5.5.0/docs/SDL.html) . 

this now becomes our new app/Main.hs 

```
{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless qPressed (appLoop renderer)
```

can we now get some user interaction to move something up and down the screen. lets see what SDL has to offer 
inside ghci we can see all exported functions 

```
:browse SDL 
```

indeed there are a lot. 

here is some emacs code to load some icons , to distract us from the complexity 

```
(use-package all-the-icons
  :ensure t
  :init
  (setq all-the-icons-icon-scale 1) ;; Optional: adjust size
  :custom
  (all-the-icons-install-fonts t)) ;; This prompts to install fonts on first load
```

Here is the first version which responds to W A S D keys and UP DOWN LEFT RIGHT keys 

[pong version 1](pictures/screenshot-2026-06-17_01-43.png)

the problem is on every new frame a keyboard state is taken and if W key is pressed the 
rendered box is moved one pixel left . 
this all happens far too fast for the human eye to detect what happended , so it looks as though the 
box jumps from one side of screen to other. 

so we need to figure out how we can control this .

haskell ffi sdl is quite flakey if get one wrong value instead of foreign C Int there is a crash, all see is the word loop , meaning the thing crashed completely somewhere in the game loop 





