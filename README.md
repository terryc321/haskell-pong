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
  build-depends:    base ^>=4.18.3.0 ,
                    sdl2

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


