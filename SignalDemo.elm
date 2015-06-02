module SignalDemo (signalDemo,mouseClick,fpsTest,countTime,clickCount,mouseIsDown) where

import View exposing (..)
import Mouse exposing (clicks,isDown)
import Signal exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (down,Element)
import Keyboard


mouseClick : Signal ()
mouseClick = Mouse.clicks

trueClick : Signal Bool
trueClick = Signal.map (always True) Mouse.clicks

mapDemo : Signal Element
mapDemo =
  signalFlow down [
                functionName "map : (a -> result) -> Signal a -> Signal result"
                ,description "Apply a function to a signal."
                ,basicCatalog mouseClick "Mouse.clicks"
                ,customCatalog trueClick
                           (\x -> if | x == "True" -> " T"
                                     | otherwise -> x)
                           "trueClick = Signal.map (always True) Mouse.clicks"
                           ]

countTime = Signal.foldp (\x y -> y + 1 ) 0 (every second)
map2Test = Signal.map2 (,) mouseClick countTime

map2Demo : Signal Element
map2Demo =
        line3
              "map2 : (a -> b -> result) -> Signal a -> Signal b -> Signal result"
              "Mouse.clicks"
              "countTime = Signal.foldp (\\x y -> y + 1 ) 0 (every second)"
              "Signal.map2 (,) mouseClick countTime"
              mouseClick
              countTime
              map2Test

clickCount = Signal.foldp (\x y -> y + 1 ) 0 mouseClick
foldpDemo =
         line2
           "foldp : (a -> state -> state) -> state -> Signal a -> Signal state"
           "Mouse.clicks"
           "clickCount = Signal.foldp (\\x y -> y + 1 ) 0 mouseClick"
            mouseClick
            clickCount


mouseIsDown = Mouse.isDown
mergeTest = Signal.merge Mouse.isDown Keyboard.enter
mergeDemo = line3
              "merge : Signal a -> Signal a -> Signal a"
              "Mouse.isDown"
              "Keyboard.enter"
              "Signal.merge Mouse.isDown Keyboard.enter"
              mouseIsDown
              Keyboard.enter
              mergeTest

multiplesOf3 = Signal.filter (\x -> if x % 3 == 0 then True else False ) 6 clickCount
filterDemo = line2
                 "filter : (a -> Bool) -> a -> Signal a -> Signal a"
                 "clickCount = Signal.foldp (\\x y -> y + 1 ) 0 Mouse.clicks"
                 "multiplesOf3 = Signal.filter (\\x -> if x % 3 == 0 then True else False ) 6 clickCount"
                 clickCount
                 multiplesOf3

sinceTest = Time.since (1 * second) mouseClick
droTest = Signal.dropRepeats sinceTest
dropRepeatsDemo = line2
                      "dropRepeats : Signal a -> Signal a"
                      "Time.since (1 * second) Mouse.clicks"
                      "Signal.dropRepeats sinceTest"
                      sinceTest
                      droTest

fpsTest = fps 1
sampleOnSignal = Signal.sampleOn trueClick fpsTest
sampleOnDemo = line3
                 "sampleOn : Signal a -> Signal b -> Signal b"
                 "trueClick"
                 "fps 1"
                 "Signal.sampleOn trueClick (fps 1 )"
                 trueClick
                 fpsTest
                 sampleOnSignal

signalDemo =            
    signalFlow down [ title "Signal"
                        , demoList [ mapDemo
                                  , map2Demo
                                  , foldpDemo
                                  ,mergeDemo
                                  , filterDemo
                                  , dropRepeatsDemo
                                  , sampleOnDemo ]
                                  ]