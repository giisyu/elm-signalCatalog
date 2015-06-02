module TimeDemo (timeDemo) where


import View exposing (..)
import SignalDemo exposing (..)
import Signal exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (down,Element)
import Keyboard


fpsDemo = line1
            "fps : number -> Signal Time"
            "fps 1"
            fpsTest

fpsWhenTest = Time.fpsWhen 5 mouseIsDown
fpsWhenDemo = line2
                         "fpsWhen : number -> Signal Bool -> Signal Time"
                         "mouseIsDown = Mouse.isDown"
                          "Time.fpsWhen 5 mouseIsDown"
                          mouseIsDown
                          fpsWhenTest

everyTest = Time.every (1 * second)
everyDemo = line1
                         "every : Time -> Signal Time"
                         "Time.every (1 * second)"
                         everyTest

timstam = Time.timestamp mouseClick
timestampDemo = line2
                      "timestamp : Signal a -> Signal (Time, a)"
                      "mouseClick = Mouse.clicks"
                      "Time.timestamp mouseClick"
                      mouseClick
                      timstam


delayTest = delay (500* millisecond) mouseClick
delayDemo = line2
              "delay : Time -> Signal a -> Signal a"
              "mouseClick = Mouse.clicks"
              "delay (500 * millisecond) mouseClick"
              mouseClick
              delayTest


sinceTest = Time.since (1 * second) mouseClick
sinceDemo =  line2
                           "since : Time -> Signal a -> Signal Bool"
                           "mouseClick = Mouse.clicks"
                           "Time.since (1 * second) mouseClick"
                           mouseClick
                           sinceTest
timeDemo = 
       signalFlow down [
                         title "Time"
                              , demoList [
                                        fpsDemo
                                      ,fpsWhenDemo
                                      , everyDemo
                                      , timestampDemo
                                      , delayDemo
                                      , sinceDemo]
                        ]