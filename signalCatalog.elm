import Html exposing (text,div)
import Html.Attributes exposing (style)
import Signal exposing (..)
import Task exposing (..)
import List
import Time exposing (..)
import Signal.Extra as Extra exposing ((~>))
import Signal.Discrete as Discrete
import Signal.Stream as Stream
import Signal.Time as ExTime
import Keyboard
import View exposing (..)
import Graphics.Element exposing (down,Element)
import String
import Mouse exposing (clicks,isDown)


import SignalDemo exposing(..)


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

--discrete
esTest = Discrete.es (every second)
esDemo =  signalFlow down [
                    functionName "es : Signal a -> EventSource"
                    ,customCatalog (every second) (\x -> String.left 3 x ) "every second"
                    ,basicCatalog esTest "Discrete.es (every second)"
                     ]

equalTest = Discrete.whenEqual True Mouse.isDown
whenEqualDemo = signalFlow down [
  functionName "whenEqual : a -> Signal a -> EventSource"
  ,customCatalog mouseIsDown (\x -> if x == "True" then "T" else "F") "Mouse.isDown"
  ,basicCatalog equalTest "Discrete.whenEqual True Mouse.isDown"]

sinceTest2 = Time.since (500 * millisecond) mouseClick
whenChangeTest = Discrete.whenChange sinceTest2
whenChangeDemo = line2
                    "whenChange : Signal a -> EventSource"
                    "sinceTest2 = Time.since (500 * millisecond) mouseClick"
                    "Discrete.whenChange sinceTest2"
                    sinceTest2
                    whenChangeTest


whenChangeToTest = Discrete.whenChangeTo True Keyboard.enter
whenChangeToDemo = line2
                      "whenChangeTo: a -> Signal a -> EventSource"
                      "Keyboard.enter"
                      "Discrete.whenChangeTo True Keyboard.enter"
                      Keyboard.enter
                      whenChangeToTest

enter = Discrete.whenChangeTo True Keyboard.enter
toggleOnEnter = Discrete.folde not False enter
foldeDemo = line2
                "folde : (b -> b) -> b -> EventSource -> Signal b"
                "enter = Discrete.whenChangeTo True Keyboard.enter"
                "toggleOnEnter = Discrete.folde not False enter"
                enter
                toggleOnEnter

sampleTest = Stream.sample (,) Keyboard.arrows (Stream.fromSignal <| fps 1)
sampleDemo = line1
  "sample : (a -> b -> c) -> Signal a -> Stream b -> Stream c"
  "Stream.sample (,) Keyboard.arrows (Stream.fromSignal <| fps 1)"
  sampleTest

---extra

zipTest = Extra.zip Mouse.x Mouse.y
zipDemo =  line1
              "zip : Signal a -> Signal b -> Signal (a,b)"
              "Extra.zip Mouse.x Mouse.y "
              zipTest

count signal = foldp (\s a -> a + 1) 0 signal
run = Extra.runBuffer 5 (count (Time.every second))
runlist = ((==) [1,2,3,4,5]) <~ Extra.runBuffer 5 (count (Time.every second))
runBufferDemo = line2
                  "runBuffer : Int -> Signal a -> Signal (List a)"
                  "runBuffer 5 (count (Time.every second))"
                  "((==) [1,2,3,4,5]) <~ runBuffer 5 (count (Time.every second))"
                  run
                  runlist

derayRTest = Extra.delayRound 0 clickCount
delayRoundDemo = line2
                  "delayRound : b -> Signal b -> Signal b"
                  "clickCount"
                  "Extra.delayRound 0 clickCount"
                  clickCount
                  derayRTest


switchWhenTest = Extra.switchWhen mouseIsDown (fps 2) (every second)
fps2 = fps 2
switchWhenDemo = line3
                    "switchWhen : Signal Bool -> Signal a -> Signal a -> Signal a"
                    "Mouse.isDown"
                    "fps 2"
                    "Extra.switchWhen mouseIsDown (fps 2) (every second)"
                    mouseIsDown
                    fps2
                    switchWhenTest

switchSampleTest = Extra.switchSample mouseIsDown (fps 2) (every second)
switchSampleDemo = line3
                    "switchSample : Signal Bool -> Signal a -> Signal a -> Signal a"
                    "fps 2"
                    "every second"
                    "Extra.switchWhen mouseIsDown (fps 2) (every second)"
                    fps2
                    (every second)
                    switchSampleTest
keepWhenTest = Extra.keepWhen mouseIsDown 0 (fps 1)
keepWhenDemo = line3
                  "keepWhen : Signal Bool -> a -> Signal a -> Signal a"
                  "mouseIsDown"
                  "fps 1"
                  "Extra.keepWhen mouseIsDown 0 (fps 1) "
                  mouseIsDown
                  (fps 1)
                  keepWhenTest

sampleWhenTest = Extra.sampleWhen mouseIsDown 0 (fps 1)
sampleWhenDemo = line3
                  "sampleWhen : Signal Bool -> a -> Signal a -> Signal a"
                  "mouseIsDown"
                  "fps 1"
                  "Extra.sampleWhen mouseIsDown 0 (fps 1) "
                  mouseIsDown
                  fpsTest
                  sampleWhenTest

keepThenTest = Extra.keepThen mouseIsDown 0 (fps 1)
keepThenDemo = line3
                  "keepThen : Signal Bool -> a -> Signal a -> Signal a"
                  "mouseIsDown"
                  "fps 1"
                  "Extra.keepThen mouseIsDown 0 (fps 1) "
                  mouseIsDown
                  fpsTest
                  keepThenTest

keepWhenITest = Extra.keepWhenI mouseIsDown (fps 1)
keepWhenIDemo = line3
                    "keepWhenI : Signal Bool -> Signal a -> Signal a"
                    "Mouse.isDown"
                    "fps 1"
                    "Extra.keepWhenI mouseIsDown (fps 1)"
                    mouseIsDown
                    fpsTest
                    keepWhenITest



----Signal.Time
throttledMouseClicks = ExTime.limitRate (2 * second) Mouse.clicks
limitRateDemo = line2
                  "limitRate : Time -> Signal a -> Signal a"
                  "Mouse.clicks"
                  "throttledMouseClicks = limitRate (2 * second) Mouse.clicks"
                  mouseClick
                  throttledMouseClicks

noDoubleClicks = ExTime.dropWithin (300 * millisecond) Mouse.clicks
dropWithinDemo = line2
                    "dropWithin : Time -> Signal a -> Signal a"
                    "Mouse.clicks"
                    "noDoubleClicks = ExTime.dropWithin (300 * millisecond) Mouse.clicks"
                    mouseClick
                    noDoubleClicks


mousePosition = Mouse.position

after = ExTime.settledAfter (500 * Time.millisecond) mousePosition

tooltip =
      merge (always False <~ Mouse.position)
            (always True <~ (Mouse.position
                            |> ExTime.settledAfter (500 * Time.millisecond)))

settledAfterDemo =
                 line3
                        "settledAfter : Time -> Signal a -> Signal a"
                        "Mouse.position"
                        "ExTime.settledAfter (500 * Time.millisecond) mousePosition"
                        ("tooltip =  merge (always False <~ Mouse.position)\n" ++
                         "                 (always True <~ (Mouse.position\n" ++
                         "                 |> ExTime.settledAfter (500 * Time.millisecond)))")
                        mousePosition
                        after
                        tooltip

start = ExTime.startTime
startTimeDemo = line1
                    "startTime : Signal Time"
                    "startTime"
                    start


relativeTest = ExTime.relativeTime (Time.every second)
tickcount =  ExTime.relativeTime (Time.every second) ~> Time.inSeconds >> round

relativeTimeDemo = line2
                      "relativeTime : Signal Time -> Signal Time"
                      "relativeTime (Time.every second)"
                      "relativeTime (Time.every second) ~> Time.inSeconds >> round"
                      relativeTest
                      tickcount
sendLine1 = Signal.mailbox 0
sendLine2 = Signal.mailbox 0

port sendCount1 : Signal (Task String ())
port sendCount1 = Signal.map (\x -> case x of
                                  Just a ->Signal.send sendLine1.address a
                                  Nothing -> Task.fail "") (Stream.fromSignal (clickCount))
port sendCount2 : Signal (Task String ())
port sendCount2 = Signal.map (\x -> case x of
                                  Just a ->Task.andThen ( Signal.send sendLine2.address a) (\x -> Task.sleep second)
                                  Nothing -> Task.fail "") (Stream.fromSignal (clickCount))

taskDemo1 = line3'
                "concurrent"
                ( "sendLine1 = Signal.mailbox 0\n" ++ "sendLine2 = Signal.mailbox 0\n" ++
                  "port sendCount1 : Signal (Task String ())\n" ++
                  "port sendCount1 = Signal.map (\\x -> case x of \n" ++
                  "                 Just a ->Signal.send sendLine1.address a\n" ++
                  "                 Nothing -> Task.fail \"\") (Stream.fromSignal (clickCount))\n"++
  "port sendCount2 : Signal (Task String ())\n"++
  "port sendCount2 = Signal.map (\\x -> case x of \n"++
                                  "                 Just a ->Task.andThen ( Signal.send sendLine2.address a) (\\x -> Task.sleep second)\n"++
                                  "                 Nothing -> Task.fail \"\") (Stream.fromSignal (clickCount))")
                "clickCount"
                "sendLine1.signal"
                "sendLine2.signal"
                clickCount
                sendLine1.signal
                sendLine2.signal


--all

all : Signal Element
all =
  signalFlow down [
                        signalDemo
                        ,title "Time"
                        , demoList [
                                  fpsDemo
                                ,fpsWhenDemo
                                , everyDemo
                                , timestampDemo
                                , delayDemo
                                , sinceDemo]
                        ,title "elm-signal-extra"
                        ,title "Signal.Discrete"
                        ,functionName "type alias EventSource = Signal ()"
                        ,demoList [esDemo
                                  ,whenEqualDemo
                                  ,whenChangeDemo
                                  ,whenChangeToDemo
                                  ,foldeDemo]
                        ,title "Signal.Extra"
                        ,demoList [
                                 zipDemo
                                ,runBufferDemo
                                ,delayRoundDemo
                                ,switchWhenDemo
                                ,switchSampleDemo
                                ,keepWhenDemo
                                ,sampleWhenDemo
                                ,keepThenDemo
                                ,keepWhenIDemo]
                        ,title "Signal.Stream"
                        ,demoList [sampleDemo]
                        ,title "Signal.Time"
                        ,demoList [limitRateDemo
                                ,dropWithinDemo
                                ,settledAfterDemo
                                ,startTimeDemo
                                ,relativeTimeDemo]
                        ,title "Signal and Task"
                        ,demoList [taskDemo1]
                        ]


main = all
