import Html exposing (text,div)
import Html.Attributes exposing (style)
import Mouse exposing (clicks,isDown)
import Signal exposing (..)
import Task exposing (..)
import Graphics.Element as Element exposing (Element,show ,flow , down,right,justified,beside)
import List 
import Text exposing (Style )
import Time exposing (..)
import String exposing (append,dropLeft)
import Signal.Extra as Extra exposing ((~>))
import Signal.Discrete as Discrete
import Signal.Stream as Stream
import Signal.Time as ExTime
import Keyboard
import Color


type TimeData a = TimeSignal | Action a

--timing
timefps = fps 15


codeBackColor = Color.rgb 255 255 255



signalFlow : Element.Direction -> List (Signal Element) -> Signal Element
signalFlow direction list = Extra.mapMany (flow direction) list 

lineString = "------------------------------------------------------"

--update

timeLineUpdate: String ->  (String -> String) -> Signal a -> Signal String
timeLineUpdate lineString f signal =
      let 
          addTimeSignal signal = merge (Action <~ signal) ( always TimeSignal<~ timefps)
          action = (addTimeSignal signal)
          update signal state = 
                        case signal of 
                          TimeSignal -> String.append state "-" |> dropLeft 1
                          Action a   -> let str = f <| toString a
                                        in String.append state str 
                                                    |> dropLeft (String.length str ) 
      in foldp update lineString action





-----View 


box : Int -> Int -> Int -> Element -> Element
box upSpace leftSpace downSpace strEle = 
         Element.flow down [
                          Element.spacer 0 upSpace 
                          , (Element.flow right [ Element.spacer leftSpace 5 , strEle])
                          , Element.spacer 0 downSpace ]


strToElement : Style -> String -> Element
strToElement stringStyle str =
             Element.justified <| Text.style stringStyle (Text.fromString str)


toSignalElement : Int -> Int -> Int -> Style -> String -> Signal Element 
toSignalElement upSpace leftSpace downSpace strStyle str = 
     Signal.constant (box upSpace leftSpace downSpace (strToElement strStyle str))


signalSpace : Int -> Int -> Signal Element
signalSpace x y = Signal.constant (Element.spacer x y )


typeEleFormat : String -> Element
typeEleFormat str = Element.justified 
                        <| Text.monospace 
                        <| Text.fromString  
                        <| String.padLeft 35 ' ' str
                        
toTypeElement : Signal a -> Signal Element
toTypeElement signal = 
   Signal.map (\x -> typeEleFormat <| toString x) signal
  
timeLineView :  String -> Element
timeLineView lineStr = 
    let format string = Element.justified 
                                <| Text.monospace 
                                <| Text.fromString string
    in  (format lineStr)

toTimeLine : (String -> String)  -> Signal a -> Signal Element
toTimeLine f signal = Signal.map timeLineView (timeLineUpdate lineString f signal)


code : String -> Signal Element
code codeStr = 
      let format string =    Element.color (Color.rgb 255 255 255) 
                                <| Element.justified 
                                <| Text.monospace 
                                <| Text.fromString string 
      in Signal.constant ( Element.spacer 5 5 `beside` format codeStr)


titleStyle : Style
titleStyle = 
              { typeface = ["monospace"]
              , height = Just 25
              , color = Color.black
              , bold = True
              , italic = False
              , line = Nothing
              }

title : String -> Signal Element
title str = toSignalElement 7 20 8 titleStyle str

typeReferenceStyle : Style
typeReferenceStyle = 
              { typeface = ["monospace"]
              , height = Just 15
              , color = Color.rgb 255 85 85
              , bold = True
              , italic = False
              , line = Nothing
              }



typeReference : String -> Signal Element
typeReference str = toSignalElement 3 70 3 typeReferenceStyle str 

reference'Sryle : Style
reference'Sryle = 
                { typeface = ["monospace"]
              , height = Just 15
              , color = Color.black
              , bold = False 
              , italic = False
              , line = Nothing
              }

reference' : String -> Signal Element
reference' str = toSignalElement 3 70 5 reference'Sryle str

customCatalog : Signal a -> (String -> String) -> String -> Signal Element
customCatalog signal f codeStr = 
            signalFlow right [toTypeElement signal 
                             ,signalSpace 5 10
                             ,toTimeLine f signal
                             ,code codeStr]

basicCatalog : Signal a -> String -> Signal Element
basicCatalog signal str = customCatalog signal identity str


line1 : String -> String -> Signal a -> Signal Element
line1 description code signal = 
            signalFlow down [
                              typeReference description
                              ,basicCatalog signal code
                              ]


line2 : String -> String -> Signal a -> Signal Element
line2 description code1 code2 signal1 signal2 = 
        signalFlow down [
                    typeReference description
                    ,basicCatalog signal1 code1
                    ,basicCatalog signal2 code2
                              ]
line1 : String -> String -> Signal a -> Signal Element
line2' description refeStr code1 code2 signal1 signal2 = 
        signalFlow down [
                    typeReference description
                    ,reference' refeStr
                    ,basicCatalog signal1 code1
                    ,basicCatalog signal2 code2
                              ]
line1 : String -> String -> Signal a -> Signal Element
line3 description code1 code2 code3 signal1 signal2 signal3 = 
            signalFlow down [
                        typeReference description
                        ,basicCatalog signal1 code1
                        ,basicCatalog signal2 code2
                        ,basicCatalog signal3 code3 ]
line1 : String -> String -> Signal a -> Signal Element
line3' description refeStr code1 code2 code3 signal1 signal2 signal3 = 
            signalFlow down [
                        typeReference description
                        ,reference' refeStr
                        ,basicCatalog signal1 code1
                        ,basicCatalog signal2 code2
                        ,basicCatalog signal3 code3 ]





-----Demo--------------




mouseClick = Mouse.clicks
trueClick = Signal.map (always True) Mouse.clicks

mapDemo = 
  signalFlow down [
                typeReference "map : (a -> result) -> Signal a -> Signal result"
                ,reference' "Apply a function to a signal."
                ,basicCatalog mouseClick "Mouse.clicks"
                ,customCatalog trueClick 
                           (\x -> if | x == "True" -> " T"
                                     | otherwise -> x)
                           "trueClick = Signal.map (always True) Mouse.clicks"
                           ]

countTime = Signal.foldp (\x y -> y + 1 ) 0 (every second)
map2Test = Signal.map2 (,) mouseClick countTime

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

fpsDemo = line1
            "fps : number -> Signal Time"
            "fps 1"
            fpsTest

mouseIsDown = Mouse.isDown
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
                    typeReference "es : Signal a -> EventSource"
                    ,customCatalog (every second) (\x -> String.left 3 x ) "every second"
                    ,basicCatalog esTest "Discrete.es (every second)"
                     ]

equalTest = Discrete.whenEqual True Mouse.isDown
whenEqualDemo = signalFlow down [
  typeReference "whenEqual : a -> Signal a -> EventSource"
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

--demoList

lineSpace : Signal Element
lineSpace = signalSpace 5 24 

demoList : List (Signal Element) -> Signal Element
demoList list = signalFlow down <| ( List.intersperse lineSpace list ) ++ [lineSpace]


--all

all : Signal Element
all = 
  signalFlow down [ 
                        title "Signal"
                        , demoList [ mapDemo
                                  , map2Demo
                                  , foldpDemo
                                  ,mergeDemo
                                  , filterDemo
                                  , dropRepeatsDemo
                                  , sampleOnDemo ]
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
                        ,typeReference "type alias EventSource = Signal ()"
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
