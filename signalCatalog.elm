module SignalCatalog where

import Signal.Stream as Stream
import Signal exposing (..)
import Task exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (down,Element,show,container,middle,flow,right,tag,link)
import Graphics.Input as Input exposing (customButton)
import Window
import Signal.Extra as Extra
import Color
import Text

import View exposing (..)
import TimeDemo exposing (..)
import SignalDemo exposing(..)
import SignalExtraDemo exposing (..) 

import Router exposing (..)
import History exposing (..)
import Html exposing (Html,nav, div,ul,li,text,fromElement,toElement)
import Html.Attributes exposing (class)

import Debug
--taskDemo

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

taskSignalDemo = 
        signalFlow down[ title "Signal and Task"
                        ,demoList [taskDemo1]
                        ]



------


signals :Signal (String,Element,Element,Element,Element)
signals = (,,,,) <~(Debug.log "hash" <~ hash)
                ~ signalDemo 
                ~ timeDemo
                ~ signalExtraDemo
                ~ taskSignalDemo


hashClick =  mailbox Nothing

port set : Signal (Task String ())
port set = (\x -> case x of   
                      Just y -> setPath y
                      Nothing -> Task.fail "" ) <~ hashClick.signal 
  

routing : (String,Element,Element,Element,Element) -> Element 
routing (pagePath,sig ,time,ex,task) = 
      let signalPage = (always sig)
          timePage = always time
          extraPage = always ex
          taskPage = always task
          allCagalog = 
              match 
                 [ "" :-> signalPage
                 , "#/Time" :-> timePage 
                 , "#/Extra" :-> extraPage
                 , "#/TaskSignal" :-> taskPage ] signalPage
      in allCagalog pagePath

routingElement : Signal Element
routingElement = routing <~ signals

makebutton : (String,String) -> Element
makebutton (str,path) = 
            let defaultText str =Element.justified <| Text.style Text.defaultStyle <| Text.fromString str
                ele = container 140 20 middle (defaultText str)

            in  Input.clickable (Signal.message hashClick.address (Just path) ) ele   


buttons :List Element
buttons = List.map makebutton [("Signal","#/"),("Time","#/Time"),("elm-signal-extra","#/Extra"),("TaskSignal","#/TaskSignal")]

button : (Int,Int) -> Element
button (x,y) = container x 20 middle <| flow right buttons

signalButton : Signal Element
signalButton = button <~ Window.dimensions

all : Signal Element
all = signalFlow down [signalButton , routingElement]
main = all
