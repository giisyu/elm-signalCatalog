module SignalCatalog where

import Signal.Stream as Stream
import Signal exposing (..)
import Task exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (down,Element,show)


import View exposing (..)
import TimeDemo exposing (..)
import SignalDemo exposing(..)
import SignalExtraDemo exposing (..) 

import Router exposing (..)
import History exposing (..)


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



------all

signals = (,,,,) <~ path 
                ~ signalDemo 
                ~ timeDemo
                ~ signalExtraDemo
                ~ taskSignalDemo

requestPage = Signal.mailbox "/"

port pageChange : Signal ( Task x () )
port pageChange = Signal.map setPath requestPage.signal

routing (pagePath,sig ,time,ex,task) = 
      let signalPage = (always sig)
          timePage = always time
          extraPage = always ex
          taskPage = always task
          allCagalog = 
              match 
                 [ "/" :-> signalPage
                 , "/Time" :-> timePage 
                 , "/Extra" :-> extraPage
                 , "/TaskSignal" :-> taskPage ] signalPage
      in allCagalog pagePath

routingElement = routing <~ signals



main = routingElement
