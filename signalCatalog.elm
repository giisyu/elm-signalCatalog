module SignalCatalog where

import Signal.Stream as Stream
import Signal exposing (..)
import Task exposing (..)
import Time exposing (..)
import Graphics.Element as Element exposing (down,Element)


import View exposing (..)
import TimeDemo exposing (..)
import SignalDemo exposing(..)
import SignalExtraDemo exposing (..) 

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

all : Signal Element
all =
  signalFlow down [
                        signalDemo
                        ,timeDemo
                        ,signalExtraDemo
                        ,taskSignalDemo
                        ]


main = all
