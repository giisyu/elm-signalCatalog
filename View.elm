module View (signalFlow,functionName,description,basicCatalog,customCatalog,signalSpace,title, line1,line2,line3,line3',demoList) where

import Graphics.Element as Element exposing (Element,show ,flow , down,right,justified,beside)
import Time exposing (..)
import Signal.Extra as Extra exposing ((~>))
import Signal exposing (..)
import String exposing (append,dropLeft)
import Text exposing (Style )
import Color



type TimeData a = TimeSignal | Action a

--timing
timefps = fps 15



signalFlow : Element.Direction -> List (Signal Element) -> Signal Element
signalFlow direction list = Extra.mapMany (flow direction) list

lineString = "------------------------------------------------------"



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

fucnNameStyle : Style
fucnNameStyle =
              { typeface = ["monospace"]
              , height = Just 15
              , color = Color.rgb 255 85 85
              , bold = True
              , italic = False
              , line = Nothing
              }



functionName : String -> Signal Element
functionName str = toSignalElement 3 70 3 fucnNameStyle str

descriptionSryle : Style
descriptionSryle =
                { typeface = ["monospace"]
              , height = Just 15
              , color = Color.black
              , bold = False
              , italic = False
              , line = Nothing
              }

description : String -> Signal Element
description str = toSignalElement 3 70 5 descriptionSryle str

customCatalog : Signal a -> (String -> String) -> String -> Signal Element
customCatalog signal f codeStr =
            signalFlow right [toTypeElement signal
                             ,signalSpace 5 10
                             ,toTimeLine f signal
                             ,code codeStr]

basicCatalog : Signal a -> String -> Signal Element
basicCatalog signal str = customCatalog signal identity str


line1 : String -> String -> Signal a -> Signal Element
line1 funcName code signal =
            signalFlow down [
                              functionName funcName
                              ,basicCatalog signal code
                              ]


line2 : String ->String -> String -> Signal a -> Signal b -> Signal Element
line2 funcName code1 code2 signal1 signal2 =
        signalFlow down [
                    functionName funcName
                    ,basicCatalog signal1 code1
                    ,basicCatalog signal2 code2
                              ]

line3 : String -> String -> String -> String -> Signal a -> Signal b ->Signal c ->Signal Element
line3 funcName code1 code2 code3 signal1 signal2 signal3 =
            signalFlow down [
                        functionName funcName
                        ,basicCatalog signal1 code1
                        ,basicCatalog signal2 code2
                        ,basicCatalog signal3 code3 ]

line3' : String -> String -> String -> String ->String -> Signal a ->Signal b ->Signal c -> Signal Element
line3' funcName refeStr code1 code2 code3 signal1 signal2 signal3 =
            signalFlow down [
                        functionName funcName
                        ,description refeStr
                        ,basicCatalog signal1 code1
                        ,basicCatalog signal2 code2
                        ,basicCatalog signal3 code3 ]


--demoList

lineSpace : Signal Element
lineSpace = signalSpace 5 24

demoList : List (Signal Element) -> Signal Element
demoList list = signalFlow down <| ( List.intersperse lineSpace list ) ++ [lineSpace]
