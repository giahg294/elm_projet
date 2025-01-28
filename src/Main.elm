{-|
Le programme principal est responsable de :
L'interface utilisateur pour la saisie des commandes ;
L'affichage du dessin SVG généré ;
L'intégration avec les modules de parsing et de dessin.
-}


module Main exposing (main)


-- 其余代码...


import Browser
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Parsing exposing (parseTcTurtle)  -- 引入解析模块
import Drawing exposing (drawTcTurtle)  -- 引入绘图模块

-- MODEL

type alias Model =
    { commands : String -- 用户输入的 TcTurtle 程序
    , svgContent : Svg Msg -- 动态生成的 SVG 绘图
    , errorMessage : Maybe String -- 错误提示信息
    }

init : Model
init =
    { commands = ""
    , svgContent = svg [] []
    , errorMessage = Nothing
    }

-- UPDATE

type Msg
    = UpdateCommands String
    | Draw

update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCommands newCommands ->
            { model | commands = newCommands, errorMessage = Nothing }

        Draw ->
            case parseTcTurtle model.commands of
                Ok program ->
                    { model | svgContent = drawTcTurtle program, errorMessage = Nothing }

                Err err ->
                    { model | errorMessage = Just ("解析错误: " ++ err) }

-- VIEW

view : Model -> Html Msg
view model =
    div [ style "padding" "20px" ]
        [ input [ placeholder "输入 TcTurtle 程序", value model.commands, onInput UpdateCommands, style "width" "100%" ] []
        , button [ onClick Draw, style "margin-top" "10px" ] [ text "绘制" ]
        , case model.errorMessage of
            Nothing ->
                div [] [ model.svgContent ]

            Just errMsg ->
                div [ style "color" "red", style "margin-top" "10px" ] [ text errMsg ]
        ]

-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view }
