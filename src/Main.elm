module Main exposing (main)


{-|
Le programme principal est responsable de :
L'interface utilisateur pour la saisie des commandes ;
L'affichage du dessin SVG généré ;
L'intégration avec les modules de parsing et de dessin.
-}

import Browser
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Parsing exposing (parseTcTurtle) 
import Drawing exposing (drawTcTurtle) 

-- MODEL

type alias Model =
    { commands : String -- Programme TcTurtle saisi par l'utilisateur  用户输入的 TcTurtle 程序
    , svgContent : Svg Msg -- SVG généré dynamiquement  动态生成的 SVG 绘图
    , errorMessage : Maybe String 
    }

-- INIT

-- Définition du modèle initial  -- 定义初始模型
init : Model
init =
    { commands = ""  -- Initialisation de la commande (vide au début)  -- 初始化命令（开始时为空）
    , svgContent = svg [] []  -- Contenu SVG vide  -- 空的SVG内容
    , errorMessage = Nothing  -- Aucune erreur au début  -- 开始时没有错误信息
    }

-- UPDATE

-- Définition des messages pour les mises à jour  -- 定义更新的消息
type Msg
    = UpdateCommands String  -- Message pour la mise à jour des commandes  -- 用于更新命令的消息
    | Draw  -- Message pour dessiner  -- 用于绘制的消息

-- Fonction de mise à jour du modèle en fonction des messages  -- 根据消息更新模型的函数
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCommands newCommands ->
            { model | commands = newCommands, errorMessage = Nothing }  -- Mise à jour des commandes et réinitialisation de l'erreur  -- 更新命令并重置错误信息

        Draw ->
            case parseTcTurtle model.commands of
                Ok program ->
                    { model | svgContent = drawTcTurtle program, errorMessage = Nothing }  -- Dessin du programme et réinitialisation de l'erreur  -- 绘制程序并重置错误信息

                Err err ->
                    { model | errorMessage = Just ("Erreur de Parsing: " ++ err) }  -- Si erreur de parsing, affichage du message d'erreur  -- 如果解析错误，显示错误信息

-- VIEW

-- Fonction de vue qui génère l'interface utilisateur  -- 生成用户界面的视图函数
view : Model -> Html Msg
view model =
    div [ style "padding" "20px" ]
        [ input [ placeholder "Veuillez entrer un programme TcTurtle", value model.commands, onInput UpdateCommands, style "width" "100%" ] []  -- Champ de saisie pour les commandes  -- 命令输入框
        , button [ onClick Draw, style "margin-top" "10px" ] [ text "绘制" ]  -- Bouton pour dessiner  -- 绘制按钮
        , case model.errorMessage of
            Nothing ->
                div [] [ model.svgContent ]  -- Affichage du contenu SVG si pas d'erreur  -- 如果没有错误，显示SVG内容

            Just errMsg ->
                div [ style "color" "red", style "margin-top" "10px" ] [ text errMsg ]  -- Affichage du message d'erreur en rouge  -- 以红色显示错误信息
        ]

-- MAIN

-- Point d'entrée principal de l'application  -- 应用程序的主入口
main =
    Browser.sandbox { init = init, update = update, view = view }
