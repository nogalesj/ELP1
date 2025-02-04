module Main exposing (..)

import Browser
import Html exposing (Html, div, input, button, text)
import Svg exposing (Svg, svg, line)
import Parser exposing (Parser, run, succeed, token, float, spaces, andThen, map, oneOf, (|.), (|=), loop, Step)
import String
import Char exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (placeholder, value, style)
import Svg.Attributes exposing (width, height, x1, y1, x2, y2, stroke)

-- MAIN


main = Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = 
    { userInput : String
    , drawing : List Instruction }


init : Model
init =
  { userInput = "" 
  , drawing = [] }



-- UPDATE


type Msg =
    UpdateInput String
    | Draw


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newText ->
            { model | userInput = newText }

        Draw ->
            { model | drawing = read model.userInput }


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Entrez votre code TcTurtle :" ]
        , input [ placeholder "Forward 100, Left 90, Forward 50", value model.userInput, onInput UpdateInput ] []
        , button [ onClick Draw ] [ text "Dessiner" ]
        , div []
            [ svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ] (drawInstructions 250 250 0 model.drawing) ]
        ]

-- FONCTION POUR DESSINER LES INSTRUCTIONS

drawInstructions : Float -> Float -> Float -> List Instruction -> List (Svg msg)
drawInstructions x y angle instructions =
    case instructions of
        [] ->
            []

        instruction :: rest ->
            case instruction of
                Forward distance ->
                    let
                        radian = degrees angle
                        newX = x + distance * cos radian
                        newY = y - distance * sin radian
                    in
                    line [ x1 (String.fromFloat x), y1 (String.fromFloat y)
                         , x2 (String.fromFloat newX), y2 (String.fromFloat newY)
                         , stroke "black" ] []
                    :: drawInstructions newX newY angle rest

                Left deltaAngle ->
                    drawInstructions x y (angle + deltaAngle) rest

                Right deltaAngle ->
                    drawInstructions x y (angle - deltaAngle) rest

                Repeat n subInstructions ->
                    drawRepeated x y angle n subInstructions rest


-- Fonction auxiliaire pour gérer les répétitions correctement

drawRepeated : Float -> Float -> Float -> Int -> List Instruction -> List Instruction -> List (Svg msg)
drawRepeated x y angle n subInstructions rest =
    if n <= 0 then
        drawInstructions x y angle rest
    else
        let
            drawn = drawInstructions x y angle subInstructions
            (newX, newY, newAngle) = List.foldl updatePosition (x, y, angle) subInstructions
        in
        drawn ++ drawRepeated newX newY newAngle (n - 1) subInstructions rest

updatePosition : Instruction -> (Float, Float, Float) -> (Float, Float, Float)
updatePosition instruction (x, y, angle) =
    case instruction of
        Forward distance ->
            let
                radian = degrees angle
                newX = x + distance * cos radian
                newY = y - distance * sin radian
            in
            (newX, newY, angle)

        Left deltaAngle ->
            (x, y, angle + deltaAngle)

        Right deltaAngle ->
            (x, y, angle - deltaAngle)

        Repeat n subInstructions ->
            List.foldl updatePosition (x, y, angle) (List.concat (List.repeat n subInstructions))




--- PARSING


-- Définition des types


type Instruction
    = Forward Float
    | Left Float
    | Right Float
    | Repeat Int (List Instruction)

type alias Program = List Instruction

-- Fonction principale de parsing
read : String -> List Instruction
read input =
    case run programParser input of
        Ok result ->
            let
                _ = Debug.log "Parsed instructions" result
            in
            result
        Err err ->
            let
                _ = Debug.log "Parsing error" err
            in
            []


-- Parser pour un programme complet
programParser : Parser (List Instruction)
programParser =
    succeed identity
        |. token "["
        |. spaces
        |= Parser.loop [] programLoop
        |. token "]"
        |. spaces

programLoop : List Instruction -> Parser (Step (List Instruction) (List Instruction))
programLoop acc =
    oneOf
        [ succeed (\instr -> Parser.Loop (instr :: acc))
            |= instructionParser
            |. spaces
            |. oneOf [ token ",", succeed () ]  -- Gère les virgules entre les instructions
            |. spaces
        , succeed (Parser.Done (List.reverse acc))
        ]


-- Parser pour une instruction individuelle
instructionParser : Parser Instruction
instructionParser =
    oneOf 
        [ forwardParser
        , leftParser
        , rightParser
        , repeatParser
        ]


-- Parser pour Forward
forwardParser : Parser Instruction
forwardParser =
    succeed Forward
        |. token "Forward"
        |. spaces
        |= float

leftParser : Parser Instruction
leftParser =
    succeed Left
        |. token "Left"
        |. spaces
        |= float

rightParser : Parser Instruction
rightParser =
    succeed Right
        |. token "Right"
        |. spaces
        |= float

repeatParser : Parser Instruction
repeatParser =
    succeed Repeat
        |. token "Repeat"
        |. spaces
        |= int
        |. spaces
        |= listParser

-- Parser pour un nombre entier
int : Parser Int
int =
    Parser.getChompedString (Parser.chompWhile isDigit)
        |> andThen (\s -> 
            case String.toInt s of
                Just n -> succeed n
                Nothing -> Parser.problem "Invalid integer"
        )

float : Parser Float
float =
    oneOf
        [ Parser.float
        , int |> map toFloat
        ]


-- Parser pour une liste d'instructions
listParser : Parser (List Instruction)
listParser =
    succeed identity
        |. token "["
        |. spaces
        |= Parser.loop [] listLoop
        |. token "]"
        |. spaces

listLoop : List Instruction -> Parser (Step (List Instruction) (List Instruction))
listLoop acc =
    oneOf
        [ succeed (\instr -> Parser.Loop (instr :: acc))
            |= instructionParser
            |. spaces
            |. oneOf [ token ",", succeed () ]  -- Gère les virgules entre les instructions
            |. spaces
        , succeed (Parser.Done (List.reverse acc))
        ]





-- Fonction principale pour afficher le dessin

display : Float -> Float -> Float -> List Instruction -> Svg msg
display x y angle instructions =
    svg [ Svg.Attributes.width "500", Svg.Attributes.height "500" ] (drawInstructions x y angle instructions)