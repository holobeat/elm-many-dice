module Main exposing (..)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Random
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { faces : List Int
    , gameStatus : GameStatus
    , diceCount : Int
    }


type GameStatus
    = Playing
    | Stopped


type Msg
    = NewRandomNumber (List Int)
      -- | NextRoll
    | Tick Time.Posix
    | AddDice
    | RemoveDice
    | ToggleStop


randomFaceNumbers : Int -> Cmd Msg
randomFaceNumbers count =
    Random.generate NewRandomNumber <| Random.list count (Random.int 1 6)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { faces = []
      , gameStatus = Playing
      , diceCount = 1
      }
    , randomFaceNumbers 1
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandomNumber faces ->
            ( { model | faces = faces }, Cmd.none )

        --
        -- NextRoll ->
        --     ( model, randomFaceNumbers model.diceCount )
        Tick _ ->
            if model.gameStatus == Stopped then
                ( model, Cmd.none )
            else
                ( model, randomFaceNumbers model.diceCount )

        AddDice ->
            ( { model
                | diceCount = model.diceCount + 1
                , faces = model.faces ++ [ 1 ]
              }
            , Cmd.none
            )

        RemoveDice ->
            if model.diceCount == 0 then
                ( model, Cmd.none )
            else
                ( { model
                    | diceCount = model.diceCount - 1
                    , faces = List.take (List.length model.faces - 1) model.faces
                  }
                , Cmd.none
                )

        ToggleStop ->
            ( { model
                | gameStatus =
                    if model.gameStatus == Playing then
                        Stopped
                    else
                        Playing
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick


view : Model -> Html Msg
view model =
    div []
        [ viewOptions model
        , div [] <|
            List.map (\x -> lazy viewDice x) <|
                model.faces
        ]


viewOptions : Model -> Html Msg
viewOptions model =
    div styleOptions
        [ viewButton AddDice "Add"
        , viewButton RemoveDice "Remove"
        , viewButton ToggleStop "Toggle Stop/Play"
        , viewStatus model.diceCount model.faces
        ]


viewStatus : Int -> List Int -> Html Msg
viewStatus diceCount faces =
    div styleStatus
        [ text <|
            "Dice count: "
                ++ String.fromInt diceCount
                ++ ", Sum: "
                ++ String.fromInt (List.sum faces)
        ]


viewButton : Msg -> String -> Html Msg
viewButton msg caption =
    button ([ onClick msg ] ++ styleButton) [ text caption ]


viewDot : Bool -> Html Msg
viewDot visible =
    div (styleDot visible) []


viewDice : Int -> Html Msg
viewDice number =
    div styleDice <|
        case number of
            1 ->
                viewFace [ 0, 0, 0, 0, 1, 0, 0, 0, 0 ]

            2 ->
                viewFace [ 1, 0, 0, 0, 0, 0, 0, 0, 1 ]

            3 ->
                viewFace [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ]

            4 ->
                viewFace [ 1, 0, 1, 0, 0, 0, 1, 0, 1 ]

            5 ->
                viewFace [ 1, 0, 1, 0, 1, 0, 1, 0, 1 ]

            6 ->
                viewFace [ 1, 0, 1, 1, 0, 1, 1, 0, 1 ]

            _ ->
                [ text ("Huh? Cannot show " ++ String.fromInt number ++ "!") ]


viewFace : List Int -> List (Html Msg)
viewFace face =
    let
        pickDot =
            \x -> viewDot (x /= 0)
    in
        [ div [] (List.map pickDot <| List.take 3 face)
        , div [] (List.map pickDot <| List.take 3 <| List.drop 3 face)
        , div [] (List.map pickDot <| List.take 3 <| List.drop 6 face)
        ]


styleDot : Bool -> List (Html.Attribute Msg)
styleDot visible =
    [ style "border" "0"
    , style "border-radius" "12px"
    , style "display" "inline-block"
    , style "width" "24px"
    , style "height" "24px"
    , style "margin" "10px"
    ]
        ++ if visible then
            [ style "background-color" "white" ]
           else
            []


styleDice : List (Html.Attribute Msg)
styleDice =
    [ style "border" "3px solid gray"
    , style "border-radius" "20px"
    , style "background-color" "brown"
    , style "display" "inline-block"
    , style "padding" "20px"
    , style "margin" "10px"
    , style "color" "white"
    ]


styleOptions : List (Html.Attribute Msg)
styleOptions =
    [ style "font-family" "Arial, Helvetica"
    , style "font-size" "1.2em"
    , style "padding" "10px"
    ]


styleButton : List (Html.Attribute Msg)
styleButton =
    [ style "font-size" "1em"
    , style "margin" "5px"
    , style "padding" "10px"
    , style "border-radius" "5px"
    ]


styleStatus : List (Html.Attribute Msg)
styleStatus =
    [ style "margin" "5px"
    , style "padding" "10px"
    , style "border-radius" "5px"
    , style "border" "2px solid gray"
    , style "background-color" "lightgray"
    , style "display" "inline-block"
    , style "width" "220px"
    ]
