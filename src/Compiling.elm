module Compiling exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (string)



----------------------------------------------
-- Types
----------------------------------------------


type alias Model =
    { grid : Array (Maybe Int)
    , current : Int
    }


type Msg
    = PressedLetter Char
    | PressedControl String


type Dir
    = Left
    | Right
    | Up
    | Down



----------------------------------------------
-- Update
----------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedControl "ArrowDown" ->
            ( { model | current = move Down model.current }, Cmd.none )

        PressedControl "ArrowUp" ->
            ( { model | current = move Up model.current }, Cmd.none )

        PressedControl "ArrowRight" ->
            ( { model | current = move Right model.current }, Cmd.none )

        PressedControl "ArrowLeft" ->
            ( { model | current = move Left model.current }, Cmd.none )

        PressedControl _ ->
            ( model, Cmd.none )

        PressedLetter 'j' ->
            ( { model | current = move Down model.current }, Cmd.none )

        PressedLetter 'k' ->
            ( { model | current = move Up model.current }, Cmd.none )

        PressedLetter 'h' ->
            ( { model | current = move Left model.current }, Cmd.none )

        PressedLetter 'l' ->
            ( { model | current = move Right model.current }, Cmd.none )

        PressedLetter _ ->
            ( model, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ div [ class "Grid" ]
            (List.map
                (\( i, _ ) ->
                    div
                        [ classList
                            [ ( "Cell", True )
                            , ( "is-current", model.current == i )
                            ]
                        ]
                        [ span [ class "Cell-index" ] [ text (String.fromInt i) ]
                        ]
                )
                (Array.toIndexedList model.grid)
            )
        ]



----------------------------------------------
-- Main
----------------------------------------------


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> onKeyDown keyDecoder
        }



----------------------------------------------
-- Helpers
----------------------------------------------


initialModel : Model
initialModel =
    Model (Array.fromList (List.repeat 100 (Just 0))) 0


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressedLetter char

        _ ->
            PressedControl string


move : Dir -> Int -> Int
move dir current =
    case dir of
        Left ->
            Basics.max (current - 1) 0

        Right ->
            Basics.min (current + 1) 99

        Up ->
            current - (if current <= 9 then 0 else 10)

        Down ->
            current + (if current >= 90 then 0 else 10)
