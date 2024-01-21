module Compiling exposing (main)

import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (onKeyDown)
import File
import File.Download as Download
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder, decodeString, field, int, map4, string, bool)
import Json.Encode as Encode
import Task



----------------------------------------------
-- Types
----------------------------------------------


type alias Model =
    { grid : Array Cell
    , current : Int
    , score : Int
    , help : Bool
    }


type Msg
    = PressedLetter Char
    | PressedControl String
    | NewBoard
    | ShowHelp
    | GotHelp
    | NoOp
    | Save
    | Load
    | FileSelected File.File
    | FileLoaded String


type Dir
    = Left
    | Right
    | Up
    | Down


type Cell
    = Occupied Int
    | Available
    | Unavailable



----------------------------------------------
-- Update
----------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PressedControl "ArrowDown" ->
            ( { model | current = move Down model.current }, Cmd.none )

        PressedControl "ArrowUp" ->
            ( { model | current = move Up model.current }, Cmd.none )

        PressedControl "ArrowRight" ->
            ( { model | current = move Right model.current }, Cmd.none )

        PressedControl "ArrowLeft" ->
            ( { model | current = move Left model.current }, Cmd.none )

        PressedLetter ' ' ->
            ( maybeSetValue model, Cmd.none )

        PressedLetter 'j' ->
            ( { model | current = move Down model.current }, Cmd.none )

        PressedLetter 'k' ->
            ( { model | current = move Up model.current }, Cmd.none )

        PressedLetter 'h' ->
            ( { model | current = move Left model.current }, Cmd.none )

        PressedLetter 'l' ->
            ( { model | current = move Right model.current }, Cmd.none )

        NewBoard ->
            ( initialModel, Task.attempt (\_ -> NoOp) (Dom.focus "cell-0") )

        ShowHelp ->
            ( { model | help = True }, Cmd.none )

        GotHelp ->
            ( { model | help = False }, Cmd.none )

        Load ->
            ( model, Select.file [ "application/json" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded content ->
            ( case decodeString modelDecoder content of
                Ok v ->
                    v

                Err _ ->
                    initialModel
            , Task.attempt (\_ -> NoOp) (Dom.focus "cell-0")
            )

        Save ->
            ( model, Download.string "board.json" "application/json" (Encode.encode 2 (encodeModel model)) )

        PressedControl _ ->
            ( model, Cmd.none )

        PressedLetter _ ->
            ( model, Cmd.none )



----------------------------------------------
-- View
----------------------------------------------


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "App", True )
            , ( "is-flipped", model.help )
            ]
        ]
        [ div [ class "App-front" ]
            [ div [ class "Grid" ]
                (List.map
                    (\( i, c ) ->
                        div
                            [ classList
                                [ ( "Cell", True )
                                , ( "is-current", model.current == i )
                                , case c of
                                    Available ->
                                        ( "is-available", True )

                                    Occupied _ ->
                                        ( "is-occupied", True )

                                    Unavailable ->
                                        ( "is-unavailable", True )
                                ]
                            , tabindex 1
                            , id ("cell-" ++ String.fromInt i)
                            ]
                            [ span [ class "Cell-val" ] [ text (cellToText c) ]
                            , span [ class "Cell-index" ] [ text (String.fromInt i) ]
                            ]
                    )
                    (Array.toIndexedList model.grid)
                )
            , div [ class "HUD" ]
                [ p [] [ text ("Score: " ++ String.fromInt model.score) ]
                , div [ class "HUD-toolbar" ]
                    [ button [ onClick NewBoard ] [ text "New" ]
                    , button [ onClick ShowHelp ] [ text "?" ]
                    , button [ onClick Load ] [ text "Load" ]
                    , button [ onClick Save ] [ text "Save" ]
                    ]
                ]
            ]
        , div [ class "App-back" ]
            [ p []
                [ text "Use "
                , code [] [ text "hjkl" ]
                , text " or the arrow keys to move the cursor."
                , br [] []
                , text "Press space to place a value."
                ]
            , p [] [ text "The rules are:" ]
            , ul []
                [ li [] [ text "You can place a new value along the x-axis skipping 2 cells" ]
                , li [] [ text "You can place a new value diagonally skipping 1 cell" ]
                ]
            , p [] [ text "The UI shows the possible moves, have fun!" ]
            , button [ onClick GotHelp ] [ text "Got it" ]
            ]
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


modelDecoder : Decoder Model
modelDecoder =
    map4 Model
        (field "grid"
            (Decode.array
                (Decode.int
                    |> Decode.andThen
                        (\v ->
                            if v == -1 then
                                Decode.succeed Available

                            else if v == -2 then
                                Decode.succeed Unavailable

                            else
                                Decode.succeed (Occupied v)
                        )
                )
            )
        )
        (field "current" int)
        (field "score" int)
        (field "help" bool)


encodeModel : Model -> Encode.Value
encodeModel m =
    Encode.object
        [ ( "grid", Encode.array encodeCell m.grid )
        , ( "current", Encode.int m.current )
        , ( "score", Encode.int m.score )
        , ( "help", Encode.bool m.help )
        ]


encodeCell : Cell -> Encode.Value
encodeCell c =
    Encode.int
        (case c of
            Occupied v ->
                v

            Available ->
                -1

            Unavailable ->
                -2
        )


maybeSetValue : Model -> Model
maybeSetValue m =
    let
        currentCell =
            Array.get m.current m.grid
    in
    case currentCell of
        Just Available ->
            occupyCell m

        _ ->
            m


occupyCell : Model -> Model
occupyCell m =
    let
        withOccupiedCurrent =
            Array.set m.current (Occupied (m.score + 1)) m.grid

        withAvailables =
            Array.indexedMap
                (\i c -> cellAvailability i c m.current)
                withOccupiedCurrent
    in
    { m
        | grid = withAvailables
        , score = m.score + 1
    }


cellAvailability : Int -> Cell -> Int -> Cell
cellAvailability i c curr =
    let
        availableMoves =
            calculateAvailableMoves curr
    in
    case c of
        Occupied v ->
            Occupied v

        Available ->
            cellIsAvailable i availableMoves

        Unavailable ->
            cellIsAvailable i availableMoves


cellIsAvailable : Int -> List Int -> Cell
cellIsAvailable i a =
    if List.member i a then
        Available

    else
        Unavailable


calculateAvailableMoves : Int -> List Int
calculateAvailableMoves curr =
    let
        f =
            firstChar curr

        l =
            lastChar curr

        e =
            diff l (<=) 6 3

        w =
            diff l (>=) 3 -3

        n =
            diff f (>=) 3 -30

        s =
            diff f (<=) 6 30

        se =
            diff l (<=) 7 22

        ne =
            diff l (<=) 7 -18

        sw =
            diff l (>=) 2 18

        nw =
            diff l (>=) 2 -22
    in
    List.filter (\x -> x /= -1) [ e, w, n, s, se, ne, sw, nw ]


lastChar : Int -> ( Maybe Int, Int )
lastChar x =
    ( x |> String.fromInt |> String.padLeft 2 '0' |> String.right 1 |> String.toInt, x )


firstChar : Int -> ( Maybe Int, Int )
firstChar x =
    ( x |> String.fromInt |> String.padLeft 2 '0' |> String.left 1 |> String.toInt, x )


diff : ( Maybe Int, Int ) -> (Int -> Int -> Bool) -> Int -> Int -> Int
diff c op y delta =
    case c of
        ( Nothing, _ ) ->
            -1

        ( Just v, x ) ->
            if op v y then
                x + delta

            else
                -1


cellToText : Cell -> String
cellToText c =
    case c of
        Available ->
            ""

        Occupied v ->
            String.fromInt v

        Unavailable ->
            ""


initialModel : Model
initialModel =
    Model (Array.fromList (List.repeat 100 Available)) 0 0 False


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
            current
                - (if current <= 9 then
                    0

                   else
                    10
                  )

        Down ->
            current
                + (if current >= 90 then
                    0

                   else
                    10
                  )
