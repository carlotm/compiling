module Umbra exposing (main)

import Html exposing (div, section, text)
import Html.Attributes exposing (..)


view _ =
    div [ class "App" ]
        [ section [] [ text "Main actions" ]
        , section [] [ text "Playground" ]
        , section [] [ text "Shadow list" ]
        ]


main =
    view "no model yet"
