module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, h3, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type Msg
    = Next
    | Reset


type alias Model =
    { current : Maybe Participant
    , pending : List Participant
    , gone : List Participant
    }


type alias Participant =
    String


init : Model
init =
    reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Next ->
            case model.pending of
                [] ->
                    model

                p :: ps ->
                    Model (Just p)
                        ps
                        (case model.current of
                            Nothing ->
                                model.gone

                            Just c ->
                                c :: model.gone
                        )

        Reset ->
            reset


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Next ] [ text "Next" ]
        , div [] [ text (Maybe.withDefault "-" model.current) ]
        , div [] [ h3 [] [ text "Pending" ], div [ style "color" "green" ] [ enumerate model.pending ] ]
        , div [] [ h3 [] [ text "Gone" ], div [ style "color" "red" ] [ enumerate model.gone ] ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


enumerate : List String -> Html Msg
enumerate elems =
    ul []
        (List.map
            (\c -> li [] [ text c ])
            elems
        )


reset : Model
reset =
    Model Nothing [ "Person 1", "Person 2", "Person 3" ] []
