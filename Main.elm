module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never
main =
  Html.program
    { init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    }



-- MODEL


type alias Model =
  { name : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model "Missing PR description", Cmd.none )



-- UPDATE


type Msg
  = Something


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Something ->
      ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h3 [] [ text "Pull requests by author" ]
    ]
