module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)


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
  { value : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model "", fetchPRInfoCmd )



-- UPDATE


type Msg
  = HttpError Http.Error
  | FetchPRInfoSuccess String
  | RefreshPRInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpError error ->
      ( { model | value = "Error: " ++ (toString error) }, Cmd.none )

    FetchPRInfoSuccess result ->
      ( { model | value = result }, Cmd.none )

    RefreshPRInfo ->
      ( model, fetchPRInfoCmd )



-- API stuff


api : String
api =
  "http://localhost:3000/"


prInfoUrl : String
prInfoUrl =
  api ++ "stats"


fetchPRInfo : Platform.Task Http.Error String
fetchPRInfo =
  Http.getString prInfoUrl


fetchPRInfoCmd : Cmd Msg
fetchPRInfoCmd =
  Task.perform HttpError FetchPRInfoSuccess fetchPRInfo



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h3 [] [ text "Pull requests by author" ]
    , p [] [ text model.value ]
    , button [ class "btn btn-default", onClick RefreshPRInfo ] [ text "Refresh" ]
    ]
