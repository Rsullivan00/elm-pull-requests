module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)
import Repo


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
  { repos : List Repo.Model
  , errorDescription : String
  }


type alias PRInfo =
  { title : String
  , user : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model [] "", fetchStatsCmd )



-- UPDATE


type Msg
  = HttpError Http.Error
  | FetchStats (List Repo.Model)
  | RefreshStats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpError error ->
      ( { model | errorDescription = "Error: " ++ (toString error) }, Cmd.none )

    FetchStats result ->
      ( { model | repos = result }, Cmd.none )

    RefreshStats ->
      ( model, fetchStatsCmd )



-- API stuff


api : String
api =
  "http://localhost:3000/"


statsUrl : String
statsUrl =
  api ++ "stats"


fetchStats : Platform.Task Http.Error (List Repo.Model)
fetchStats =
  Http.get (Decode.list Repo.decoder) statsUrl


fetchStatsCmd : Cmd Msg
fetchStatsCmd =
  Task.perform HttpError FetchStats fetchStats



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Open pull requests" ]
    , h4 [] [ text model.errorDescription ]
    , div [] (List.map Repo.view model.repos)
    , button [ class "btn", onClick RefreshStats ]
        [ i [ class "material-icons left" ]
            [ text "loop" ]
        , text "Refresh"
        ]
    ]
