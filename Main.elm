module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)


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
  { repos : List RepoStats
  , errorDescription : String
  }


type alias RepoStats =
  { name : String
  , prs : List PRInfo
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
  | FetchStats (List RepoStats)
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


prInfoDecoder : Decoder PRInfo
prInfoDecoder =
  Decode.object2 PRInfo
    ("title" := Decode.string)
    (Decode.oneOf [ "user" := Decode.string, Decode.succeed "No User" ])


repoStatsDecoder : Decoder RepoStats
repoStatsDecoder =
  Decode.object2 RepoStats
    ("repo" := Decode.string)
    ("prs" := (Decode.list prInfoDecoder))


fetchStats : Platform.Task Http.Error (List RepoStats)
fetchStats =
  Http.get (Decode.list repoStatsDecoder) statsUrl


fetchStatsCmd : Cmd Msg
fetchStatsCmd =
  Task.perform HttpError FetchStats fetchStats



-- VIEW


view : Model -> Html Msg
view model =
  div [ class "container" ]
    [ h1 [] [ text "Pull requests by author" ]
    , h4 [] [ text model.errorDescription ]
    , div [] (List.map viewRepo model.repos)
    , button [ class "btn btn-default", onClick RefreshStats ] [ text "Refresh" ]
    ]


viewRepo : RepoStats -> Html msg
viewRepo repoStats =
  div []
    [ h3 [] [ text (repoStats.name) ]
    , div [] (List.map viewPR repoStats.prs)
    ]


viewPR : PRInfo -> Html msg
viewPR prinfo =
  div []
    [ text (prinfo.title)
    , text " by "
    , text (prinfo.user)
    ]
