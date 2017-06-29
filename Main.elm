module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Array exposing (Array)
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)
import Dict
import Repo
import PRCountColumn
import Utils exposing (..)
import Api


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
  { prCountColumns : Array PRCountColumn.Model
  , errorDescription : String
  }


init : ( Model, Cmd Msg )
init =
  ( Model Array.empty "", fetchStatsCmd )



-- UPDATE


type Msg
  = HttpError Http.Error
  | FetchStats (List Repo.Model)
  | RefreshStats
  | ColumnMsg Int PRCountColumn.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    HttpError error ->
      ( { model | errorDescription = "Error: " ++ (toString error) }, Cmd.none )

    FetchStats result ->
      ( { model | prCountColumns = (Api.mapReposToPRCountColumns result) }, Cmd.none )

    RefreshStats ->
      ( model, fetchStatsCmd )

    ColumnMsg i msg ->
      let
        updatedColumns =
          updateAtIndex model.prCountColumns i PRCountColumn.update msg
      in
        ( { model | prCountColumns = updatedColumns }, Cmd.none )


fetchStatsCmd : Cmd Msg
fetchStatsCmd =
  Task.perform HttpError
    FetchStats
    Api.fetchStats



-- VIEW


viewColumns : List PRCountColumn.Model -> List (Html Msg)
viewColumns columns =
  columns
    |> List.indexedMap (\i column -> Html.map (ColumnMsg i) (PRCountColumn.view column))


view : Model -> Html Msg
view model =
  let
    columns =
      model.prCountColumns
        |> Array.toList
  in
    div [ class "app-container" ]
      (viewColumns columns)
