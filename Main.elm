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
import Author
import PRCountColumn
import Utils exposing (..)


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
      ( { model | prCountColumns = (mapReposToPRCountColumns result) }, Cmd.none )

    RefreshStats ->
      ( model, fetchStatsCmd )

    ColumnMsg i msg ->
      case Array.get i model.prCountColumns of
        Nothing ->
          ( model, Cmd.none )

        Just column ->
          let
            columns =
              model.prCountColumns

            ( updatedColumn, columnCmd ) =
              PRCountColumn.update msg column

            beforeColumns =
              Array.slice 0 i columns

            afterColumns =
              Array.slice (i + 1) (Array.length columns) columns

            updatedPrCountColumns =
              Array.append (Array.push updatedColumn beforeColumns) afterColumns
          in
            ( { model | prCountColumns = updatedPrCountColumns }, Cmd.none )


mapReposToAuthors : List Repo.Model -> List Author.Model
mapReposToAuthors repos =
  repos
    |> List.concatMap
        (\repo -> List.map (\pr -> ( pr.user, pr )) repo.prs)
    |> listToDictOfLists
    |> Dict.map (\key value -> Author.create key value)
    |> Dict.values


mapReposToPRCountColumns : List Repo.Model -> Array PRCountColumn.Model
mapReposToPRCountColumns repos =
  repos
    |> mapReposToAuthors
    |> List.map (\author -> ( (List.length author.prs), author ))
    |> listToDictOfLists
    |> Dict.map (\count authors -> PRCountColumn.create count authors)
    |> Dict.values
    |> Array.fromList



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
  Task.perform HttpError
    FetchStats
    fetchStats



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
