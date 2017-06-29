module Api exposing (..)

import Http
import Json.Decode as Decode exposing ((:=), Decoder)
import Array exposing (Array)
import Dict
import Author
import Repo
import PRCountColumn
import Utils exposing (..)


api : String
api =
  "http://localhost:3000/"


statsUrl : String
statsUrl =
  api ++ "stats"


fetchStats : Platform.Task Http.Error (List Repo.Model)
fetchStats =
  Http.get (Decode.list Repo.decoder) statsUrl



-- Data transformation helpers


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
