module Repo exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=), Decoder)


-- MODEL


type alias Model =
  { name : String
  , prs : List PRInfo
  }


type alias PRInfo =
  { title : String
  , user : String
  }



-- UPDATE -- NONE YET
-- API stuff


decoder : Decoder Model
decoder =
  Decode.object2 Model
    ("repo" := Decode.string)
    ("prs" := (Decode.list prInfoDecoder))


prInfoDecoder : Decoder PRInfo
prInfoDecoder =
  Decode.object2 PRInfo
    ("title" := Decode.string)
    (Decode.oneOf [ "user" := Decode.string, Decode.succeed "No User" ])



-- VIEW


view : Model -> Html msg
view model =
  div []
    [ h3 [] [ text (model.name) ]
    , div [] (List.map viewPR model.prs)
    ]


viewPR : PRInfo -> Html msg
viewPR prinfo =
  div []
    [ text (prinfo.title)
    , text " by "
    , text (prinfo.user)
    ]
