module PullRequest exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing ((:=), Decoder)


-- MODEL


type alias Model =
  { title : String
  , user : String
  }



-- UPDATE


decoder : Decoder Model
decoder =
  Decode.object2 Model
    ("title" := Decode.string)
    (Decode.oneOf [ "user" := Decode.string, Decode.succeed "No User" ])



-- VIEW


view : Model -> Html msg
view model =
  div []
    [ text (model.title)
    ]
