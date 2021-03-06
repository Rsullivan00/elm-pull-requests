module Repo exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing ((:=), Decoder)
import PullRequest


-- MODEL


type alias Model =
  { name : String
  , prs : List PullRequest.Model
  }



-- UPDATE


decoder : Decoder Model
decoder =
  Decode.object2 Model
    ("repo" := Decode.string)
    ("prs" := (Decode.list PullRequest.decoder))



-- VIEW


view : Model -> Html msg
view model =
  div []
    [ h3 [] [ text (model.name) ]
    , div [] (List.map PullRequest.view model.prs)
    ]
