port module Ports exposing (..)

import Json.Encode as E

port globalKeyUp : (Int -> msg) -> Sub msg