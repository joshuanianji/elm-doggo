port module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (..)
import Html exposing (Html)
import Subscriptions exposing (subscriptions)
import Types exposing (Flags, Model, Msg, init)
import Update exposing (update)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view >> Element.layout []
        , update = update
        , subscriptions = subscriptions
        }
