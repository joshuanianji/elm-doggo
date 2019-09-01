module Subscriptions exposing (subscriptions)

import Browser.Events
import Json.Decode as Decode
import Ports
import Types exposing (Model, Msg(..), WindowSize)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize
            (\x y ->
                WindowResize (WindowSize x y)
            )

        -- because we need the type on songEnded to be () -> Msg
        , Ports.songEnded (\_ -> RequestNewSong)

        -- space bar pauses music. We need to decode the spacebar though rip.
        , Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)
        ]



-- decode Keys
-- just returns string of what the user inputted. a spacebar will return " "


keyDecoder =
    Decode.field "key" Decode.string
