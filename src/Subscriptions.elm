module Subscriptions exposing (subscriptions)

import Browser.Events
import Convertor exposing (keyDecoder)
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
        , Ports.nextSong GotSong

        -- because we need the type on songEnded to be () -> Msg
        , Ports.songEnded (\_ -> GetNewSong)

        -- space bar pauses music. We need to decode the spacebar though rip.
        , Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)
        , Ports.gotPicture GotPicture
        ]
