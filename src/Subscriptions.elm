module Subscriptions exposing (subscriptions)

import Browser.Events
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
        ]
