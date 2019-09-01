module Update exposing (update)

import Element exposing (classifyDevice)
import Json.Decode as Decode
import Music
import Ports
import Types exposing (Model, Msg(..))
import Visual


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSize ->
            ( { model | device = classifyDevice windowSize |> Debug.log "Device" }
            , Cmd.none
            )

        ToggleMusic currState ->
            let
                updateMusic music =
                    { music | state = Music.toggle currState }
            in
            ( { model | music = Result.map updateMusic model.music }
            , case currState of
                Music.Off ->
                    Ports.toggleMusic False

                Music.On ->
                    Ports.toggleMusic True
            )

        RequestNewSong ->
            ( model
            , case model.music of
                Ok m ->
                    Music.newSong GotSong m

                _ ->
                    Cmd.none
            )

        RequestPreviousSong ->
            ( model
            , case model.music of
                Ok m ->
                    Music.previousSong GotSong m

                _ ->
                    Cmd.none
            )

        -- once we got a song, and play the music again.
        GotSong music ->
            ( { model | music = Ok music }
            , Ports.playMusic ()
            )

        RequestNewVisual ->
            ( model
            , case model.visuals of
                Ok v ->
                    Visual.newVisual GotVisual v

                _ ->
                    Cmd.none
            )

        GotVisual visuals ->
            ( { model | visuals = Ok visuals }
            , Cmd.none
            )

        KeyPressed value ->
            case value of
                " " ->
                    case model.music of
                        Ok m ->
                            update (ToggleMusic m.state) model

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-----------------------------
-- OLD CODE (gonna delete ;))
-----------------------------
{--


requestSong : (ToJsSongPackage -> Cmd msg) -> Model -> ( Model, Cmd msg )
requestSong portRequest model =
    let
        cmd =
            case
                ( model.music
                    |> Result.toMaybe
                    |> Maybe.andThen .currentSong
                , model.music
                )
            of
                ( Just currentSong, Ok music ) ->
                    let
                        toJsSongPackage =
                            ToJsSongPackage
                                music.previousSongs
                                currentSong
                                music.nextSongs
                                music.songs
                    in
                    portRequest toJsSongPackage

                _ ->
                    Cmd.none
    in
    ( model, cmd )


requestPic : (ToJsPicPackage -> Cmd msg) -> Model -> ( Model, Cmd msg )
requestPic portRequest model =
    let
        cmd =
            case
                ( model.pictures
                    |> Result.toMaybe
                    |> Maybe.andThen .currentPic
                    |> Debug.log "bruh"
                , model.pictures
                )
            of
                ( Just currentPic, Ok pictures ) ->
                    let
                        toJsPicPackage =
                            ToJsPicPackage
                                currentPic
                                pictures.allPics
                    in
                    portRequest toJsPicPackage

                _ ->
                    Cmd.none
    in
    ( model, cmd )



-- updates music. Code gets a bit ugly with all the Result types so we just cover it up lmao
-- yeah this code is real ugly


musicUpdate : Model -> (Music -> ( Music, Cmd Msg )) -> ( Model, Cmd Msg )
musicUpdate model converter =
    let
        tuple =
            Result.map
                converter
                model.music
    in
    ( { model | music = tuple |> Result.map Tuple.first }
    , tuple |> Result.map Tuple.second |> Result.toMaybe |> Maybe.withDefault Cmd.none
    )


{-| MAMA MIA MY CODE ISA TURNING TO SPAGHETTIA

    Double yikes but I wanna get this done lol
    Also this is useful for Result.andThen because i love monads weeeee

    Anyways when we receive the new data JS sends us when the user skips a song,
    we convert it to FromJsonSongPackage type which may or may not work.
    Sincs model.music is already a result type we can easily implement Maybe.andThen to
    make our code nice!

-}
updateSongs : Decode.Value -> Music -> Result Decode.Error Music
updateSongs jsonValue music =
    let
        package =
            Convertor.songPackageFromJson jsonValue
    in
    Result.map
        (\p ->
            { music
                | currentSong = Just p.currentSong
                , previousSongs = p.previousSongs
                , nextSongs = p.nextSongs
            }
        )
        package


{-| I did kind of a weird way of sending ports.

    I send a True when I want to play the music and a False when I want to pause it.

    This prevents me from sending type aliases because I don't know how to handle them (I would tbh because it makes more sense)

-}
toggleMusicPort : MusicState -> Cmd Msg
toggleMusicPort music =
    case music of
        Off ->
            Ports.toggleMusic False

        On ->
            Ports.toggleMusic True


updatePicture : Decode.Value -> Pictures -> Result Decode.Error Pictures
updatePicture jsonValue pics =
    let
        package =
            Convertor.toPic jsonValue
    in
    Result.map
        (\p -> { pics | currentPic = Just p })
        package
--}
