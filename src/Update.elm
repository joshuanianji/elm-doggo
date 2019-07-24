module Update exposing (update)

import Element exposing (classifyDevice)
import Json.Decode as Decode
import Music as Music exposing (FromJsSongPackage, Music, MusicState(..), ToJsSongPackage)
import Parser
import Picture exposing (Picture, Pictures, ToJsPicPackage)
import Ports
import Types exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize windowSize ->
            ( { model | device = classifyDevice windowSize |> Debug.log "Device" }
            , Cmd.none
            )

        ToggleMusic ->
            musicUpdate model
                (\music ->
                    ( { music | state = Music.toggle music.state }
                      -- toggle music port for the state the music is going to be in
                    , toggleMusicPort (Music.toggle music.state)
                    )
                )

        -- request a new song
        GetNewSong ->
            requestSong Ports.getNewSong model

        -- requests a previous song
        GetPreviousSong ->
            requestSong Ports.getPreviousSong model

        -- once we got a new song, and play the music again.
        GotSong package ->
            ( { model | music = model.music |> Result.andThen (updateSongs package) }
            , Ports.playMusic ()
            )

        KeyPressed value ->
            case value of
                " " ->
                    update ToggleMusic model

                _ ->
                    ( model, Cmd.none )

        ChangePicture ->
            requestPic Ports.getPicture model

        GotPicture package ->
            ( { model | pictures = model.pictures |> Result.andThen (updatePicture package) }
            , Cmd.none
            )


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

    Anyways when we recieve the new data JS sends us when the user skips a song,
    we convert it to FromJsonSongPackage type which may or may not work.
    Sincs model.music is already a result type we can easily implement Maybe.andThen to
    make our code nice!

-}
updateSongs : Decode.Value -> Music -> Result Decode.Error Music
updateSongs jsonValue music =
    let
        package =
            Parser.songPackageFromJson jsonValue
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
            Parser.toPic jsonValue
    in
    Result.map
        (\p -> { pics | currentPic = Just p })
        package
