module Picture exposing (PicType(..), Picture, Pictures, ToJsPicPackage, init)

{-| How I handle Pictures

    This is much simpler than Music because I'm not implementing backtracking and stuff
    Also pretty funny because I'm also handling videos with this but I'm calling it pictures lol.

-}

import Json.Decode as Decode


type alias Pictures =
    { currentPic : Maybe Picture
    , allPics : List Picture
    }


init : List Picture -> Pictures
init pictures =
    { currentPic = Nothing
    , allPics = pictures
    }


type alias Picture =
    { src : String
    , id : String
    }



-- used by parser and stuff


type PicType
    = Image
    | Video



-- data format we send to JS


type alias ToJsPicPackage =
    { currentPic : Picture
    , allPics : List Picture
    }
