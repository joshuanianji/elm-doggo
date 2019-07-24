module Picture exposing (Picture, Pictures, ToJsPicPackage, init)

{-| How I handle Pictures

    This is much simpler than Music because I'm not implementing backtracking and stuff

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



-- data format we send to JS


type alias ToJsPicPackage =
    { currentPic : Picture
    , allPics : List Picture
    }
