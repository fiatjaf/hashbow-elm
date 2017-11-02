module Hashbow exposing (hashbow, color, toHex)

{-| Generate hexadecimal (usable as colors) hashes from arbitrary strings.

The hashes are not collision-resistant at all, but they are useful, for
example, if you have to generate a custom different color for each element
of a collection of whatever you may have in your app.

@docs hashbow, color, toHex

-}

import Color exposing (Color)
import List exposing (..)
import String
import Hex
import Char.CodePoint


{-| The simplest method: takes any String and outputs a color in hex format.

    hashbow "Bruce Willis" == "#bf408e"
    hashbow "Nicolas Cage" == "#40bf8c"

-}
hashbow : String -> String
hashbow = color 0.5 0.5 >> toHex


{-| By default, `hashbow` uses the HSL colors scheme to generate the hashes,
with the hue provided by the string and saturation and lightness set to 0.5
each. Here you can generate a Color with different saturation and lightness.

    color 0.5 0.5 "Bruce Willis" == "#bf408e"

`hashbow` is defined like this:

    hashbow = color 0.5 0.5 >> toHex

-}
color : Float -> Float -> String -> Color
color saturation lightness str =
  let
    sum = String.toList str
      |> map Char.CodePoint.fromChar
      |> foldl (+) 0
    hue = (sum * sum) % 360 |> toFloat
  in
    Color.hsl hue saturation lightness

{-| Takes a Color and turns it into a hex format. Just a helper for when you
use `color` instead of `hashbow`.

    color 0.5 0.5 "Bruce Willis" |> toHex == "#bf408e"

-}
toHex : Color -> String
toHex color =
  let
    rgba = Color.toRgb color
    hex =
      ( Hex.toString rgba.red ) ++
      ( Hex.toString rgba.green ) ++
      ( Hex.toString rgba.blue )
  in
    "#" ++ hex
