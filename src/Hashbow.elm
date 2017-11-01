module Hashbow exposing (hashbow, color, toHex)

import Color exposing (Color)
import List exposing (..)
import String
import Hex
import Char.CodePoint


hashbow : String -> String
hashbow = color 0.5 0.5 >> toHex


color : Float -> Float -> String -> Color
color saturation lightness str =
  let
    sum = String.toList str
      |> map Char.CodePoint.fromChar
      |> foldl (+) 0
    hue = (sum * sum) % 360 |> toFloat
  in
    Color.hsl hue saturation lightness

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
