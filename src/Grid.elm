module Grid exposing (..)

import Array exposing (Array)
import Maybe

type alias Grid a = Array (Array a)

type alias Coords =
    ( Int, Int )

get : Coords -> Grid a -> Maybe a
get ( i, j ) grid =
    Array.get i grid 
      |> Maybe.andThen (\row -> Array.get j row)

neighbors : Coords -> List Coords
neighbors ( i, j ) =
    [ ( i, j ), ( i - 1, j ), ( i + 1, j ), ( i, j - 1 ), ( i, j + 1 ) ]

indexedMap : (Coords -> a -> b) -> Grid a -> Grid b
indexedMap f board =
    board
        |> Array.indexedMap (\i row -> row |> Array.indexedMap (\j a -> f ( i, j ) a))

