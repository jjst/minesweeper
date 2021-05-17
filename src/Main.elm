module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, tr, td, table)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model =
    { board : GameBoard, moves : Int }


type alias Coords =
    ( Int, Int )

type alias GameBoard = List (List Cell)

type alias Cell = 
  { hasMine : Bool
  , state : CellState
  }

type CellState = Unexplored | Flagged | Explored

init : Model
init =
    let
        model =
            { board = emptyBoard, moves = 0 }
    in
        model


emptyBoard =
    { hasMine = False, state = Unexplored }
        |> List.repeat 15
        |> List.repeat 15




neighbors : Coords -> List Coords
neighbors ( i, j ) =
    [ ( i, j ), ( i - 1, j ), ( i + 1, j ), ( i, j - 1 ), ( i, j + 1 ) ]


isWon : GameBoard -> Bool
isWon board =
    False


indexedMap : (Coords -> a -> b) -> List (List a) -> List (List b)
indexedMap f board =
    board
        |> List.indexedMap (\i row -> row |> List.indexedMap (\j cellModel -> f ( i, j ) cellModel))




-- UPDATE

type Msg = ClickedCell Coords

update : Msg -> Model -> Model
update message ({ board, moves } as model) =
    let
        newModel =
            case message of
                ClickedCell coords ->
                    { model | board = indexedMap (\c cell -> if coords == c then { cell | state = Explored } else cell) board }

    in
        newModel



-- VIEW


view : Model -> Html Msg
view model =
    let

        rows =
            List.indexedMap
                (\i row -> tr [] (row |> List.indexedMap (\j cellModel -> td [] [ (renderCellAt ( i, j ) cellModel) ])))
                model.board

        lightsTable =
            table [] rows
    in
        table [] rows


renderCellAt : Coords -> Cell -> Html Msg
renderCellAt ( i, j ) cellModel =
  button [ onClick (ClickedCell ( i, j )) ] [ text (renderText cellModel) ]

renderText : Cell -> String
renderText { hasMine, state } =
  case (hasMine, state) of
    (_, Unexplored) -> "_"
    (True, Explored) -> "x"
    (False, Explored) -> "o"
    (_, Flagged) -> "|>"
