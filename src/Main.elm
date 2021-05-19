module Main exposing (main)

import Array
import Browser
import Html exposing (Html, button, div, node, text, tr, td, table)
import Html.Events exposing (onClick)
import Html.Attributes as HA
import Grid
import Grid exposing (Grid, Coords)
import Random



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL

type alias Model =
    { board : GameBoard, moves : Int }


type alias GameBoard = Grid Cell

type alias Cell = 
  { hasMine : Bool
  , state : CellState
  }

type CellState = Unexplored | Flagged | Explored

init : () -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { board = emptyBoard, moves = 0 }
    in
        ( model, Random.generate NewBoard randomBoard)

boardSize : Int
boardSize = 15

emptyBoard : GameBoard
emptyBoard =
    { hasMine = False, state = Unexplored }
        |> List.repeat boardSize |> Array.fromList
        |> List.repeat boardSize |> Array.fromList


randomBoard : Random.Generator GameBoard
randomBoard =
    randomBool 
        |> Random.map (\bool -> { hasMine = bool, state = Unexplored })
        |> Random.list boardSize |> Random.map Array.fromList
        |> Random.list boardSize |> Random.map Array.fromList


randomBool : Random.Generator Bool
randomBool =
  Random.weighted (10, True) [ (90, False) ]


-- minesInNeighborhood : GameBoard -> Coords -> Int


isWon : GameBoard -> Bool
isWon board =
    False




-- UPDATE

type Msg 
  = ClickedCell Coords
  | NewBoard GameBoard

update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ board, moves } as model) =
    let
        newModel =
            case message of
                NewBoard gameBoard ->
                    { model | board = gameBoard }
                ClickedCell coords ->
                    { model | board = Grid.indexedMap (\c cell -> if coords == c then { cell | state = Explored } else cell) board }

    in
        ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    let

        rows = model.board
            |> Array.indexedMap
                (\i row -> div [HA.class "row" ] 
                    (row |> Array.indexedMap (\j cell -> viewCell cell (i, j)) |> Array.toList))
            |> Array.toList
        grid = div [ HA.class "grid" ] rows
    in
        div [] [ css "style.css", grid ]



viewCell : Cell -> Coords -> Html Msg
viewCell { hasMine, state } coords =
  case (hasMine, state) of
    (_, Unexplored) -> div [ HA.class "cell", onClick (ClickedCell coords) ] [ ]
    (True, Explored) -> div [ HA.class "cell mine triggered", onClick (ClickedCell coords) ] [ ]
    (False, Explored) -> div [ HA.class "cell triggered", onClick (ClickedCell coords) ] [ ]
    (_, Flagged) -> div [ HA.class "cell", onClick (ClickedCell coords) ] [ ]

css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
