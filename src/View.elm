module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Grid exposing (Grid, Tile)


viewGrid : Grid -> Html msg
viewGrid grid =
    viewCells (Grid.width grid) (Grid.height grid)
        ++ viewTiles grid
        |> div [ class ("grid grid-" ++ toString (Grid.width grid) ++ "-" ++ toString (Grid.height grid)) ]


viewCells : Int -> Int -> List (Html msg)
viewCells width height =
    List.range 0 (height - 1)
        |> List.map (viewCellRows width)
        |> List.concat


viewCellRows : Int -> Int -> List (Html msg)
viewCellRows width row =
    List.range 0 (width - 1)
        |> List.map (viewCell row)


viewCell : Int -> Int -> Html msg
viewCell row col =
    div [ class ("cell grid-row-" ++ toString row ++ " grid-col-" ++ toString col) ] []


viewTiles : Grid -> List (Html msg)
viewTiles grid =
    grid
        |> Grid.tiles
        |> List.map viewTile


viewTile : Tile -> Html msg
viewTile { pos, val } =
    div [ class ("tile grid-row-" ++ toString pos.y ++ " grid-col-" ++ toString pos.x) ]
        [ text (toString val)
        ]
