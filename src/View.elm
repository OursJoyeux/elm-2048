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
    List.range 0 (height - 1) |> List.map (viewCellRows width) |> List.concat


viewCellRows : Int -> Int -> List (Html msg)
viewCellRows width row =
    List.range 0 (width - 1) |> List.map (viewCell row)


viewCell : Int -> Int -> Html msg
viewCell row col =
    div [ class ("cell grid-row-" ++ toString row ++ " grid-col-" ++ toString col) ] []


viewTiles : Grid -> List (Html msg)
viewTiles grid =
    grid
        |> Grid.flat
        |> List.filter (\( x, y, tile ) -> tile /= 0)
        |> List.map viewTile


viewTile : ( Int, Int, Tile ) -> Html msg
viewTile ( x, y, tile ) =
    div [ class ("tile grid-row-" ++ toString y ++ " grid-col-" ++ toString x) ]
        [ text (toString tile)
        ]
