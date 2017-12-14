module Grid exposing (Grid, Tile, Direction(..), make, width, height, get, set, flat, move, genRandomTile, randomTile)

import Array exposing (Array)
import Random exposing (Generator)


type alias Grid =
    Array (Array Tile)


type alias Lane =
    Array Tile


type alias Tile =
    Int


type Direction
    = Left
    | Right
    | Up
    | Down


type MergeOp
    = Keep Tile
    | Merge Tile Tile


make : Int -> Int -> Grid
make width height =
    Array.repeat height <| Array.repeat width 0


width : Grid -> Int
width grid =
    grid |> row 0 |> Array.length


height : Grid -> Int
height grid =
    grid |> Array.length


get : Int -> Int -> Grid -> Tile
get x y grid =
    grid
        |> row y
        |> Array.get x
        |> Maybe.withDefault 0


set : Int -> Int -> Tile -> Grid -> Grid
set x y tile grid =
    let
        newRow =
            grid
                |> row y
                |> Array.set x tile
    in
        grid |> Array.set y newRow


flat : Grid -> List ( Int, Int, Tile )
flat grid =
    grid
        |> Array.toList
        |> List.indexedMap
            (\rowIndex row ->
                row
                    |> Array.toList
                    |> List.indexedMap
                        (\colIndex tile ->
                            ( colIndex
                            , rowIndex
                            , tile
                            )
                        )
            )
        |> List.concat


move : Direction -> Grid -> Grid
move dir grid =
    case dir of
        Left ->
            grid
                |> Array.map moveLane

        Right ->
            grid
                |> Array.map (reverse >> moveLane >> reverse)

        Up ->
            grid
                |> transpose
                |> Array.map moveLane
                |> transpose

        Down ->
            grid
                |> transpose
                |> Array.map (reverse >> moveLane >> reverse)
                |> transpose


genRandomTile : (( Int, Int, Tile ) -> msg) -> Grid -> Cmd msg
genRandomTile msg grid =
    Random.generate msg (randomTile grid)


randomTile : Grid -> Generator ( Int, Int, Tile )
randomTile grid =
    let
        mapGen ( x, y ) val =
            ( x, y, val )
    in
        Random.map2 mapGen (randomPosition grid) (randomValue 0.8)



-- PRIVATE --


randomPosition : Grid -> Generator ( Int, Int )
randomPosition grid =
    let
        available =
            flat grid
                |> List.filterMap
                    (\( x, y, tile ) ->
                        if tile == 0 then
                            Just ( x, y )
                        else
                            Nothing
                    )
    in
        Random.int 0 (List.length available)
            |> Random.map
                (\pos ->
                    available
                        |> List.drop pos
                        |> List.head
                        |> Maybe.withDefault ( 0, 0 )
                )


randomValue : Float -> Generator Int
randomValue p =
    Random.float 0 1
        |> Random.map
            (\val ->
                if val < p then
                    2
                else
                    4
            )


moveLane : Lane -> Lane
moveLane lane =
    lane
        |> Array.filter ((/=) 0)
        |> merge
        |> pad (Array.length lane)


merge : Lane -> Lane
merge lane =
    lane
        |> Array.toList
        |> mergeRecursive Array.empty
        |> Array.map
            (\op ->
                case op of
                    Keep a ->
                        a

                    Merge a b ->
                        a + b
            )


mergeRecursive : Array MergeOp -> List Tile -> Array MergeOp
mergeRecursive acc lane =
    case lane of
        [] ->
            acc

        [ a ] ->
            acc |> Array.push (Keep a)

        a :: b :: rest ->
            if a == b then
                rest |> mergeRecursive (Array.push (Merge a b) acc)
            else
                b :: rest |> mergeRecursive (Array.push (Keep a) acc)



-- HELPERS --


row : Int -> Grid -> Lane
row index grid =
    grid
        |> Array.get index
        |> Maybe.withDefault Array.empty


col : Int -> Grid -> Lane
col index grid =
    grid
        |> Array.map
            (Array.get index
                >> Maybe.withDefault 0
            )


reverse : Lane -> Lane
reverse lane =
    lane |> Array.foldr Array.push Array.empty


transpose : Grid -> Grid
transpose grid =
    grid
        |> row 0
        |> Array.indexedMap (\index _ -> col index grid)


pad : Int -> Lane -> Lane
pad len lane =
    Array.repeat (len - Array.length lane) 0
        |> Array.append lane
