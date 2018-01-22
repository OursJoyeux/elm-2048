module Grid exposing (Grid, Tile, Dimension, Position, Value, Direction(..), make, init, width, height, get, set, tiles, move, moves, apply, score, hasMoved, generator, generate)

import Random exposing (Generator)
import List.Extra


type Grid
    = Grid { dim : Dimension, tiles : List Tile }


type alias Tile =
    { pos : Position, val : Value }


type alias Dimension =
    { w : Int, h : Int }


type alias Position =
    { x : Int, y : Int }


type alias Value =
    Int


type Direction
    = Left
    | Right
    | Up
    | Down


type Move
    = Keep Tile
    | Move Tile Tile
    | Merge Tile Tile Tile


type alias MoveFactory =
    Position -> Int -> Position


make : Dimension -> Grid
make dim =
    Grid { dim = dim, tiles = [] }


init : Grid -> Generator Grid
init grid =
    let
        genAndSet : Grid -> Generator (Maybe Grid)
        genAndSet grid =
            grid
                |> generator
                |> Random.map (Maybe.andThen (\tile -> grid |> set tile.pos tile.val))
    in
        grid
            |> genAndSet
            |> Random.andThen
                (Maybe.map genAndSet
                    >> Maybe.withDefault (Random.bool |> Random.map (always Nothing))
                )
            |> Random.map (Maybe.withDefault grid)


width : Grid -> Int
width grid =
    dim grid |> .w


height : Grid -> Int
height grid =
    dim grid |> .h


get : Position -> Grid -> Maybe Value
get pos grid =
    tiles grid
        |> List.Extra.find (\t -> t.pos == pos)
        |> Maybe.map .val


set : Position -> Value -> Grid -> Maybe Grid
set pos val grid =
    case grid |> get pos of
        Nothing ->
            Just
                (Grid
                    { dim = dim grid
                    , tiles = { pos = pos, val = val } :: (tiles grid)
                    }
                )

        Just _ ->
            Nothing


tiles : Grid -> List Tile
tiles (Grid { tiles }) =
    tiles


dim : Grid -> Dimension
dim (Grid { dim }) =
    dim


move : Direction -> Grid -> Grid
move dir grid =
    grid |> apply (grid |> moves dir)


moves : Direction -> Grid -> List Move
moves dir grid =
    let
        makePosRow old index =
            { x = index, y = old.y }

        makePosRowRev old index =
            { x = (width grid) - index - 1, y = old.y }

        makePosCol old index =
            { x = old.x, y = index }

        makePosColRev old index =
            { x = old.x, y = (height grid) - index - 1 }
    in
        case dir of
            Left ->
                grid
                    |> rows
                    |> List.map (lineMoves makePosRow [])
                    |> List.concat

            Right ->
                grid
                    |> rows
                    |> List.map (List.reverse >> lineMoves makePosRowRev [])
                    |> List.concat

            Up ->
                grid
                    |> columns
                    |> List.map (lineMoves makePosCol [])
                    |> List.concat

            Down ->
                grid
                    |> columns
                    |> List.map (List.reverse >> lineMoves makePosColRev [])
                    |> List.concat


apply : List Move -> Grid -> Grid
apply moves grid =
    let
        getNewTile move =
            case move of
                Keep t ->
                    t

                Move _ t ->
                    t

                Merge _ _ t ->
                    t
    in
        Grid { dim = dim grid, tiles = moves |> List.map getNewTile }


score : List Move -> Int
score moves =
    moves
        |> List.map
            (\m ->
                case m of
                    Merge _ _ res ->
                        res.val

                    _ ->
                        0
            )
        |> List.sum


hasMoved : List Move -> Bool
hasMoved moves =
    moves
        |> List.any
            (\m ->
                case m of
                    Keep _ ->
                        False

                    Move _ _ ->
                        True

                    Merge _ _ _ ->
                        True
            )


generator : Grid -> Generator (Maybe Tile)
generator grid =
    let
        makeTile : Maybe Position -> Value -> Maybe Tile
        makeTile pos val =
            pos |> Maybe.map (\pos -> { pos = pos, val = val })
    in
        Random.map2 makeTile (randomPosition grid) (randomValue 0.8)


generate : (Maybe Tile -> msg) -> Grid -> Cmd msg
generate msg grid =
    Random.generate msg (generator grid)



-- PRIVATE --


size : Grid -> Int
size grid =
    (width grid) * (height grid)


rows : Grid -> List (List Tile)
rows grid =
    tiles grid
        |> List.sortBy (.pos >> .y)
        |> List.Extra.groupWhile (\t1 t2 -> t1.pos.y == t2.pos.y)
        |> List.map (List.sortBy (.pos >> .x))


columns : Grid -> List (List Tile)
columns grid =
    tiles grid
        |> List.sortBy (.pos >> .x)
        |> List.Extra.groupWhile (\t1 t2 -> t1.pos.x == t2.pos.x)
        |> List.map (List.sortBy (.pos >> .y))


lineMoves : MoveFactory -> List Move -> List Tile -> List Move
lineMoves factory acc tiles =
    let
        makeMove acc tile =
            let
                newPos =
                    factory tile.pos (List.length acc)
            in
                if newPos == tile.pos then
                    Keep tile
                else
                    Move tile { tile | pos = newPos }

        makeMerge acc t1 t2 =
            let
                newPos =
                    factory t1.pos (List.length acc)
            in
                Merge t1 t2 { pos = newPos, val = t1.val + t2.val }
    in
        case tiles of
            [] ->
                acc

            [ a ] ->
                (makeMove acc a) :: acc

            a :: b :: rest ->
                if a.val == b.val then
                    rest |> lineMoves factory ((makeMerge acc a b) :: acc)
                else
                    b :: rest |> lineMoves factory ((makeMove acc a) :: acc)


availablePositions : Grid -> List Position
availablePositions grid =
    let
        all =
            List.Extra.lift2 Position
                (List.range 0 (width grid - 1))
                (List.range 0 (height grid - 1))

        occupied =
            grid |> tiles |> List.map .pos
    in
        all |> List.filter (\pos -> not <| List.member pos occupied)


randomPosition : Grid -> Generator (Maybe Position)
randomPosition grid =
    let
        avail =
            grid |> availablePositions
    in
        Random.int 0 ((List.length avail) - 1)
            |> Random.map
                (\pos ->
                    avail |> List.Extra.getAt pos
                )


randomValue : Float -> Generator Value
randomValue p =
    Random.float 0 1
        |> Random.map
            (\val ->
                if val < p then
                    2
                else
                    4
            )
