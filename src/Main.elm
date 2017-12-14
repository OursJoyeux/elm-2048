module Main exposing (main)

import Html exposing (..)
import Keyboard exposing (KeyCode)
import Random
import Grid exposing (Grid, Tile, Direction(..))
import View


main : Platform.Program Basics.Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL --


type alias Model =
    { score : Int
    , grid : Grid
    }


init : ( Model, Cmd Msg )
init =
    let
        grid =
            Grid.make 4 4
                |> Grid.set 1 1 2
                |> Grid.set 2 1 2
                |> Grid.set 1 2 2
                |> Grid.set 2 2 4
                |> Grid.set 2 3 8
    in
        ( Model 0 grid, Cmd.none )



-- UPDATE --


type Msg
    = NewGame
    | NewTile ( Int, Int, Tile )
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        NewTile ( x, y, tile ) ->
            ( { model | grid = Grid.set x y tile model.grid }, Cmd.none )

        KeyDown code ->
            case codeToDirection code of
                Just dir ->
                    ( { model | grid = Grid.move dir model.grid }, Grid.genRandomTile NewTile model.grid )

                Nothing ->
                    ( model, Cmd.none )


codeToDirection : KeyCode -> Maybe Direction
codeToDirection code =
    case code of
        37 ->
            Just Left

        38 ->
            Just Up

        39 ->
            Just Right

        40 ->
            Just Down

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown



-- VIEWS --


view : Model -> Html Msg
view model =
    View.viewGrid model.grid
