module Main exposing (main)

import Html exposing (..)
import Keyboard exposing (KeyCode)
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
            Grid.make { w = 4, h = 4 }
                |> Grid.set { x = 1, y = 1 } 2
                |> Maybe.andThen (Grid.set { x = 2, y = 1 } 2)
                |> Maybe.andThen (Grid.set { x = 1, y = 2 } 2)
                |> Maybe.andThen (Grid.set { x = 2, y = 2 } 4)
                |> Maybe.andThen (Grid.set { x = 2, y = 3 } 8)
                |> Maybe.withDefault (Grid.make { w = 4, h = 4 })
    in
        ( Model 0 grid, Cmd.none )



-- UPDATE --


type Msg
    = NewGame
    | NewTile (Maybe Tile)
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        NewTile Nothing ->
            ( model, Cmd.none )

        NewTile (Just { pos, val }) ->
            ( { model | grid = (Grid.set pos val model.grid) |> Maybe.withDefault model.grid }, Cmd.none )

        KeyDown code ->
            case codeToDirection code of
                Just dir ->
                    let
                        newGrid =
                            Grid.move dir model.grid
                    in
                        ( { model | grid = newGrid }, Grid.generate NewTile newGrid )

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



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs KeyDown



-- VIEWS --


view : Model -> Html Msg
view model =
    View.viewGrid model.grid
