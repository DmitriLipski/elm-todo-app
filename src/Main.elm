module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed



---- MODEL ----


type alias Model =
    { uid : Int
    , inputValue : String
    , taskList : List Task
    }


type alias Task =
    { id : Int
    , description : String
    , isCompleted : Bool
    , editing : Bool
    }


newTask : String -> Int -> Task
newTask desc id =
    { id = id
    , description = desc
    , isCompleted = False
    , editing = False
    }


init : ( Model, Cmd Msg )
init =
    ( { uid = 0
      , inputValue = ""
      , taskList =
            []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateInput String
    | AddTask Task
    | RemoveTask Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput value ->
            ( { model | inputValue = value }, Cmd.none )

        AddTask _ ->
            ( { model
                | uid = model.uid + 1
                , inputValue = ""
                , taskList =
                    if String.isEmpty model.inputValue then
                        model.taskList

                    else
                        model.taskList ++ [ newTask model.inputValue model.uid ]
              }
            , Cmd.none
            )

        RemoveTask id ->
            ( { model | taskList = List.filter (\t -> t.id /= id) model.taskList }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Welcome todo list app" ]
        , viewInput "text" "Add new task" model.inputValue UpdateInput
        , button [ onClick (AddTask (newTask model.inputValue 1)) ] [ text "Add" ]
        , taskListView model.taskList
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


taskListView : List Task -> Html Msg
taskListView tasks =
    Keyed.ul [ class "todo-list" ] <|
        List.map taskView tasks


taskView : Task -> ( String, Html Msg )
taskView task =
    ( String.fromInt task.id, li [ onClick (RemoveTask task.id) ] [ text task.description ] )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
