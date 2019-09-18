port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Keyed as Keyed


main : Program (Maybe Model) Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "Todo List", body = [ view model ] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



---- MODEL ----


type alias Model =
    { uid : Int
    , inputValue : String
    , editInputValue : String
    , taskList : List Task
    , filter : String
    }


type alias Task =
    { id : Int
    , description : String
    , isCompleted : Bool
    , editing : Bool
    }


emptyModel : Model
emptyModel =
    { uid = 0
    , inputValue = ""
    , editInputValue = ""
    , taskList = []
    , filter = "All"
    }


newTask : String -> Int -> Task
newTask desc id =
    { id = id
    , description = desc
    , isCompleted = False
    , editing = False
    }


emptyTask : Task
emptyTask =
    { id = 0
    , description = ""
    , isCompleted = False
    , editing = False
    }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeModel =
    ( Maybe.withDefault emptyModel maybeModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = UpdateInput String
    | AddTask Task
    | RemoveTask Int
    | EditTask Int
    | ToggleComplete Int
    | SaveTask Int
    | OnEditTask String
    | ChangeFilter String


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

        EditTask id ->
            ( { model
                | taskList =
                    List.map
                        (\t ->
                            if t.id == id then
                                { t | editing = True }

                            else
                                t
                        )
                        model.taskList
                , editInputValue = .description (Maybe.withDefault emptyTask (List.head (List.filter (\t -> t.id == id) model.taskList)))
              }
            , Cmd.none
            )

        ToggleComplete id ->
            ( { model
                | taskList =
                    List.map
                        (\t ->
                            if t.id == id then
                                { t | isCompleted = not t.isCompleted }

                            else
                                t
                        )
                        model.taskList
              }
            , Cmd.none
            )

        OnEditTask value ->
            ( { model | editInputValue = value }, Cmd.none )

        SaveTask id ->
            ( { model
                | taskList =
                    List.map
                        (\t ->
                            if t.id == id then
                                { t | editing = False, description = model.editInputValue }

                            else
                                t
                        )
                        model.taskList
              }
            , Cmd.none
            )

        ChangeFilter filter ->
            ( { model | filter = filter }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Welcome todo list app" ]
        , viewInput "text" "Add new task" model.inputValue UpdateInput
        , button [ onClick (AddTask (newTask model.inputValue 1)) ] [ text "Add" ]
        , taskListView model
        , actionBarView model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


taskListView : Model -> Html Msg
taskListView model =
    Keyed.ul [ class "todo-list" ] <|
        List.map (taskView model.editInputValue) <|
            getTasksByFilter model.filter model.taskList


taskView : String -> Task -> ( String, Html Msg )
taskView editInputValue task =
    ( String.fromInt task.id
    , if task.editing == True then
        li []
            [ input [ type_ "text", value editInputValue, onInput OnEditTask ] []
            , button [ onClick (SaveTask task.id) ] [ text "Save" ]
            , button [ onClick (RemoveTask task.id) ] [ text "Delete" ]
            ]

      else
        li []
            [ span [ classList [ ( "completed", task.isCompleted ), ( "task", True ) ], onClick (ToggleComplete task.id) ] [ text task.description ]
            , button [ onClick (EditTask task.id) ] [ text "Edit" ]
            , button [ onClick (RemoveTask task.id) ] [ text "Delete" ]
            ]
    )


actionBarView : Model -> Html Msg
actionBarView model =
    div []
        [ span [] [ text ((String.fromInt <| List.length model.taskList) ++ " items left") ]
        , button [ onClick (ChangeFilter "All") ] [ text "All" ]
        , button [ onClick (ChangeFilter "Completed") ] [ text "Completed" ]
        ]



--UTILS


getTasksByFilter : String -> List Task -> List Task
getTasksByFilter filter tasks =
    case filter of
        "All" ->
            tasks

        "Completed" ->
            List.filter
                (\t -> t.isCompleted)
                tasks

        _ ->
            tasks
