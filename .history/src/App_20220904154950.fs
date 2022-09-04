module App

open Elmish
open Elmish.React
open Feliz
open System

type Todo = {
  Id: Guid
  Description: string
  Completed: bool
}

type TodoBeingEdited = {
  Id: Guid
  Description: string
}

type State = { 
  NewTodo : string
  TodoList: Todo list
  TodoBeingEdited : TodoBeingEdited option
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | ToggleCompleted of Guid
  | DeleteTodo of Guid
  | CancelEdit
  | ApplyEdit
  | StartEditingTodo of Guid
  | SetEditedDescription of string

let init() =
    { 
      NewTodo = "" 
      TodoList = [ 
        { Id = Guid.NewGuid(); Description= "Learn F#"; Completed = false }
        { Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = false } ]
      TodoBeingEdited = None
    }

let update (msg: Msg) (state: State) =
  match msg with
    | SetNewTodo desc ->
        { state with NewTodo = desc }

    | DeleteTodo (todoId) ->
        let nextTodoList =
          state.TodoList
          |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

    | ToggleCompleted todoId ->
        let nextTodoList =
          state.TodoList
          |> List.map (fun todo ->
            if todo.Id = todoId
            then { todo with Completed = not todo.Completed }
            else todo)

        { state with TodoList = nextTodoList }

    | AddNewTodo when state.NewTodo = "" ->
        state

    | AddNewTodo ->
      let nextTodoId =
        match state.TodoList with
        | [ ] -> Guid.Empty
        | elems ->
            elems
            |> List.maxBy (fun todo -> todo.Id)
            |> fun todo -> Guid.NewGuid()

      let nextTodo =
        { Id = nextTodoId
          Description = state.NewTodo
          Completed = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

    | DeleteTodo todoId ->
        let nextTodoList =
          state.TodoList
          |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }

    | ToggleCompleted todoId ->
        let nextTodoList =
          state.TodoList
          |> List.map (fun todo ->
            if todo.Id = todoId
            then { todo with Completed = not todo.Completed }
            else todo)

        { state with TodoList = nextTodoList }

    | StartEditingTodo todoId ->
      let nextEditModel =
        state.TodoList
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })

      { state with TodoBeingEdited = nextEditModel }

    | CancelEdit ->
        { state with TodoBeingEdited = None }

    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
              state.TodoList
              |> List.map (fun todo ->
                  if todo.Id = todoBeingEdited.Id
                  then { todo with Description = todoBeingEdited.Description }
                  else todo)

            { state with TodoList = nextTodoList; TodoBeingEdited = None }

    | SetEditedDescription newText ->
        let nextEditModel =
          state.TodoBeingEdited
          |> Option.map (fun todoBeingEdited -> { todoBeingEdited with Description = newText })

        { state with TodoBeingEdited = nextEditModel }

let inputField (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded"]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault state.NewTodo
            prop.onTextChange (SetNewTodo >> dispatch)
          ]
        ]
      ]

      Html.div [
        prop.className "control"
        prop.children [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch AddNewTodo)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
  div [ "tabs"; "is-toggle"; "is-fullwidth" ] [
    Html.ul [
      Html.li [
        prop.className "is-active"
        prop.children [
          Html.a [
            prop.text "All"
          ]
        ]
      ]

      Html.li [
        Html.a [
          prop.text "Completed"
        ]
      ]

      Html.li [
        Html.a [
          prop.text "Not Completed"
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column" ] [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.classes [ "button"; if todo.Completed then "is-success"]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-primary" ]
            prop.onClick (fun _ -> dispatch (StartEditingTodo  todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do List"
  ]

let renderEditForm (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "field"; "is-grouped" ] [
      div [ "control"; "is-expanded" ] [
        Html.input [
          prop.classes [ "input"; "is-medium" ]
          prop.valueOrDefault todoBeingEdited.Description;
          prop.onTextChange (SetEditedDescription >> dispatch)
        ]
      ]

      div [ "control"; "buttons" ] [
        Html.button [
          prop.classes [ "button"; "is-primary"]
          prop.onClick (fun _ -> dispatch ApplyEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-save" ] ]
          ]
        ]

        Html.button [
          prop.classes ["button"; "is-warning"]
          prop.onClick (fun _ -> dispatch CancelEdit)
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-arrow-right"] ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    //for todo in state.TodoList -> renderTodo todo dispatch
    prop.children [
      for todo in state.TodoList ->
        match state.TodoBeingEdited  with
        | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
            renderEditForm todoBeingEdited dispatch
        | otherwise ->
            renderTodo todo dispatch
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      renderFilterTabs state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run