module App

open Elmish
open Elmish.React
open Feliz

type Todo = {
  Id: int
  Description: string
  Completed: bool
}

type State = {
  TodoList: Todo list
  NewTodo : string
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | ToggleCompleted of int
  | DeleteTodo of int

let init() =
    { 
      TodoList = [ 
        {Id=1; Description= "Learn F#"; Completed = false} 
        ]
      NewTodo = ""  }

let update (msg: Msg) (state: State) =
  match msg with
  | SetNewTodo desc ->
      { state with NewTodo = desc }

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

  | AddNewTodo when state.NewTodo = "" ->
      state

  | AddNewTodo ->
      let nextTodoId =
        match state.TodoList with
        | [ ] -> 1
        | elems ->
            elems
            |> List.maxBy (fun todo -> todo.Id)
            |> fun todo -> todo.Id + 1

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

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    for todo in state.TodoList ->
      Html.li [
        prop.classes ["box"; "subtitle"]
        prop.text todo.Description
      ]
  ]

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do List"
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run