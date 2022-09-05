module App

open Elmish
open Elmish.React
open Feliz
open System
open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css", Naming.PascalCase>

type Todo = {
  Id: Guid
  Description: string
  Completed: bool
}

type TodoBeingEdited = {
  Id: Guid
  Description: string
  SaveDisabled: bool
}

type Filter =
  | All
  | NotCompleted
  | Completed

type State = { 
  NewTodo : string
  TodoList: Todo list
  TodoBeingEdited : TodoBeingEdited list 
  Filter: Filter
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | ToggleCompleted of Guid
  | DeleteTodo of Guid
  | CancelEdit of Guid
  | ApplyEdit of Guid
  | StartEditingTodo of Guid
  | SetEditedDescription of string*Guid
  | ShowAll
  | ShowCompleted
  | ShowNotCompleted

let init() =
    { 
      NewTodo = "" 
      TodoList = [ 
        { Id = Guid.NewGuid(); Description= "Learn F#"; Completed = false }
        { Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = false } ]
      TodoBeingEdited = []
      Filter = All
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
        let nextEditModel: TodoBeingEdited option =
          state.TodoList
          |> List.tryFind (fun todo -> todo.Id = todoId)
          |> Option.map (fun todo -> { 
              Id = todoId; Description = todo.Description; SaveDisabled = true })
        match nextEditModel with
        | Some todo -> { state with TodoBeingEdited = state.TodoBeingEdited @ [todo] }
        | _ -> failwith "Todo was not found"
      

    | CancelEdit todoId->
        let nextTodoBeingEdited =
          state.TodoBeingEdited
          |> List.filter (fun todo -> todo.Id <> todoId)
        { state with TodoBeingEdited = nextTodoBeingEdited }

    | ApplyEdit todoId->
        let todoBeingEdited =
          state.TodoBeingEdited 
          |> List.tryFind (fun todo -> todo.Id = todoId)
        
        match todoBeingEdited with
        | None -> state
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
        | Some todoBeingEdited ->
            let nextTodoList =
              state.TodoList
              |> List.map (fun todo ->
                  if todo.Id = todoBeingEdited.Id then 
                    { todo with Description = todoBeingEdited.Description }
                  else 
                    todo)

            { state with 
                TodoList = nextTodoList; 
                TodoBeingEdited = state.TodoBeingEdited 
                    |> List.filter (fun todo -> todo.Id <> todoId)
               }

    | SetEditedDescription (newText: string, todoId:Guid) ->
        
        let nextEditModel =
          state.TodoBeingEdited
          |> List.map (fun todo ->
            if todo.Id = todoId then 
               { todo with Description = newText; SaveDisabled = false }
            else todo)


        { state with TodoBeingEdited = nextEditModel }

    | ShowAll ->
        {state with Filter = All}

    | ShowCompleted ->
        {state with Filter = Completed}

    | ShowNotCompleted ->
        {state with Filter = NotCompleted}

let inputField (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ Bulma.Field; Bulma.HasAddons ]
    prop.children [
      Html.div [
        prop.classes [ Bulma.Control; Bulma.IsExpanded]
        prop.children [
          Html.input [
            prop.classes [ Bulma.Input; Bulma.IsMedium ]
            prop.valueOrDefault state.NewTodo
            prop.onTextChange (SetNewTodo >> dispatch)
          ]
        ]
      ]

      Html.div [
        prop.className Bulma.Control
        prop.children [
          Html.button [
            prop.classes [ Bulma.Button; Bulma.IsPrimary; Bulma.IsMedium ]
            prop.onClick (fun _ -> dispatch AddNewTodo)
            prop.children [
              Html.i [ prop.classes [ Bulma.Fa; "fa-plus" ] ]
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
  div [ Bulma.Tabs; Bulma.IsToggle; Bulma.IsFullwidth ] [
    Html.ul [
      Html.li [
        if state.Filter = All then prop.className "is-active"
        prop.children [
          Html.a [
            prop.text "All"
            prop.onClick (fun _ -> dispatch ShowAll)
          ]
        ]
      ]

      Html.li [
        if state.Filter = Completed then prop.className Bulma.IsActive
        prop.children [
          Html.a [
            prop.text "Completed"
            prop.onClick (fun _ -> dispatch ShowCompleted)
          ]
        ]      
      ]

      Html.li [
        if state.Filter = NotCompleted then prop.className "is-active"
        prop.children [
          Html.a [
            prop.onClick (fun _ -> dispatch ShowNotCompleted)
            prop.text "Not Completed"
          ]
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
          //prop.onTextChange  ((SetEditedDescription) todoBeingEdited.Id >> dispatch) //todoBeingEdited.Id
          prop.onTextChange (fun text -> dispatch (SetEditedDescription (text, todoBeingEdited.Id)))
        ]
      ]

      div [ "control"; "buttons" ] [
        Html.button [
          prop.classes [ "button"; "is-primary"; ]
          prop.onClick (fun _ -> dispatch (ApplyEdit todoBeingEdited.Id))
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-save" ] ]
          ]
          prop.disabled todoBeingEdited.SaveDisabled
        ]

        Html.button [
          prop.classes ["button"; "is-warning"]
          prop.onClick (fun _ -> dispatch (CancelEdit todoBeingEdited.Id))
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-arrow-right"] ]
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  Html.ul [
    prop.children [
      let todoList = 
        match state.Filter with
        | All -> state.TodoList
        | Completed -> state.TodoList |> List.filter (fun todo -> todo.Completed = true)
        | NotCompleted -> state.TodoList |> List.filter (fun todo -> todo.Completed = false)

      for todo in todoList do
          let todoBeingEdited = 
            state.TodoBeingEdited 
            |> List.tryFind (fun t -> t.Id = todo.Id)
          match todoBeingEdited with
            | Some todoBeingEdited -> renderEditForm todoBeingEdited dispatch
            | None -> renderTodo todo dispatch    
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