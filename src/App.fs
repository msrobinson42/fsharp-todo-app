module App

open System
open Elmish
open Elmish.React
open Feliz

type Todo = {
  Id : Guid
  Description : string
  Completed : bool
}

type TodoBeingEdited = {
  Id: Guid
  Description: string
}

type ListFilterKind = All | Completed | Incompleted
type ListFilterPredicate = Todo -> bool
type ListFilter = {
  Kind: ListFilterKind
  Predicate: ListFilterPredicate
}

type State = { 
  NewTodo : string
  TodoList : Todo list
  TodosBeingEdited: TodoBeingEdited list
  ListFilter: ListFilter
}

type Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditingTodo of Guid
    | SetEditedDescription of (Guid * string)
    | SetListFilter of ListFilterKind

let init() =
    { TodoList = [ 
        {Id = Guid.NewGuid(); Description = "Learn F#"; Completed = true }
        {Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = false }
      ]
      NewTodo = ""
      TodosBeingEdited = [ ]
      ListFilter = {Kind = All; Predicate = fun _ -> true } }

let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo todoText -> 
      { state with NewTodo = todoText }

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
      let nextTodo =
        { Id = Guid.NewGuid()
          Description = state.NewTodo
          Completed = false }

      { state with 
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo] }

    | StartEditingTodo todoId ->
      let nextEditModel =
        state.TodoList
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })
        |> Option.toList
      
      { state with TodosBeingEdited = state.TodosBeingEdited @ nextEditModel }

    | CancelEdit todoId ->
      let filteredTodosBeingEdited =
        state.TodosBeingEdited
        |> List.filter (fun todo -> todo.Id <> todoId)

      { state with TodosBeingEdited = filteredTodosBeingEdited }

    | ApplyEdit todoId ->
      let todoBeingEdited =
        state.TodosBeingEdited
        |> List.tryFind (fun todo -> todo.Id = todoId)

      match (state.TodosBeingEdited, todoBeingEdited) with
      | ([ ], _) -> state
      | (_, None) -> state
      | (_, Some todoBeingEdited) when todoBeingEdited.Description = "" -> state
      | (todosBeingEdited, Some todoBeingEdited) ->
        let nextTodoList =
          state.TodoList
          |> List.map (fun todo -> 
            if todo.Id = todoBeingEdited.Id
            then { todo with Description = todoBeingEdited.Description }
            else todo)

        let nextEditedList =
          todosBeingEdited
          |> List.filter (fun todo -> todo.Id <> todoBeingEdited.Id)
          
        { state with TodoList = nextTodoList; TodosBeingEdited = nextEditedList }

    | SetEditedDescription (todoId, newText) ->
      let nextEditedTodos =
        state.TodosBeingEdited
        |> List.map (fun todo -> 
          if todo.Id = todoId 
          then { todo with Description = newText } 
          else todo)

      { state with TodosBeingEdited = nextEditedTodos }

    | SetListFilter filterKind ->
      let predicate: ListFilterPredicate =
        match filterKind with
        | All -> fun _ -> true
        | Completed -> fun todo -> todo.Completed
        | Incompleted -> fun todo -> not todo.Completed

      { state with ListFilter = { Kind = filterKind; Predicate = predicate } }

let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]

let appTitle =
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do List"
  ]

let inputField (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded" ]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault state.NewTodo
            prop.onChange (SetNewTodo >> dispatch)
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

let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
  div [ "tabs"; "is-toggle"; "is-fullwidth" ] [
    Html.ul [
      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter All))
        prop.className (if state.ListFilter.Kind = All then "is-active" else "")
        prop.children [
          Html.a [
            prop.text "All"
          ]
        ]
      ]

      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter Completed))
        prop.className (if state.ListFilter.Kind = Completed then "is-active" else "")
        prop.children [
          Html.a [
            prop.text "Completed"
          ]
        ]
      ]

      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter Incompleted))
        prop.className (if state.ListFilter.Kind = Incompleted then "is-active" else "")
        prop.children [
          Html.a [
            prop.text "Not Completed"
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  let renderTodo (dispatch: Msg -> unit) (todo: Todo) =
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
              prop.classes [ "button"; if todo.Completed then "is-success" ]
              prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
              prop.children [
                Html.i [ prop.classes [ "fa"; "fa-check" ] ]
              ]
            ]

            Html.button [
              prop.classes [ "button"; "is-primary" ]
              prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
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

  let renderEditForm (state: State) (dispatch: Msg -> unit) (todoBeingEdited: TodoBeingEdited) =
    let hasSameDescription =
      state.TodoList
      |> List.tryFind (fun todo -> todo.Id = todoBeingEdited.Id)
      |> Option.filter (fun todo -> todo.Description = todoBeingEdited.Description)
      |> Option.isSome

    let currentId = todoBeingEdited.Id

    let setEditedDescriptionFromString text = SetEditedDescription (currentId, text)
      
    div [ "box" ] [
      div [ "field"; "is-grouped" ] [
        div [ "control"; "is-expanded" ] [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault todoBeingEdited.Description
            prop.onTextChange (setEditedDescriptionFromString >> dispatch)
          ]
        ]

        div [ "control"; "buttons" ] [
          Html.button [
            prop.classes [ "button"; if not hasSameDescription then "is-primary" ]
            prop.onClick (fun _ -> dispatch (ApplyEdit currentId))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-save" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-warning" ]
            prop.onClick (fun _ -> dispatch (CancelEdit currentId))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-arrow-right" ] ]
            ]
          ]
        ]
      ]
    ]

  let filteredTodos = state.TodoList |> List.filter state.ListFilter.Predicate

  Html.ul [
    prop.children [
      for todo in filteredTodos ->
        let editedTodoMaybe = 
          state.TodosBeingEdited
          |> List.tryFind (fun edited -> todo.Id = edited.Id)

        match editedTodoMaybe with
        | Some editedTodo -> 
          renderEditForm state dispatch editedTodo
        | _ -> 
          renderTodo dispatch todo
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