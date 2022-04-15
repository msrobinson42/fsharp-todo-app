module App

open System
open Elmish
open Elmish.React
open Feliz

let init () =
  { TodoList =
      [ { Id = Guid.NewGuid()
          Description = "Learn F#"
          Completed = true }
        { Id = Guid.NewGuid()
          Description = "Learn Elmish"
          Completed = false } ]
    NewTodo = ""
    TodosBeingEdited = []
    ListFilter =
      { Kind = All
        Predicate = fun _ -> true } }

let update (msg: Msg) (state: State) : State =
  match msg with
  | SetNewTodo todoText -> { state with NewTodo = todoText }

  | DeleteTodo todoId ->
    let nextTodoList =
      state.TodoList
      |> List.filter (fun todo -> todo.Id <> todoId)

    { state with TodoList = nextTodoList }

  | ToggleCompleted todoId ->
    let nextTodoList =
      state.TodoList
      |> List.map (fun todo ->
        if todo.Id = todoId then
          { todo with Completed = not todo.Completed }
        else
          todo)

    { state with TodoList = nextTodoList }

  | AddNewTodo when state.NewTodo = "" -> state

  | AddNewTodo ->
    let nextTodo =
      { Id = Guid.NewGuid()
        Description = state.NewTodo
        Completed = false }

    { state with
        NewTodo = ""
        TodoList = List.append state.TodoList [ nextTodo ] }

  | StartEditingTodo todoId ->
    let nextEditModel =
      state.TodoList
      |> List.tryFind (fun todo -> todo.Id = todoId)
      |> Option.map (fun todo ->
        { Id = todoId
          Description = todo.Description })
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
    | ([], _) -> state
    | (_, None) -> state
    | (_, Some todoBeingEdited) when todoBeingEdited.Description = "" -> state
    | (todosBeingEdited, Some todoBeingEdited) ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo ->
          if todo.Id = todoBeingEdited.Id then
            { todo with Description = todoBeingEdited.Description }
          else
            todo)

      let nextEditedList =
        todosBeingEdited
        |> List.filter (fun todo -> todo.Id <> todoBeingEdited.Id)

      { state with
          TodoList = nextTodoList
          TodosBeingEdited = nextEditedList }

  | SetEditedDescription (todoId, newText) ->
    let nextEditedTodos =
      state.TodosBeingEdited
      |> List.map (fun todo ->
        if todo.Id = todoId then
          { todo with Description = newText }
        else
          todo)

    { state with TodosBeingEdited = nextEditedTodos }

  | SetListFilter filterKind ->
    let predicate: ListFilterPredicate =
      match filterKind with
      | All -> fun _ -> true
      | Completed -> fun todo -> todo.Completed
      | Incompleted -> fun todo -> not todo.Completed

    { state with
        ListFilter =
          { Kind = filterKind
            Predicate = predicate } }

let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]

let appTitle =
  Html.p [
    prop.className Bulma.Title
    prop.text "Elmish To-Do List"
  ]

let inputField (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.classes [
      Bulma.Field
      Bulma.HasAddons
    ]

    prop.children [
      Html.div [
        prop.classes [
          Bulma.Control
          Bulma.IsExpanded
        ]

        prop.children [
          Html.input [
            prop.classes [
              Bulma.Input
              Bulma.IsMedium
            ]
            prop.valueOrDefault state.NewTodo
            prop.onChange (SetNewTodo >> dispatch)
          ]
        ]
      ]

      Html.div [
        prop.className Bulma.Control
        prop.children [
          Html.button [
            prop.classes [
              Bulma.Button
              Bulma.IsPrimary
              Bulma.IsMedium
            ]
            prop.onClick (fun _ -> dispatch AddNewTodo)
            prop.children [
              Html.i [
                prop.classes [ FA.Fa; "fa-plus" ]
              ]
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
        prop.className (
          if state.ListFilter.Kind = All then
            Bulma.IsActive
          else
            ""
        )
        prop.children [
          Html.a [ prop.text "All" ]
        ]
      ]

      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter Completed))
        prop.className (
          if state.ListFilter.Kind = Completed then
            Bulma.IsActive
          else
            ""
        )
        prop.children [
          Html.a [ prop.text "Completed" ]
        ]
      ]

      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter Incompleted))
        prop.className (
          if state.ListFilter.Kind = Incompleted then
            Bulma.IsActive
          else
            ""
        )
        prop.children [
          Html.a [ prop.text "Not Completed" ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  let renderTodo (dispatch: Msg -> unit) (todo: Todo) =
    div [ Bulma.Box ] [
      div [ Bulma.Columns
            Bulma.IsMobile
            Bulma.IsVcentered ] [
        div [ Bulma.Column ] [
          Html.p [
            prop.className Bulma.Subtitle
            prop.text todo.Description
          ]
        ]

        div [ Bulma.Column; Bulma.IsNarrow ] [
          div [ Bulma.Buttons ] [
            Html.button [
              prop.classes [
                Bulma.Button
                if todo.Completed then Bulma.IsSuccess
              ]
              prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
              prop.children [
                Html.i [
                  prop.classes [ FA.Fa; FA.FaCheck ]
                ]
              ]
            ]

            Html.button [
              prop.classes [
                Bulma.Button
                Bulma.IsPrimary
              ]
              prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
              prop.children [
                Html.i [
                  prop.classes [ FA.Fa; FA.FaEdit ]
                ]
              ]
            ]

            Html.button [
              prop.classes [
                Bulma.Button
                Bulma.IsDanger
              ]
              prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
              prop.children [
                Html.i [
                  prop.classes [ FA.Fa; FA.FaTimes ]
                ]
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

    let setEditedDescriptionFromString text = SetEditedDescription(currentId, text)

    div [ Bulma.Box ] [
      div [ Bulma.Field; Bulma.IsGrouped ] [
        div [ Bulma.Control; Bulma.IsExpanded ] [
          Html.input [
            prop.classes [
              Bulma.Input
              Bulma.IsMedium
            ]
            prop.valueOrDefault todoBeingEdited.Description
            prop.onTextChange (setEditedDescriptionFromString >> dispatch)
          ]
        ]

        div [ Bulma.Control; Bulma.Buttons ] [
          Html.button [
            prop.classes [
              Bulma.Button
              if not hasSameDescription then
                Bulma.IsPrimary
            ]
            prop.onClick (fun _ -> dispatch (ApplyEdit currentId))
            prop.children [
              Html.i [
                prop.classes [ FA.Fa; FA.FaSave ]
              ]
            ]
          ]

          Html.button [
            prop.classes [
              Bulma.Button
              Bulma.IsWarning
            ]
            prop.onClick (fun _ -> dispatch (CancelEdit currentId))
            prop.children [
              Html.i [
                prop.classes [ FA.Fa; FA.FaArrowRight ]
              ]
            ]
          ]
        ]
      ]
    ]

  let filteredTodos =
    state.TodoList
    |> List.filter state.ListFilter.Predicate

  Html.ul [
    prop.children [
      for todo in filteredTodos ->
        let editedTodoMaybe =
          state.TodosBeingEdited
          |> List.tryFind (fun edited -> todo.Id = edited.Id)

        match editedTodoMaybe with
        | Some editedTodo -> renderEditForm state dispatch editedTodo
        | _ -> renderTodo dispatch todo
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
