namespace Component

open Feliz

module TodoList =
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
