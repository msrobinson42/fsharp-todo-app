namespace Component

open Feliz

module TodoInput =

  let inputField (state: State) (dispatch: Msg -> unit) =
    let textInput =
      div [ Bulma.Control; Bulma.IsExpanded ] [
        Html.input [
          prop.classes [
            Bulma.Input
            Bulma.IsMedium
          ]
          prop.valueOrDefault state.NewTodo
          prop.onChange (SetNewTodo >> dispatch)
        ]
      ]

    let addTodoButton =
      div [ Bulma.Control ] [
        Html.button [
          prop.classes [
            Bulma.Button
            Bulma.IsPrimary
            Bulma.IsMedium
          ]
          prop.onClick (fun _ -> dispatch AddNewTodo)
          prop.children [
            Html.i [
              prop.classes [ FA.Fa; FA.FaPlus ]
            ]
          ]
        ]
      ]

    div [ Bulma.Field; Bulma.HasAddons ] [
      textInput
      addTodoButton
    ]
