namespace Component

open Feliz

module Tabs =
  let renderFilterTabs (state: State) (dispatch: Msg -> unit) =
    let renderTab kind =
      Html.li [
        prop.onClick (fun _ -> dispatch (SetListFilter kind))
        prop.className (
          if state.ListFilter.Kind = kind then
            Bulma.IsActive
          else
            ""
        )
        prop.children [
          Html.a [ prop.text (string kind) ]
        ]
      ]

    let tabKinds = [ All; Completed; Incompleted ]

    div [ Bulma.Tabs
          Bulma.IsToggle
          Bulma.IsFullwidth ] [
      Html.ul (tabKinds |> List.map renderTab)
    ]
