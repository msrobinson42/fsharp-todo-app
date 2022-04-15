namespace Component

open Feliz

module Header =

  let appTitle =
    Html.p [
      prop.className Bulma.Title
      prop.text "Elmish To-Do List"
    ]
