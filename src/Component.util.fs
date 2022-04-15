[<AutoOpen>]
module Component.Utilities

open Feliz

let div (classes: string list) (children: Fable.React.ReactElement list) =
  Html.div [
    prop.classes classes
    prop.children children
  ]