module App

open System
open Elmish
open Elmish.React
open Feliz
open Component

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

let update (msg: Msg) (state: State) : State = Update.Todo.updateTodoState msg state

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      Header.appTitle
      TodoInput.inputField state dispatch
      Tabs.renderFilterTabs state dispatch
      TodoList.todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
