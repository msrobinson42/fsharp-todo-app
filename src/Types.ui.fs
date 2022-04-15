[<AutoOpen>]
module UI

open System

type State =
  { NewTodo: string
    TodoList: Todo list
    TodosBeingEdited: TodoBeingEdited list
    ListFilter: ListFilter }

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
