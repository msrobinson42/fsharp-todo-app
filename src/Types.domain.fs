[<AutoOpen>]
module Domain

open System

type Todo =
  { Id: Guid
    Description: string
    Completed: bool }

type TodoBeingEdited = { Id: Guid; Description: string }

type ListFilterKind =
  | All
  | Completed
  | Incompleted

type ListFilterPredicate = Todo -> bool

type ListFilter =
  { Kind: ListFilterKind
    Predicate: ListFilterPredicate }