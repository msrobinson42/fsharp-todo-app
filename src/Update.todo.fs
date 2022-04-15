[<AutoOpen>]
module Update.Todo

open System

let setTodo state text = { state with NewTodo = text }

let deleteTodo state id =
  let newList =
    state.TodoList
    |> List.filter (fun t -> t.Id <> id)

  { state with TodoList = newList }

let toggleCompleted state id =
  let newList =
    state.TodoList
    |> List.updateElementBy (fun t -> t.Id = id) (fun t -> { t with Completed = not t.Completed })

  { state with TodoList = newList }

let addNewTodo state =
  match state.NewTodo with
  | "" -> state
  | text ->
    let newTodo =
      { Id = Guid.NewGuid()
        Description = text
        Completed = false }

    let newTodoList = List.append state.TodoList [ newTodo ]

    { state with
        NewTodo = ""
        TodoList = newTodoList }

let startEditingTodo state id =
  let editableTodo (t: Todo) =
    { Id = id; Description = t.Description }

  let editableTodos =
    state.TodoList
    |> List.tryFind (fun todo -> todo.Id = id)
    |> Option.map editableTodo
    |> Option.fold (fun tail head -> head :: tail) state.TodosBeingEdited

  { state with TodosBeingEdited = editableTodos }

let cancelEditingTodo state id =
  let editableTodos =
    state.TodosBeingEdited
    |> List.filter (fun t -> t.Id <> id)

  { state with TodosBeingEdited = editableTodos }

let applyEdit state id =
  let editedTodo =
    state.TodosBeingEdited
    |> List.tryFind (fun t -> t.Id = id)

  match state.TodosBeingEdited, editedTodo with
  | [], _
  | _, None -> state
  | _, Some todo when todo.Description = "" -> state
  | editedTodos, Some todo ->
    let newList =
      state.TodoList
      |> List.updateElementBy (fun t -> t.Id = todo.Id) (fun t -> { t with Description = todo.Description })

    let newEditedTodos =
      editedTodos
      |> List.filter (fun t -> t.Id <> todo.Id)

    { state with
        TodoList = newList
        TodosBeingEdited = newEditedTodos }

let updateEditedDescription state id text =
  let newEditableTodos =
    state.TodosBeingEdited
    |> List.updateElementBy (fun t -> t.Id = id) (fun t -> { t with Description = text })

  { state with TodosBeingEdited = newEditableTodos }

let setListFilter state kind =
  let pred: ListFilterPredicate =
    match kind with
    | All -> fun _ -> true
    | Completed -> fun todo -> todo.Completed
    | Incompleted -> fun todo -> not todo.Completed

  { state with ListFilter = { Kind = kind; Predicate = pred } }

let updateTodoState (msg: Msg) (state: State) : State =
  match msg with
  | SetNewTodo text -> setTodo state text
  | DeleteTodo todoId -> deleteTodo state todoId
  | ToggleCompleted todoId -> toggleCompleted state todoId
  | AddNewTodo -> addNewTodo state
  | StartEditingTodo todoId -> startEditingTodo state todoId
  | CancelEdit todoId -> cancelEditingTodo state todoId
  | ApplyEdit todoId -> applyEdit state todoId
  | SetEditedDescription (todoId, newText) -> updateEditedDescription state todoId newText
  | SetListFilter kind -> setListFilter state kind
