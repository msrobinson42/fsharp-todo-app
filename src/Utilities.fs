module List

let updateElementBy pred f xs =
  xs
  |> List.map (fun x -> if pred x then f x else x)
