let unfold f init =
  let rec go acc xs =
    match f acc with
    | Some x, acc -> go acc (x :: xs)
    | None, acc -> (List.rev xs, acc)
  in go init []

let seq_init (n: int) (f: int -> 'a): 'a Seq.t =
  let rec loop idx () =
    if idx < n then
      let v = f idx in
      Seq.Cons (v, loop (idx + 1))
    else
      Seq.Nil
  in loop 0

let seq_mapi f seq =
  let rec loop i seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, xs) -> Seq.Cons (f i x, loop (i + 1) xs)
  in loop 0 seq

let seq_iteri f seq =
  let rec loop i seq =
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons (x, xs) ->
      f i x;
      loop (i + 1) xs
  in loop 0 seq

let rec search f =
  function
  | [] -> None
  | x :: xs ->
    match f x with
    | Some res -> Some res
    | None -> search f xs

let null = function
  | [] -> true
  | _ -> false
