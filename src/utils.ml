let unfold f init =
  let rec loop acc xs =
    match f acc with
    | Some x, acc -> loop acc (x :: xs)
    | None, acc -> (List.rev xs, acc)
  in loop init []

let unzip_with f =
  let rec loop lef rig =
    function
    | [] -> (List.rev lef, List.rev rig)
    | x :: xs ->
      let (l, r) = f x in
      loop (l :: lef) (r :: rig) xs
  in loop [] []

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

let seq_replicate n x =
  let rec loop n () =
    if n > 0 then
      Seq.Cons (x, loop (n - 1))
    else
      Seq.Nil
  in loop n

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
