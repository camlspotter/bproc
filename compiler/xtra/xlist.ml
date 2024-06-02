include List

let rec drop n xs =
  match (n, xs) with
  | 0, xs -> xs
  | n, _ when n < 0 -> assert false
  | n, _ :: xs -> drop (n - 1) xs
  | _, [] -> []

let split_at n xs =
  let rec split_at_ n st xs =
    if n <= 0 then (st, xs)
    else
      match xs with [] -> (st, []) | x :: xs -> split_at_ (n - 1) (x :: st) xs
  in
  let r, dropped = split_at_ n [] xs in
  (rev r, dropped)

let take n xs = fst @@ split_at n xs

let () = assert (split_at 10 [ 1; 2; 3 ] = ([ 1; 2; 3 ], []))

let uniq_sorted eq xs =
  let rec f = function
    | [] -> []
    | x :: (y :: _ as xs) when eq x y -> f xs
    | x :: xs -> x :: f xs
  in
  f xs

let () =
  assert (uniq_sorted ( = ) [ 1; 2; 3; 4; 5 ] = [ 1; 2; 3; 4; 5 ]);
  assert (uniq_sorted ( = ) [ 1; 1; 2; 3; 3; 4; 5; 5 ] = [ 1; 2; 3; 4; 5 ])

let rev_filter_map f lst =
  fold_left (fun st x -> match f x with Some v -> v :: st | None -> st) [] lst

(** mapMaybe of Haskell *)
let filter_map f lst = rev @@ rev_filter_map f lst

(** Tail recursive verions *)

let ( @ ) xs ys = rev_append (rev xs) ys

let rev_concat xs =
  let rec f acc = function [] -> acc | x :: xs -> f (rev_append x acc) xs in
  f [] xs

let concat xs = rev @@ rev_concat xs

let map f xs =
  let rec loop acc = function
    | [] -> rev acc
    | x :: xs -> loop (f x :: acc) xs
  in
  loop [] xs

let map2 f xs ys =
  let rec loop acc xs ys =
    match (xs, ys) with
    | [], [] -> rev acc
    | x :: xs, y :: ys -> loop (f x y :: acc) xs ys
    | _ -> raise (Invalid_argument "List.map2")
  in
  loop [] xs ys

let combine xs ys =
  let rec loop acc xs ys =
    match (xs, ys) with
    | [], [] -> rev acc
    | x :: xs, y :: ys -> loop ((x, y) :: acc) xs ys
    | _ -> raise (Invalid_argument "List.map2")
  in
  loop [] xs ys

let mapi f xs =
  let rec loop i acc = function
    | [] -> rev acc
    | x :: xs -> loop (i + 1) (f i x :: acc) xs
  in
  loop 0 [] xs

let rev_concat_map f xs =
  let rec loop acc = function
    | [] -> acc
    | x :: xs -> loop (rev_append (f x) acc) xs
  in
  loop [] xs

let concat_map f xs = rev @@ rev_concat_map f xs

let rec is_prefix xs ys =
  match (xs, ys) with
  | [], _ -> Some ys
  | x :: xs, y :: ys when x = y -> is_prefix xs ys
  | _ -> None

let take_while_map p xs =
  let rec loop st = function
    | [] -> (List.rev st, [])
    | x :: xs -> (
        match p x with
        | None -> (List.rev st, x :: xs)
        | Some y -> loop (y :: st) xs)
  in
  loop [] xs

let rev_split xys =
  let rec f xs ys = function
    | [] -> (xs, ys)
    | (x, y) :: xys -> f (x :: xs) (y :: ys) xys
  in
  f [] [] xys

let split xys =
  let rxs, rys = rev_split xys in
  (List.rev rxs, List.rev rys)

let partition_map f xs =
  let ls, rs =
    List.fold_left
      (fun (ls, rs) x ->
        match f x with `Left x -> (x :: ls, rs) | `Right x -> (ls, x :: rs))
      ([], [])
      xs
  in
  (List.rev ls, List.rev rs)
