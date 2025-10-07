(* The Smallest Free Number *)

(*
 *
 * An array-based solution
 *
 *)

(* diff removes the element in the `vs` from `us`
 * The book uses the \\ operator*)
let diff us vs = List.filter (fun x -> not (List.mem x vs)) us

(*test the diff*)
let _ = assert (diff [ 1; 2; 3 ] [ 2 ] = [ 1; 3 ])
let _ = assert (diff [] [ 2 ] = [])

(*range returns an array of list from lo to hi*)
let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi

let head = function
  | [] -> None
  | x :: _ -> Some x

let minfree xs =
  let rec find x =
    match head (diff (range 0 x) xs) with
    | None -> find (x + 1)
    | Some v -> v
  in
  find 0

let _ = assert (minfree [ 0; 1; 2; 6; 9 ] = 3)
let _ = assert (minfree [ 0 ] = 1)
