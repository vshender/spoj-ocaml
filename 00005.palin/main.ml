(* PALIN - The Next Palindrome
   https://www.spoj.com/problems/PALIN/
 *)

let ( %> ) f g x = g (f x)

(** [take n l] returns up to the [n] first elements from the list [l], if
    available. *)
let take n l =
  let rec take_inner accu n l =
    if n = 0 then List.rev accu
    else
      match l with
      | [] -> List.rev accu
      | h :: t -> take_inner (h :: accu) (n - 1) t
  in take_inner [] n l

(** [drop n l] returns [l] without the first [n] elements, or the empty list if
    [l] contains less than [n] elements. *)
let rec drop n l =
  if n = 0 then l
  else
    match l with
    | [] -> []
    | _ :: t -> drop (n - 1) t

let zero = Char.code '0'

(** [list_of_num_string num] returns a list of digits given a string
    representation of a number. *)
let list_of_num_string num =
  let rec iter accu i =
    if i = -1 then
      accu
    else
      iter (Char.code num.[i] - zero :: accu) (i - 1)
  in iter [] (String.length num - 1)

(* Since OCaml 4.07
let list_of_num_string =
  String.to_seq %> Seq.map (fun x -> Char.code x - zero) %> List.of_seq
*)

(** [num_string_of_list nl] returns a string representation of a number given a
    list of digits. *)
let num_string_of_list nl =
  let buf = Buffer.create (List.length nl) in
  List.iter (( + ) zero %> Char.chr %> Buffer.add_char buf) nl;
  Buffer.contents buf

(** [inc_num_list nl] increments a number represented as a list of digits.
    Returns the incremented number and a flag indicating whether a new digit
    has been added. *)
let inc_num_list nl =
  let rec inc_num_list_iter accu carry = function
    | [] ->
      if carry > 0 then
        List.rev (1 :: accu), true
      else
        List.rev accu, false
    | h :: t ->
      let d = (h + carry) mod 10 and c = (h + carry) / 10 in
      inc_num_list_iter (d :: accu) c t
  in inc_num_list_iter [] 1 nl

let next_palin num =
  let len = String.length num in
  let lnum = list_of_num_string num in
  (* Split the number into two parts. *)
  let left = lnum |> take ((len + 1) / 2) |> List.rev
  and right = lnum |> drop (len / 2) in
  (* Increment the left part if it's less than or equal to the right one. *)
  let left_inc, carry = if left <= right then inc_num_list left else left, false in
  (* Truncate the left and right parts if it's needed. *)
  let left' = if carry && len mod 2 > 0 then List.tl left_inc else left_inc in
  let right' = if (len + if carry then 1 else 0) mod 2 > 0 then List.tl left' else left' in
  List.rev_append left' right' |> num_string_of_list

let main () =
  let n = Scanf.scanf "%d " (fun x -> x) in
  for _ = 1 to n do
    let num = Scanf.scanf "%s " (fun x -> x) in
    Printf.printf "%s\n" (next_palin num)
  done

let () = main ()
