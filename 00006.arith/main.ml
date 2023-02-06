(* ARITH - Simple Arithmetics
   https://www.spoj.com/problems/ARITH/
 *)

open Num

(** [print_rjust len str] prints a right-justified string [str] of length
    [len]. *)
let print_rjust len str =
  Printf.printf "%s%s\n" (String.make (len - String.length str) ' ') str

(** [print_dashes len num] prints [num] right-aligned dashes in a string of
    length [len]. *)
let print_dashes len num =
  print_rjust len (String.make num '-')

(** [operator c] returns an operator corresponding to the given character. *)
let operator = function
  | "+" -> ( +/ )
  | "-" -> ( -/ )
  | _ -> failwith "unknown operator"

(** [max3 a b c] returns the greater of the three arguments. *)
let max3 a b c = max (max a b) c

(** [print_add_sub_calc op left right] prints calculation of addition or
    substraction of the two given numbers [left] and [right].  The intended
    operation is specified by [op]. *)
let print_add_sub_calc op left right =
  let res = operator op (num_of_string left) (num_of_string right) |> string_of_num in
  let res_len = String.length res in

  let rightop = op ^ right in
  let rightop_len = String.length rightop in
  let len = max3 (String.length left) rightop_len res_len in

  print_rjust len left;
  print_rjust len rightop;
  print_dashes len (max rightop_len res_len);
  print_rjust len res

(** [print_mul_calc left right] prints calculation of multiplication of the
    two given numbers [left] and [right]. *)
let print_mul_calc left right =
  let leftnum = num_of_string left and rightnum = num_of_string right in
  let res = leftnum */ rightnum |> string_of_num in
  let res_len = String.length res in

  let rightop = "*" ^ right in
  let rightop_len = String.length rightop in
  let len = max3 (String.length left) rightop_len res_len in

  print_rjust len left;
  print_rjust len rightop;

  if rightop_len > 2 then
    for i = 0 to rightop_len - 2 do
      let digit = rightop.[rightop_len - i - 1] in
      let interim = leftnum */ (num_of_string @@ String.make 1 digit) |> string_of_num in
      if i = 0 then
        print_dashes len (max rightop_len (String.length interim));
      print_rjust (len - i) interim;
      if i = rightop_len - 2 then
        print_dashes len (max (String.length interim + i) res_len);
    done
  else
    print_dashes len (max rightop_len res_len);

  print_rjust len res

(** [print_calc op left right] prints calculation of the operation [op] on the
    two given numbers [left] and [right]. *)
let print_calc op left right =
  match op with
  | "+" | "-" -> print_add_sub_calc op left right
  | "*" -> print_mul_calc left right
  | _ -> failwith "unknown operator"

let main () =
  let re = Str.regexp "\\([0-9]+\\)\\([-+*]\\)\\([0-9]+\\)" in
  let t = Scanf.scanf "%d " (fun x -> x) in
  for _ = 1 to t do
    let expr = Scanf.scanf "%s " (fun x -> x) in
    let _ = Str.string_match re expr 0 in
    print_calc
      (Str.matched_group 2 expr)
      (Str.matched_group 1 expr)
      (Str.matched_group 3 expr);
    Printf.printf "\n"
  done

let () = main ()
