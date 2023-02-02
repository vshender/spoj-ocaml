(* PRIME1 - Prime Generator
   https://www.spoj.com/problems/PRIME1/
 *)

let is_prime n =
  let sn = float n |> sqrt |> int_of_float in
  let rec is_prime_inner m =
    m > sn || (n mod m <> 0 && is_prime_inner (m + 2))
  in
  n > 1 && (n = 2 || (n mod 2 <> 0 && is_prime_inner 3))

let main () =
  let k = Scanf.scanf "%d " (fun x -> x) in
  for _ = 1 to k do
    let m, n = Scanf.scanf "%d %d " (fun m n -> m, n) in
    for i = m to n do
      if is_prime i then
        Printf.printf "%d\n" i
    done;
    print_newline ();
  done

let () = main ()
