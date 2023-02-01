(* TEST - Life, the Universe, and Everything.
   https://www.spoj.com/problems/TEST/
 *)

let rec iter () =
  let num = Scanf.scanf "%d " (fun x -> x) in
  match num with
  | 42 -> ()
  | _ -> begin
      Printf.printf "%d\n" num;
      iter ()
    end

let () = iter ()
