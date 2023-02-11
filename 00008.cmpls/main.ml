(* CMPLS - Complete the Sequence!
   https://www.spoj.com/problems/CMPLS/
*)

let complete_sequence seq c =
  (* Using the method of finite differences. *)

  let s = Array.length seq in
  let m = Array.make_matrix (s + c) (s + c) 0 in

  for i = 0 to s - 1 do
    m.(0).(i) <- seq.(i)
  done;

  for i = 1 to s - 1 do
    for j = 0 to (s - i - 1) do
      m.(i).(j) <- m.(i - 1).(j + 1) - m.(i - 1).(j)
    done
  done;

  for j = 1 to c do
    m.(s - 1).(j) <- m.(s - 1).(j - 1)
  done;

  for i = s - 2 downto 0 do
    for j = s - i to s - i + c - 1 do
      m.(i).(j) <- m.(i).(j - 1) + m.(i + 1).(j - 1)
    done
  done;

  Array.sub m.(0) s c

let main () =
  let t = Scanf.scanf "%d " (fun x -> x) in
  for _ = 1 to t do
    let s, c = Scanf.scanf "%d %d " (fun x y -> x, y) in
    let seq = Array.init s (fun _ -> Scanf.scanf "%d " (fun x -> x)) in

    let cseq = complete_sequence seq c in

    Array.iter (Printf.printf "%d ") cseq;
    Printf.printf "\n"
  done

let () = main ()
