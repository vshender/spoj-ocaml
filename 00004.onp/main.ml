(* ONP - Transform the Expression
   https://www.spoj.com/problems/ONP/
 *)

let priority = function
  | '(' -> 0
  | '+' -> 1
  | '-' -> 2
  | '*' -> 3
  | '/' -> 4
  | '^' -> 5
  | _ -> invalid_arg "priority"

let transform expr =
  let buf = Buffer.create 256
  and operators = Stack.create () in

  let process_stack f =
    while f () do
      Buffer.add_char buf @@ Stack.pop operators
    done
  in

  String.iter (fun c ->
      match c with
      | 'a' .. 'z' ->
        Buffer.add_char buf c
      | '(' ->
        Stack.push '(' operators
      | ')' ->
        process_stack (fun () -> Stack.top operators <> '(');
        ignore (Stack.pop operators)
      | _ ->
        process_stack (fun () -> not (Stack.is_empty operators) && priority (Stack.top operators) >= priority c);
        Stack.push c operators)
    expr;
  process_stack (fun () -> not (Stack.is_empty operators));

  Buffer.contents buf

let main () =
  let n = Scanf.scanf "%d " Fun.id in
  for _ = 1 to n do
    let expr = Scanf.scanf "%s " Fun.id in
    Printf.printf "%s\n" @@ transform expr
  done

let () = main ()
