(* BULK - The Bulk!
   https://www.spoj.com/problems/BULK/
*)

let ( %> ) f g x = g (f x)

module Point = struct
  type t = {x : int; y : int; z : int}

  (** [input ()] inputs a point from the standard input. *)
  let input () = Scanf.scanf "%d %d %d " (fun x y z -> {x; y; z})
end

module Face = struct
  (** [input ()] inputs a bulk face from the standard input. *)
  let input () =
    let n = Scanf.scanf "%d " (fun x -> x) in
    let rec input_face_iter face = function
      | 0 -> face
      | n -> input_face_iter (Point.input () :: face) (n - 1)
    in input_face_iter [] n

  (** [is_perpendicular_to_z face] checks if [face] is perpendicular to the Z
      axis. *)
  let is_perpendicular_to_z face =
    let {Point.z} = List.hd face in
    List.for_all (fun {Point.z = z'} -> z' = z) face

  (* The underlying functions assume that the faces are perpendicular to the Z
     axis. *)

  (** [shift lst] shifts the first element of the list [lst] to its end. *)
  let shift lst =
    List.tl lst @ [List.hd lst]

  (** [edges face] returns a list of edges of [face]. *)
  let edges face =
    let open Point in
    List.map2
      (fun {x = x1; y = y1} {x = x2; y = y2} -> ((x1, y1), (x2, y2)))
      face
      (shift face)

  (** [lower_left_point face] returns the lower left point of [face]. *)
  let lower_left_point face =
    List.fold_left
      (fun p p' ->
         Point.(if p.y < p'.y || (p.y = p'.y && p.x < p'.x) then p else p'))
      (List.hd face)
      (List.tl face)

  (** [covers face1 face2] checks if [face1] covers [face2].  Here I use the
      fact that faces do not overlap, so if the number of vertical edges of
      [face1] that are to the left of [face2] is odd, then [face1] covers
      [face2]. *)
  let covers face1 face2 =
    let open Point in
    let {x = lx; y = ly} = lower_left_point face2 in
    let edges_to_the_left =
      edges face1
      |> List.fold_left
        (fun counter ((x1, y1), (x2, y2)) ->
           let ymin = min y1 y2 and ymax = max y1 y2 in
           counter + (if x1 = x2 && x1 <= lx && ymin <= ly && ly < ymax then 1 else 0))
        0
    in edges_to_the_left mod 2 = 1

  (** [area face] returns the area of [face]. *)
  let area =
    edges
    %> List.map (fun ((x1, y), (x2, _)) -> (x2 - x1) * y)
    %> List.fold_left ( + ) 0
    %> abs
end

module Bulk = struct
  (** [input ()] inputs a bulk from the standard input. *)
  let input () =
    let n = Scanf.scanf "%d " (fun x -> x) in
    let rec input_bulk_iter bulk = function
      | 0 -> bulk
      | n ->
        let face = Face.input () in
        input_bulk_iter
          (* skip faces non perpendicular to the Z axis *)
          (if Face.is_perpendicular_to_z face then face :: bulk else bulk)
          (n - 1)
    in input_bulk_iter [] n

  (** [face_signs faces] assigns signs to [faces] whether they contribute
      positive or negative volume. *)
  let rec face_signs = function
    | [] -> []
    | face :: faces ->
      let signs = face_signs faces in
      let cover_face_sign = signs |> List.find_opt (fun (face', _) -> Face.covers face' face) in
      let sign = match cover_face_sign with
        | Some (_, sign) -> -sign
        | None -> 1
      in
      (face, sign) :: signs

  (** [calc_units bulk] calculates the number of units [bulk] is composed of. *)
  let calc_units =
    (* sort faces on the basis of height along the Z axis *)
    List.sort
      (fun face1 face2 ->
         let {Point.z = z1} = List.hd face1
         and {Point.z = z2} = List.hd face2
         in compare z1 z2)
    (* assign a sign to each face whether it contributes positive or negative volume *)
    %> face_signs
    (* calculate volume for each face *)
    %> List.map
      (fun (face, sign) ->
         let {Point.z} = List.hd face in
         sign * (Face.area face) * z)
    (* calculate total volume *)
    %> List.fold_left ( + ) 0
end

let main () =
  let t = Scanf.scanf "%d " (fun x -> x) in
  for _ = 1 to t do
    Bulk.input ()
    |> Bulk.calc_units
    |> Printf.printf "The bulk is composed of %d units.\n"
  done

let () = main ()
