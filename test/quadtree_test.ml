open Quadtree

let rec insert_entities o n pos =
  if 0 < n then (
    let x = 10. in
    let y = 10. in
    let o = Quadtree.add o n pos in
    insert_entities o (n - 1) pos
  ) else (
    o
  )

let () =
  let open Core.Printf in
  let o = Quadtree.create {x = 0.0; y = 0.} 32.0 in
  let o = insert_entities o 3 {x=(0.0); y=(0.)} in
  Quadtree.print o;

  let res = Quadtree.within o {x=1.;y=1.} 1.5 in


  printf "found %d entities\n" (Core.List.length res);

  Core.List.iter res ~f:(fun (id, pos) ->
                   printf "%d\n" id)
