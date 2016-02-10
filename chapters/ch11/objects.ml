(* #use "objects.ml";; *)

let stack init = object
    val mutable v = init

    method pop = 
        match v with
        | hd :: tl ->
            v <- tl;
            Some hd
        | [] -> None

    method push hd = 
        v <- hd :: v
end;;

let t = object
    method pop = Some (Float.to_int (Time.to_float (Time.now ())))
end;;

let print_pop st = Option.iter ~f:(printf "Popped: %s\n") st#pop;;

let imm_stack init = object
    val v = init

    method pop =
        match v with
        | hd :: tl -> Some (hd, {<v = tl>})
        | [] -> None

    (* {<.. ..>} produces a copy of the current object with *)
    (* all of the same fields except the one that's specified *)
    method push hd = {<v = hd :: v>}
end;;


