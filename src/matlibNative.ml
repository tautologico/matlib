
open MatVec

module NativeVector : Vector with type elem := float = 
struct
  type t = { size: int; elems: float array }

  let create ?(initval=0.0) n = { size = n; elems = Array.create n initval }

  let size v = v.size 

  let zero n = create n

  let from_array a = 
    { size = Array.length a; elems = Array.copy a }  (* arrays are mutable, so we must copy *)

  let from_list l = 
    let len = List.length l in 
    let res = zero len in
    let rec set_elems i = function 
      | [] -> ()
      | x :: xs -> ( res.elems.(i) <- x; set_elems (i+1) xs )
    in
    set_elems 0 l;
    res

  let get v i = v.elems.(i)

  let scale v s = { v with elems = Array.map (fun x -> x *. s) v.elems }

  let dot v1 v2 =   (* check dimensions *)
    let res = ref 0.0 in
    v1.elems |> Array.iteri (fun i x -> res := !res +. x *. v2.elems.(i));
    !res

  let add v1 v2 =   (* check dimensions *)
    let elems = v1.elems |> Array.mapi (fun i x -> x +. v2.elems.(i)) in
    { size = v1.size; elems }

  let norm2 v = 
    v.elems |> Array.fold_left (fun s x -> s +. (x *. x)) 0.0 |> sqrt
end
