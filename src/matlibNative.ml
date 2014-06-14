
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

(* row-major native matrix *)
module NativeMatrix : Matrix with type elem := float = 
struct
  type t = { rows: int; cols: int; elems: float array }

  let create ?(initval=0.0) ~rows ~cols = 
    { rows; cols; elems = Array.create (rows * cols) initval }

  let zero rows cols = create rows cols 
  
  let from_array a ~rows ~cols = (* verify if there *)
    if Array.length a <> (rows * cols) then failwith "from_array: Wrong dimensions for array"
    else { rows; cols; elems = a } 

  let rows m = m.rows 

  let cols m = m.cols 

  let get m row col = m.elems.(row * m.cols + col)

  let add m1 m2 = (* check dimensions *)
    let elems = m1.elems |> Array.mapi (fun i x -> x +. m2.elems.(i)) in
    { rows = m1.rows; cols = m1.cols; elems }

  let mult m1 m2 = 
    if m1.cols <> m2.rows then
      failwith "mult: Incompatible matrix dimensions"
    else
      let res = create m1.rows m2.cols in
      for i = 0 to res.rows - 1 do
        for j = 0 to res.cols - 1 do
          for p = 0 to m1.cols - 1 do
            res.elems.(i * res.cols + j) <- 
              res.elems.(i * res.cols + j) +. m1.elems.(i * m1.cols + p) *. m2.elems.(p * m2.cols + j)
          done
        done
      done;
    res
end
