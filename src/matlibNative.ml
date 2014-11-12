
open MatVec 

module NativeMatVec : Impl with type elem := float = 
struct 
  type vector = { size: int; vxs: float array }

  type matrix = { rows: int; cols: int; mxs: float array }

  (* vector operations *)
  module Vector : VectorOps with type elem := float and type vector := vector = 
  struct 
    let zero n = { size = n; vxs = Array.make n 0.0 }

    let create ?(initval=0.0) n = { size = n; vxs = Array.make n initval }

    let init ~f n = { size = n; vxs = Array.init n f }

    let size v = v.size 

    let from_array a = 
      { size = Array.length a; vxs = Array.copy a }

    let from_list l = 
      let len = List.length l in 
      let res = zero len in
      let rec set_elems i = function 
        | [] -> ()
        | x :: xs -> ( res.vxs.(i) <- x; set_elems (i+1) xs )
      in
      set_elems 0 l;
      res

    let copy v = { v with vxs = Array.copy v.vxs }

    let get v i = v.vxs.(i) 

    let set v i x = v.vxs.(i) <- x 

    let scale v s = { v with vxs = Array.map (fun x -> x *. s) v.vxs }

    let dot v1 v2 =   (* check dimensions *)
      let res = ref 0.0 in
      v1.vxs |> Array.iteri (fun i x -> res := !res +. x *. v2.vxs.(i));
      !res

    let add v1 v2 =   (* check dimensions *)
      let vxs = v1.vxs |> Array.mapi (fun i x -> x +. v2.vxs.(i)) in
      { size = v1.size; vxs }

    let norm2 v = 
      v.vxs |> Array.fold_left (fun s x -> s +. (x *. x)) 0.0 |> sqrt
  end

  (* matrix operations *)
  module Matrix : MatrixOps with type elem := float 
                             and type vector := vector 
                             and type matrix := matrix =
  struct 
    let create ?(initval=0.0) ~rows ~cols = 
      { rows; cols; mxs = Array.make (rows * cols) initval }

    let zero rows cols = create rows cols 
  
    let from_array a ~rows ~cols = (* verify if there *)
      if Array.length a <> (rows * cols) then failwith "from_array: Wrong dimensions for array"
      else { rows; cols; mxs = a } 

    let init ~f ~rows ~cols = 
      { rows; cols; mxs = Array.init (rows * cols) (fun i -> f (i mod cols) (i / cols)) }

    let copy m = { m with mxs = Array.copy m.mxs }

    let rows m = m.rows 

    let cols m = m.cols 

    let get m row col = m.mxs.(row * m.cols + col)

    let add m1 m2 = (* check dimensions *)
      let mxs = m1.mxs |> Array.mapi (fun i x -> x +. m2.mxs.(i)) in
      { rows = m1.rows; cols = m1.cols; mxs }

    let mult m1 m2 = 
      if m1.cols <> m2.rows then
        failwith "mult: Incompatible matrix dimensions"
      else
        let res = create m1.rows m2.cols in
        for i = 0 to res.rows - 1 do
          for j = 0 to res.cols - 1 do
            for p = 0 to m1.cols - 1 do
              res.mxs.(i * res.cols + j) <- 
                res.mxs.(i * res.cols + j) +. 
                m1.mxs.(i * m1.cols + p) *. m2.mxs.(p * m2.cols + j)
            done
          done
        done;
        res

    let mult_vec m v = 
      if m.cols <> v.size then
        failwith "mult_vec: Incompatible dimensions"
      else
        let res = Vector.zero m.cols in
        for i = 0 to m.rows - 1 do
          for j = 0 to m.cols - 1 do
            res.vxs.(i) <-  
              ((res.vxs.(i)) +. (v.vxs.(j)) *. m.mxs.(i * m.cols + j))
          done
        done;
        res
  end

end
