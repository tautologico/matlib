
open MatVec

module NativeVector : Vector with type elem := float = 
struct
  type t = { size: int; elems: float array }

  let create ?(initval=0.0) n = { size = n; elems = Array.create n initval }

  let zero n = create n

  let get v i = v.elems.(i)

  let scale v s = { v with elems = Array.map (fun x -> x *. s) v.elems }

  let dot v1 v2 =   (* check dimensions *)
    let res = ref 0.0 in
    Array.iteri (fun i x -> res := !res +. x *. v2.elems.(i)) v1.elems;
    !res
end
