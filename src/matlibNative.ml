
open Matlib

module NativeVector : Vector = struct
  type t = { size: int; elems: float array }

  type elem = float 

  let create ?(initval=0.0) n = { size = n; elems = Array.create n initval }

  let zero n = create n

  let get v i = v.elems.(i)

  let scale v s = { v with elems = Array.map (fun x -> x *. s) v.elems }

  let dot v1 v2 =   (* check dimensions *)
    let res = ref 0.0 in
    for i = 0 to v1.size-1 do
      res := !res +. v1.elems.(i) *. v2.elems.(i)
    done;
    !res
end
