
module type Impl = sig
  type elem
  module Vector : MatVec.Vector with type elem := elem

  (* module Matrix : MatVec.Matrix  *)
end

module Native : Impl with type elem := float = struct
  module Vector = MatlibNative.NativeVector 
end
