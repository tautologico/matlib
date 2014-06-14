
module type Impl = sig
  type elem
  module Vector : MatVec.Vector with type elem := elem
  module Matrix : MatVec.Matrix with type elem := elem
end

module Native : Impl with type elem := float 
