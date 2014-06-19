
module type VectorOps = sig 
  type elem 
  type vector 

  val zero : int -> vector
  val create : ?initval : elem -> int -> vector 
  val init : f : (int -> elem) -> int -> vector 
  val size : vector -> int 
  val from_array : elem array -> vector 
  val from_list : elem list -> vector 
  val get : vector -> int -> elem 
  val set : vector -> int -> elem -> unit
  val scale : vector -> elem -> vector 
  val dot : vector -> vector -> elem 
  val add : vector -> vector -> vector
  val norm2 : vector -> elem 
end 

module type MatrixOps = sig 
  type elem
  type matrix 

  val create : ?initval : elem -> rows : int -> cols : int -> matrix 
  val zero : int -> int -> matrix 
  val init : f : (int -> int -> elem) -> rows : int -> cols : int -> matrix 
  val from_array : elem array -> rows : int -> cols : int -> matrix 
  val rows : matrix -> int
  val cols : matrix -> int
  val get : matrix -> int -> int -> elem 
  val add : matrix -> matrix -> matrix
  val mult : matrix -> matrix -> matrix
  (* val mult_vec : matrix -> vector -> vector *)
end 

module type Impl = sig 
  type elem 
  type vector
  type matrix

  module Vector : VectorOps with type elem := elem and type vector := vector
  module Matrix : MatrixOps with type elem := elem and type matrix := matrix  
end
