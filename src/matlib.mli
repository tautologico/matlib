module type Impl = sig

  module type Vector = sig
    type t
    type elem 

    val zero : int -> t
    val create : ?initval : elem -> int -> t
    val get : t -> int -> elem
    val scale : t -> elem -> t
    val dot : t -> t -> t 
  end

  module type Matrix = sig 
    type t
    type elem

    val zero : int -> int -> t
    val create : ?initval : elem -> rows : int -> cols : int -> t
    val get : t -> int -> int -> elem
    val add : t -> t -> t
    val mult : t -> t -> t 
  end

end
