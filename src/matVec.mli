module type Vector = sig
  type t
  type elem 

  val zero : int -> t
  val create : ?initval : elem -> int -> t
  val init : f : (int -> elem) -> int -> t
  val size : t -> int
  val from_array : elem array -> t
  val from_list : elem list -> t 
  val get : t -> int -> elem
  val set : t -> int -> elem -> unit 
  val scale : t -> elem -> t
  val dot : t -> t -> elem
  val add : t -> t-> t 
  val norm2 : t -> elem
end

module type Matrix = sig
  type t
  type elem
  type vec 

  val zero : int -> int -> t
  val create : ?initval : elem -> rows : int -> cols : int -> t
  val from_array : elem array -> rows : int -> cols : int -> t
  val init : f : (int -> int -> elem) -> rows : int -> cols : int -> t
  (* val sub_matrix : t -> rows : int -> cols : int -> t *)
  val rows : t -> int
  val cols : t -> int
  val get : t -> int -> int -> elem
  val add : t -> t -> t
  val mult : t -> t -> t 
  val mult_vec : t -> vec -> vec 
end
