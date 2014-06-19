
open MatVec

module NativeVector : Vector with type elem := float 

module NativeMatrix : Matrix with type elem := float and type vec := NativeVector.t

