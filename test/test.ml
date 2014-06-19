open OUnit2
open Matlib.Native

let cmp_vec v1 v2 = 
  let rec cmp_contents i = 
    if i >= Vector.size v1 then 
      true
    else if cmp_float ~epsilon:0.0001 (Vector.get v1 i) (Vector.get v2 i) then 
      cmp_contents (i+1)
    else 
      false
  in
  (Vector.size v1 = Vector.size v2) && cmp_contents 0

let dot1 ctxt = 
  let v1 = Vector.from_list [1.0; 2.0; 3.0] in
  let v2 = Vector.init ~f:(fun i -> (float i) +. 4.0) 3 in
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.dot v1 v2) 32.0

let add1 ctxt = 
  let v1 = Vector.from_array [| 1.0; 2.0; 3.0 |] in
  let v2 = Vector.init ~f:(fun i -> (float i) +. 4.0) 3 in
  let vr = Vector.from_array [| 5.0; 7.0; 9.0 |] in
  assert_equal ~cmp:cmp_vec (Vector.add v1 v2) vr

let norm2 ctxt = 
  let v1 = Vector.from_array [| 3.52 |] in
  let v2 = Vector.from_array [| 3.0;  4.0 |] in
  let v3 = Vector.from_array [| 3.0; -4.0 |] in
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v1) 3.52; 
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v2) 5.00;
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v3) 5.00


let cmp_matrix m1 m2 = 
  let rec cmp_contents i j = 
    if j >= Matrix.cols m1 then 
      cmp_contents 0 (j+1)
    else if i >= Matrix.rows m1 then 
      true
    else if cmp_float ~epsilon:0.0001 (Matrix.get m1 i j) (Matrix.get m2 i j) then 
      cmp_contents (i+1) j
    else 
      false
  in
  (Matrix.rows m1 = Matrix.rows m2) && (Matrix.cols m1 = Matrix.cols m2) && cmp_contents 0 0

let matrix_add1 ctxt = 
  let m1 = Matrix.create 2 2 in
  let m2 = Matrix.from_array [| 1.0; 2.0; 3.0; 4.0 |] ~rows:2 ~cols:2 in
  let m3 = Matrix.add m1 m2 in
  let m4 = Matrix.init ~f:(fun i j -> (float i) *. 10.0 +. (float j)) ~rows:3 ~cols:3 in
  let m5 = Matrix.create 3 3 in 
  let m6 = Matrix.add m4 m5 in
  assert_equal ~cmp:cmp_matrix m2 m3;
  assert_equal ~cmp:cmp_matrix m4 m6

(*
let matvec_mult ctxt = 
  let m = Matrix.from_array [| 1.0; 2.0; 3.0; 4.0 |] ~rows:2 ~cols:2 in
  let v = Vector.from_array [| 5.0; 10.0 |] in
  let vr = Vector.from_array [| 25.0; 55.0 |] in
  let vm = Matrix.mult_vec m v in
  assert_equal ~cmp:cmp_vec vm vr 
*)

let suite = 
  "suite1" >:::
    ["dot product" >:: dot1;
     "vector addition" >:: add1;
     "2-norm" >:: norm2;
     (* "matrix-vector multiplication" >:: matvec_mult; *)
     "matrix addition" >:: matrix_add1
     ]

let () = 
  run_test_tt_main suite 
