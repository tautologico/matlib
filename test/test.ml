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
  let v2 = Vector.from_list [4.0; 5.0; 6.0] in
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.dot v1 v2) 32.0

let add1 ctxt = 
  let v1 = Vector.from_array [| 1.0; 2.0; 3.0 |] in
  let v2 = Vector.from_array [| 4.0; 5.0; 6.0 |] in
  let vr = Vector.from_array [| 5.0; 7.0; 9.0 |] in
  assert_equal ~cmp:cmp_vec (Vector.add v1 v2) vr

let norm2 ctxt = 
  let v1 = Vector.from_array [| 3.52 |] in
  let v2 = Vector.from_array [| 3.0;  4.0 |] in
  let v3 = Vector.from_array [| 3.0; -4.0 |] in
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v1) 3.52; 
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v2) 5.00;
  assert_equal ~cmp:(cmp_float ~epsilon:0.0001) (Vector.norm2 v3) 5.00

let suite = 
  "suite1" >:::
    ["dot product" >:: dot1;
     "vector addition" >:: add1;
     "2-norm" >:: norm2]

let () = 
  run_test_tt_main suite 
