(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
			     Spring 2017
 *)

open Mapfold ;;
  
let test () =
  assert ((negate_all []) = []);
  assert ((negate_all [1; -2; 0]) = [-1; 2; 0]);;

  (*  Additional tests go here... *)
  assert ((negate_all [3; 2]) = [-3; -2]);;
  assert ((negate_all [-3; -2] = [3; 2]));;

  (* sum *)
  assert ((sum [] = 0));;
  assert ((sum [1] = 1));;
  assert ((sum [4; 4; 4; 5; -9; 0] = 8));;
  assert ((sum [-6] = -6));;

  (*sum_rows*)

  assert ((sum_rows [] = []));;
  assert ((sum_rows [[1; 2]; [3; 4]] = [3; 7]));;
  assert ((sum_rows [[3; 9; -1; 7]] = [18]));;
  assert ((sum_rows [[2; 9; 5]; []; [1; 3 ;-5]] = [16; 0; -1]));;

  (*filter_odd*)
  assert((filter_odd []) = []);;
  assert((filter_odd [2; 2; 4; 8]) = []);;
  assert((filter_odd [1; 7; 5]) = [1; 7; 5]);;
  assert((filter_odd [2]) = []);;
  assert((filter_odd [1]) = [1]);;

  (*num_occurs*)
  assert ((num_occurs 0 [] = 0));;
  assert ((num_occurs 0 [4; 2; 6] = 0));;
  assert ((num_occurs 2 [9; 2; 2; 2; 6] = 3));;
  assert ((num_occurs (-1) [-1; 9; -1] = 2));;
  assert ((num_occurs 1 [] = 0));;

  (*super_sum*)
  assert ((super_sum []) = );;
  assert ((super_sum [[1]; [9; 4; 5]]) = 19);;
  assert ((super_sum [[2; -3]; [-2; 3]]) = 0 );;
  assert ((super_sum [[]; []]) = 0);;

  (*filter_range*)
  assert ((filter_range [] (1, 3)) = []);;
  assert ((filter_range [3; 5 ;8; 0] (0, 0)) = [0]);;
  assert ((filter_range [7; 4; 2; 10] (1, 1)) = []);;
  assert ((filter_range [1; 3; 4; 5; 2] (2, -3)) = []);;
  assert ((filter_range [8; 8; 29] (-1, 2)) = []);;

 (*floats_of_int*)
  assert ((floats_of_ints [] = []);;
  assert ((floats_of_ints [0] = [0.]);;
  assert ((floats_of_ints [9; 0; 8; 7; 3] = [9.; 0.; 8.; 7.; 3.]);;
  assert ((floats_of_ints [4; 8] = [4.; 8.]);;

  (*log10s*)

  assert ((log10s []) = []);;
  assert ((log10s [1.0; 10.0; -10.0]) = [Some 0.; Some 1.; None]);;
  assert ((log10s [100.; 10.; 0.]) = [Some 2.; Some 1.; None]);;
  assert ((log10s [1000.; -10.]) = [[Some 3.; None]);;

  (*deoptionalize*)
  assert ((deoptionalize [None])= []);;
  assert ((deoptionalize [Some 1])= [1]);;
  assert ((deoptionalize [Some (-1)])= [-1]);;
  assert ((deoptionalize [Some 3; None; Some 5; Some 10])= [3; 5; 10]);;

  (*some_sum*)
  assert ((some_sum []) = 0);;
  assert ((some_sum [Some 4; Some 3]) = 7);;
  assert ((some_sum [None]) = 0);;
  assert ((some_sum [Some 8; Some 3]) = 11);;

  (*mult_odds*)

  assert ((mult_odds []) = 1);;
  assert ((mult_odds [1; 0; 2; 0; -7]) = -7);;
  assert ((mult_odds [8; 0; 4; 2]) = );;
  assert ((mult_odds [9; 3; 5]) = 135);;

  (*concat*)
  assert ((concat []) = []);;
  assert ((concat [[5; 3; 6]; [9; 8; 5]]) = [5; 3; 6; 9; 8; 5]);;
  assert ((concat [[5; 3; 5]; []]) = [5; 3; 5]);;
  assert ((concat [[9; 3; 5]]) = [9; 3; 5]);;

(*filter_by_year*)
assert ((filter_by_year [("Tom", 2013)] 2013) = ["Tom"]);;
assert ((filter_by_year [("Jehn", 2013); ("Paul", 2014)] 2020) = []);;
assert ((filter_by_year [("Joe", 2010); ("Bob", 2010); ("Tom", 2013)] 2010) = ["Joe"; "Bob"]);;
assert ((filter_by_year [] 2018) = []);;



;;

test();;
print_endline "All tests passed.";;
