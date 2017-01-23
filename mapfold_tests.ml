(* 
			 CS 51 Problem Set 2
	    Higher Order Functional Programming -- Testing
			     Spring 2017
 *)

open Mapfold ;;
  
let test () =
  assert ((negate_all []) = []);
  assert ((negate_all [1; -2; 0]) = [-1; 2; 0])

  (*  Additional tests go here... *)

;;

test();;
print_endline "All tests passed.";;
