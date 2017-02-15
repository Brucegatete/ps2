(*
			 CS 51 Problem Set 2
		 A Language for Symbolic Mathematics
			     Spring 2017
*)

(*......................................................................
Before reading this code (or in tandem), read the problem set 2
writeup. It provides context and crucial information for completing
the problems.

The type definition of the expression type can be found at the top of
expressionLibrary.ml. You will be using this ADT for this part of the
problem set. Note that we also provide you with unary and binary
operators that operate on expressions.

The module ExpressionLibrary is opened to provide you with access to
the expression data type and helpful functions that you may use for
this part of the problem set.
......................................................................*)

open ExpressionLibrary ;;

(*......................................................................
Tips for problem 2:

1. READ THE WRITEUP, particularly for the derivative function.
 
2. Use the type definitions provided at the top of
   expressionLibrary.ml as a reference, and don't change any of the
   code in the file. It provides functions such as "parse" and
   "to_string_smart" that will be helpful in this part of the problem
   set.
......................................................................*)

(*......................................................................
Problem 2.1: The function "contains_var" tests whether an 
expression contains a variable "x". For example:

# contains_var (parse "x^4")
- : bool = true
# contains_var (parse "4+3") 
- : bool = false
......................................................................*)
(* This function iterates over the expression and if the expression is a constant it has to return false if its a variable it returns true.
For binary and unary functions it looks for the expression(s) and distinguishes them using the same criteria, hence being recursive*)
let rec contains_var (e : expression) : bool = 
match e with
Num _ -> false
| Var -> true
| Unop (_u, e1) ->  contains_var e1
|Binop (_b, e1, e2) -> 
 if contains_var e1 = false && contains_var e2 = false then false
else true;;
  

(*......................................................................
Problem 2.2: The function "evaluate" evaluates an expression for a
particular value of x. Don't worry about specially handling the
"divide by zero" case. For example:

# evaluate (parse "x^4 + 3") 2.0
- : float = 19.0
......................................................................*)

let rec evaluate (e : expression) (x : float) : float =
  match e, x with
  | Num a, _ -> a
  | Var, x -> x
  | Unop (u, e1), x ->
      (match u with
        | Sin -> sin (evaluate e1 x)
        | Cos -> cos (evaluate e1 x)
        | Ln -> log (evaluate e1 x)
        | Neg -> (evaluate e1 x) *. (-1.))
  | Binop (b, e1, e2), x ->
       (match b with
       | Add -> (evaluate e1 x) +. (evaluate e2 x)
       | Sub -> (evaluate e1 x) -. (evaluate e2 x)
       | Mul -> (evaluate e1 x) *. (evaluate e2 x)
       | Div -> (evaluate e1 x) /. (evaluate e2 x)
       | Pow -> (evaluate e1 x) ** (evaluate e2 x));;
        

  

(*......................................................................
Problem 2.d: The "derivative" function returns the expression that
represents the derivative of the argument expression. We provide the
skeleton of the implementation here along with a few of the cases;
you're responsible for filling in the remaining parts that implement
the derivative transformation provided in the figure in the
writeup. See the writeup for instructions.
......................................................................*)

let rec derivative (e : expression) : expression =
  match e with
  | Num _ -> Num 0.
  | Var -> Num 1.
  | Unop (u, e1) ->
     (match u with
      | Sin -> Binop (Mul, Unop (Cos, e1), derivative e1)
      | Cos -> Binop (Mul, Unop (Neg, Unop (Sin, e1)), derivative e1)
      | Ln -> Binop (Div, derivative e1, e1)
      | Neg -> Unop(Neg,derivative e1))
  | Binop (b, e1, e2) ->
     match b with
     | Add -> Binop (Add, derivative e1, derivative e2)
     | Sub -> Binop (Sub, derivative e1, derivative e2)
     | Mul -> Binop (Add, Binop (Mul, e1, derivative e2),
                    Binop (Mul, derivative e1, e2))
     | Div -> Binop (Div, Binop (Sub, Binop (Mul, derivative e1, e2), Binop (Mul, derivative e2, e1)), Binop (Mul, e2, e2))
     | Pow -> 
        (* split based on whether the exponent has any variables *)
        if derivative e2 = Num 0.
        then Binop (Mul, e2, Binop (Pow, e1, Binop(Sub, e2, Num 1.)))
        else Binop (Mul, Binop (Pow, e1, e2), Binop (Add, Binop (Mul, derivative e2, Unop(Ln, e1)), Binop (Div, Binop (Mul, derivative e1, e2), e1)));;



(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
  print_string ("Checking expression: " ^ strs ^ "\n");
  let parsed = parse strs in
  (print_string "contains variable : ";
   print_string (string_of_bool (contains_var parsed));
   print_endline " ";
   print_string "Result of evaluation: "; 
   print_float (evaluate parsed xval);
   print_endline " ";
   print_string "Result of derivative: ";
   print_endline " ";
   print_string (to_string (derivative parsed));
   print_endline " ") ;;
  
(*......................................................................
Problem 2.4: Zero-finding. See writeup for instructions.
......................................................................*)

let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
	: float option = 
  match e, g, epsilon, lim with
  _e, _g, _epsilon, 0 -> None
  | e, g, epsilon, lim -> 

  if abs_float (evaluate e (g -. ((evaluate e g)/. evaluate (derivative e) g))) < epsilon then Some g 
  else find_zero e (g -. ((evaluate e g)/. evaluate (derivative e) g)) epsilon (lim - 1);;
  
(*......................................................................
Problem 2.5: Challenge problem -- exact zero-finding. This problem is
not counted for credit and is not required. Just leave it
unimplemented if you do not want to do it. See writeup for
instructions.
......................................................................*)

let rec find_zero_exact (e:expression) : expression option =
  failwith "find_zero_exact not implemented" ;;

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete.  We care
about your responses and will use them to help guide us in creating
future assignments.
......................................................................*)

let minutes_spent_on_part2 () : int = 720 ;;
