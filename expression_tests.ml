open Expression ;;
open ExpressionLibrary ;;

let test () =
	(*contains_var*)
  assert (contains_var (parse "x+3") = true);
  assert (not (contains_var (parse "2") =));

  (*  Additional tests go here... *)
  assert ((contains_var (parse "sin x"))= true );;
  assert ((contains_var (parse "ln 3")) = false) ;;
  assert ((contains_var (parse "cos 90 - 3")) = false);;
  assert ((contains_var (parse "1 + 3")) = false);;

  (*evaluate*)
  assert ((evaluate (parse "10 + x") 90.) = 100.);;
  assert ((evaluate (parse "2 * x") 3.) = 6.);;
  assert ((evaluate (parse " 3") 3.) = 3.);;
  assert ((evaluate (parse "2^x") 3.) = 8.);;

  (*derivative*)
  assert ((derivative (parse "3 * x")) = Binop (Add, Binop (Mul, Num 3., Num 1.), Binop (Mul, Num 0., Var)));;
  assert ((derivative (parse "ln 3")) = Binop (Div, Num 0., Num 3.));;
  assert ((derivative (parse "0")) = Num 0.);;
  assert ((derivative (parse "x^3")) = Binop (Mul, Num 3., Binop (Pow, Var, Binop (Sub, Num 3., Num 1.))));;

  (*find_zero*)
  assert ((find_zero (parse "x^3") 1. 0.6 10)) = Some 1.);;
  assert ((find_zero (parse "sin x") 2. 0.01 10)) = Some 3.26618627756910618);;
  assert ((find_zero (parse "ln x") 0.9 0.001 10)) = Some 0.994824464092043614);;
  assert ((find_zero (parse "cos x^4") 1. 0.0001 20)) = Some (-1.47007221911864749));;



test();;
print_endline "All tests passed.";;
