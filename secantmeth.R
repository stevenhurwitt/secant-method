#functions to implement secant method:

#new_x returns the secant method formula run with two values of x
new_x = function(int_x0, int_x1, f){
  return(int_x1 - f(int_x1)*((int_x1 - int_x0)/(f(int_x1) - f(int_x0))))
}

#regulafalsi takes two values of x, a function and tolerance
#runs the secant method formula until the two x values
#are below the tolerance
regulafalsi = function(int_x0, int_x1, f, eps){
  new_x0 = new_x(int_x0, int_x1, f)
  new_x1 = new_x(int_x1, new_x0, f)
  while (abs(new_x0 - new_x1) > eps){
    new_x0 = new_x(new_x0, new_x1, f)
    new_x1 = new_x(new_x1, new_x0, f)
  }
  return(new_x1)
}


###example
int_x0 = 1
int_x1 = 5
f = function(x){
  return(x^.5 + 3*log(x) - 5)
}
regulafalsi(1,5,f, .001)
