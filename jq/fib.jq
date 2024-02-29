def fib(a):
  def fib_helper(prev; curr):
    if curr > a then
      .
    else
      . + [curr] | fib_helper(curr; prev + curr)
    end;
  fib_helper(0; 1);

def myfib(a):
  if a == 0
  then [0]
  else if a == 1
       then myfib(a - 1) + [1]
       else myfib(a - 1) as $r
         | $r + [$r[-1] + $r[-2]]
       end
  end ;

def f($a): body;

def g(a): a as $a | body;
