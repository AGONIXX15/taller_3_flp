declarar-recursivo(
    @helper=procedimiento(@i)
     {
        Si ((@m * @i) <= @n) 
        {
          Si ((@n - (@m * @i)) < @m) {(@n - (@m * @i))} sino 
          {
            evaluar @helper (@n,@m, (@i + 1)) finEval
          }
        } sino {@n}
      }) {
        evaluar @helper(0) finEval
      }

declarar-recursivo(
@helper=procedimiento(@n,@m,@i) {
        Si ((@m * @i) <= @n) 
        {Si ((@n ~ (@m * @i)) < @m) {(@n ~ (@m * @i))} sino {
            evaluar @helper (@n,@m, (@i + 1)) finEval
          }} sino {@n}
      };
@modulo=procedimiento(@n, @m) {
    evaluar @helper(@n,@m, 0) finEval
  };
@div10=procedimiento(@n) {
    ((@n ~ evaluar @modulo(@n,10) finEval) / 10)
  };
@sumarDigitos=procedimiento(@n) {
    Si (@n == 0) {0} sino {(evaluar @modulo(@n,10) finEval +
    evaluar @sumarDigitos (evaluar @div10 (@n) finEval) finEval)}
  };
) 
  {
    evaluar @sumarDigitos (111) finEval
  }


declarar-recursivo (
  @potencia=procedimiento(@n, @m) {
      Si (@m == 0) {1} sino {(@n * evaluar @potencia(@n,sub1(@m)) finEval)}
    };
) {
    evaluar @potencia(2,8) finEval
  }

declarar-recursivo (
  @sumaRango=procedimiento(@a, @b) {
      Si (@a == @b) {@b} sino {(@a + evaluar @sumaRango(add1(@a), @b) finEval)}
    };
) {
    evaluar @sumaRango(2,5) finEval
  }