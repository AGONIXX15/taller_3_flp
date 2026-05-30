#lang eopl
;*************************************
;TALLER 3 FLP
;*************************************
;Samuel banguero 2418671
;Sebastian rubio 2459628
;Camilo Riscanevo 2459753
;
;repositorio git hub: https://github.com/AGONIXX15/taller_3_flp.git

; ESPECIFICACIÓN LÉXICA
; Define los tokens que el scanner reconocerá en el código fuente.
; Cada regla indica: (nombre-token (patrón) acción)
(define scanner-spec-simple-interpreter
'(
  ; Ignora espacios en blanco, tabs y saltos de línea
  (white-sp (whitespace) skip)
  
  ; Identificadores: inician con '@' seguido de letras o dígitos
  ; Ejemplos válidos: @x, @var1, @miVariable
  (identifier ("@" (or letter digit) (arbno (or letter digit))) symbol)

  ; Números enteros positivos: 42, 100
  (number (digit (arbno digit)) number)

  ; Números enteros negativos: -42, -100
  (number ("-" digit (arbno digit)) number)

  ; Números decimales positivos: 3.14, 2.0
  (number (digit (arbno digit) "." digit (arbno digit)) number)

  ; Números decimales negativos: -3.14, -2.0
  (number ("-" digit (arbno digit) "." digit (arbno digit)) number)

  ; Texto: inicia con letra, seguido de letras, dígitos o guión bajo
  ; Ejemplos válidos: hola, mi_texto, palabra123
  ; Las comillas se manejan en la gramática, no aquí
  (text ((or "_" letter) (arbno (or letter digit "_" ":"))) string)
  ))

; ESPECIFICACIÓN GRAMATICAL
; Define la estructura sintáctica del lenguaje usando los tokens anteriores.
; Cada regla tiene la forma: (no-terminal (producción) nombre-constructor)
(define grammar-simple-interpreter
  '(
    ; Un programa es exactamente una expresión
    (program (expression) a-program)

    ; Literal numérico: cualquier número definido en el scanner
    (expression (number) number-exp)

    ; Variable: un identificador que empieza con @
    (expression (identifier) var-exp)

    ; Literal de texto: cualquier texto entre comillas dobles
    ; Las comillas se escapan con \" en la gramática
    (expression ("\"" text "\"") text-lit)

    ; Operación binaria en notación infija: (exp1 op exp2)
    ; Ejemplo: (3 + 4), (@x * @y)
    (expression ("(" expression prim-binary expression ")") binary-exp)

    ; Operación unaria en notación prefija: op(exp)
    ; Ejemplo: longitud(@texto), add1(5)
    (expression (prim-unary "(" expression ")") unary-exp)

    ; CONDICIONAL
    ; Si <condición> { <rama-verdadera> } sino { <rama-falsa> }
    ; La condición usa aritmética de booleanos: 0 es falso, otro valor es verdadero
    (expression ("Si" expression "{" expression "}" "sino" "{" expression "}") condicional-exp)

    ; PROCEDIMIENTOS (funciones anónimas)
    ; procedimiento (@param1, @param2, ...) { cuerpo }
    ; Crea una cerradura que captura el ambiente actual
    (expression ("procedimiento" "(" (separated-list identifier ",") ")" "{" expression "}") procedimiento-ex)

    ; VARIABLES LOCALES 
    ; declarar (@x=expr1; @y=expr2; ...) { cuerpo }
    ; Declara variables locales disponibles solo dentro del cuerpo
    (expression ("declarar" "(" (arbno identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)

    ; APLICACIÓN DE PROCEDIMIENTOS
    ; evaluar <proc> (arg1, arg2, ...) finEval
    ; Llama a un procedimiento con los argumentos dados
    (expression ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)

    ; VARIABLES LOCALES RECURSIVAS
    ; Como declarar, pero permite que los procedimientos se llamen a sí mismos
    ; Necesario para implementar funciones recursivas como factorial, fibonacci, etc.
    (expression ("declarar-recursivo" "(" (arbno identifier "=" expression ";") ")" "{" expression "}") variableLocalRec-exp)

    ; PRIMITIVAS BINARIAS
    ; Operadores que reciben dos operandos

    (prim-binary ("+") prim-binary-add)          ; Suma numérica
    (prim-binary ("~") prim-binary-sub)          ; Resta (~ en vez de - para evitar confusión con negativos)
    (prim-binary ("/") prim-binary-div)          ; División
    (prim-binary ("*") prim-binary-mul)          ; Multiplicación
    (prim-binary ("==") prim-binary-equal)       ; Igualdad: retorna 1 si son iguales, 0 si no
    (prim-binary ("<") prim-binary-less)         ; Menor que: retorna 1 o 0
    (prim-binary ("<=") prim-binary-less-equal)  ; Menor o igual: retorna 1 o 0
    (prim-binary (">") prim-binary-greater)      ; Mayor que: retorna 1 o 0
    (prim-binary (">=") prim-binary-greater-equal) ; Mayor o igual: retorna 1 o 0
    (prim-binary ("!=") prim-binary-different)   ; Diferente: retorna 1 o 0
    (prim-binary ("concat") prim-binary-concat)  ; Concatenación de strings

    ; PRIMITIVAS UNARIAS
    ; Operadores que reciben un solo operando

    (prim-unary ("longitud") prim-unary-length)  ; Longitud de un string
    (prim-unary ("add1") prim-unary-add1)        ; Suma 1 al número
    (prim-unary ("sub1") prim-unary-sub1)        ; Resta 1 al número
    (prim-unary ("neg") prim-unary-neg)          ; Negación booleana: 0->1, otro->0
    ))



; TIPO DE DATO AMBIENTES
; Un ambiente es una estructura que asocia nombres de variables con sus valores.
; Se implementa como una lista enlazada

(define-datatype environment environment?
   ; Ambiente vacío: representa el final de la cadena de búsqueda
  (empty-env-record)
   ; Ambiente extendido: agrega un marco con una lista de símbolos y sus valores al ambiente anterior
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  ; Ambiente recursivo: almacena procedimientos que pueden referenciarse a sí mismos
  ; Guarda nombres, parámetros y cuerpos por separado para reconstruir la cerradura al buscar
  (recursive-extended-env-record (proc-names (list-of symbol?))
                                 (ids (list-of (list-of symbol?)))
                                 (bodies (list-of expression?))
                                 (env environment?))
  )

; empty-env: () -> environment
; Crea un ambiente completamente vacío (base de la cadena de ambientes)
(define empty-env (lambda ()
                    (empty-env-record)
 ))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;extend-env-recursively: <list-of-symbols> <list-of<list-of-symbols>> <list-of-exprs> environment -> environment
(define extend-env-recursively
  (lambda (proc-names idss bodies env)
    (recursive-extended-env-record
     proc-names
     idss
     bodies
     env)))

; init-env: () -> environment
; Ambiente inicial del intérprete: predefine cinco variables (@a..@e) con valores de ejemplo
(define init-env (lambda ()
      (extend-env '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (empty-env))
))


; buscar-variable:
; Recorre la cadena de ambientes buscando el símbolo dado.
; - En empty-env-record: la variable no existe
; - En extended-env-record: busca la posición del símbolo en el marco actual;
;   si no está, continúa en el ambiente anterior
; - En recursive-extended-env-record: si encuentra el símbolo entre los procedimientos recursivos,
;   construye y retorna la cerradura usando el ambiente recursivo como contexto (permite recursión)
(define buscar-variable (lambda (sym env)
                    (cases environment env
                    (empty-env-record () (eopl:error buscar-variable "la variable no existe" sym))
                    (extended-env-record (syms vals next-env)
                                        (let ([pos (list-find-position syms sym)])
                                          (if (number? pos)
                                            (list-ref vals pos)
                                            (buscar-variable sym next-env))
                                        )
                   )
                  (recursive-extended-env-record (proc-names idss bodies old-env)
                                                 (let ([pos (list-find-position proc-names sym)])
                                                   (if (number? pos) (closure (list-ref idss pos)
                                                                              (list-ref bodies pos)
                                                                              env)
                                                       (buscar-variable sym old-env))))
                      )))



; TIPO DE DATO PROCEDIMIENTOS 
; Una cerradura (closure) captura: la lista de parámetros, el cuerpo de la función
; y el ambiente en el momento de su definición
(define-datatype procval procval?
(closure (ids (list-of symbol?)) (body expression?) (env environment?))
)

; Retorna #t si la expresión es un procedimiento (procedimiento-ex), #f en otro caso
; Útil para distinguir procedimientos de valores simples en declarar-recursivo
(define proc-expr? (lambda (expr)
                     (cases expression expr
                       (procedimiento-ex (ids body) #t)
                       (else #f))))

; Extrae la lista de parámetros de un procedimiento-ex
; Precondición: la expresión debe ser un procedimiento-ex (verificar con proc-expr? antes de llamar)
(define get-proc-ids
  (lambda (expr)
    (cases expression expr
      (procedimiento-ex (ids body)
                        ids)
      (else eopl:error 'get-proc-ids "no es un procedimiento ~s" expr))))

; get-proc-body: <expression> -> expression:
; Extrae el cuerpo de un procedimiento-ex
; Precondición: la expresión debe ser un procedimiento-ex (verificar con proc-expr? antes de llamar)
(define get-proc-body
  (lambda (expr)
    (cases expression expr
      (procedimiento-ex (ids body) body)
      (else eopl:error 'get-proc-body "no es un procedimiento ~s" expr))))

; split-rec-and-normal: <list-of-symbols> <list-of-expression> ->
; '(<proc-names> <list-of<list-of-symbols>> <list-of-expressions> <list-of-symbols> <list-of-expressions>)
; la idea se basa en poder separar los procedimientos recursivos de declaraciones normales esto para permitir al
; lenguaje en el declarar-recursivo hacer tanto como @a = 1 y @b = procedimiento.. sin problema
(define split-rec-and-normal (lambda (ids exprs)
                               (if (null? ids) (list '() '() '() '() '())
                                   (let ([rest (split-rec-and-normal (cdr ids) (cdr exprs))])
                                     (if (proc-expr? (car exprs))
                                        (list
                                         (cons (car ids) (list-ref rest 0))
                                         (cons (get-proc-ids (car exprs)) (list-ref rest 1))
                                         (cons (get-proc-body (car exprs)) (list-ref rest 2))
                                         (list-ref rest 3)
                                         (list-ref rest 4))
                                        (list
                                         (list-ref rest 0)
                                         (list-ref rest 1)
                                         (list-ref rest 2)
                                         (cons (car ids) (list-ref rest 3))
                                         (cons (car exprs) (list-ref rest 4)))
                                         )))
              ))


;EVALUADORES
; Desempaqueta el programa y evalúa su expresión
; en el ambiente inicial predefinido
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (evaluate-expr body (init-env))))))


; evalúa una expresión en el ambiente dado.
; Cada caso corresponde a una forma sintáctica definida en la gramática:
(define evaluate-expr (lambda (expr env)
                 (cases expression expr
                   (number-exp (n) n)
                   (text-lit (t) t)
                   (var-exp (id) (buscar-variable id env))
                   (binary-exp (left op right) (evaluate-prim-binary
                                                (evaluate-expr left env)
                                                op
                                               (evaluate-expr right env)))
                   (unary-exp (op left) (evaluate-prim-unary op (evaluate-expr left env)))
                   (condicional-exp (condition true-exp false-exp)
                                  (if (valor-verdad?  (evaluate-expr condition env)) ; haciendo if al if
                                      (evaluate-expr true-exp env)
                                      (evaluate-expr false-exp env)))
                   (variableLocal-exp (ids exprs body) 
                                       (evaluate-expr body (extend-env ids (evaluate-list-expr exprs env) env))
                                                       )
                   (procedimiento-ex (ids body) (closure ids body env))
                   (app-exp (expr exprs)
                            (let ([func (evaluate-expr expr env)])
                              
                            (if (procval? func)
                                (cases procval func
                                  (closure (ids body curr-env)
                                  (evaluate-expr body (extend-env ids (evaluate-list-expr exprs env) curr-env))))
                                (eopl:error 'app-exp "~s no es un procedimiento" func)) ; que alguien mejore el error xd
                            ))
                   (variableLocalRec-exp (ids exprs body)
                                         (let* (
                                                [result (split-rec-and-normal ids exprs)]
                                                [proc-names (list-ref result 0)]
                                                [idss (list-ref result 1)]
                                                [bodies (list-ref result 2)]
                                                [identifiers (list-ref result 3)]
                                                [expressions (list-ref result 4)]
                                                [env1 (extend-env identifiers (evaluate-list-expr expressions env) env)]
                                                [env2 (extend-env-recursively proc-names idss bodies env1)]
                                                )
                                           (evaluate-expr body env2)
                                           )))
                      ))


; Aplica una operación unaria sobre su único operando ya evaluado.
; longitud espera string; add1/sub1/neg esperan número
(define evaluate-prim-unary (lambda (op left)
                              (cases prim-unary op
                                (prim-unary-length () (string-length left))
                                (prim-unary-add1 () (+ left 1))
                                (prim-unary-sub1 () (- left 1))
                                (prim-unary-neg () (booleano-a-numero (zero? left)))
                                )))

; Aplica una operación binaria sobre sus dos operandos ya evaluados.
; Los operadores de comparación retornan 1 (verdadero) o 0 (falso).
; concat espera dos strings; los demás operadores esperan números.
(define evaluate-prim-binary (lambda (left op right)
                              (cases prim-binary op
                                (prim-binary-add () (+ left right))
                                (prim-binary-sub () (- left right))
                                (prim-binary-div () (/ left right))
                                (prim-binary-mul () (* left right))
                                (prim-binary-equal () (booleano-a-numero (equal? left right)))
                                (prim-binary-less () (booleano-a-numero (< left right)))
                                (prim-binary-less-equal () (booleano-a-numero (<= left right)))
                                (prim-binary-greater () (booleano-a-numero (> left right)))
                                (prim-binary-greater-equal () (booleano-a-numero (>= left right)))
                                (prim-binary-different () (booleano-a-numero (not (equal? left right))))
                                (prim-binary-concat () (string-append left right))
                                                   )
                              )
                             )


; Evalúa una lista de expresiones en el mismo ambiente, retornando la lista de resultados.
; Usado para evaluar argumentos de app-exp y las expresiones de variableLocal-exp.
(define evaluate-list-expr (lambda (exprs env)
                             (map (lambda(x) (evaluate-expr x env)) exprs)))




;FUNCIONES AUXILIARES
; Busca v en lst y retorna su índice (base 0) si lo encuentra, #f si no está.
; El tercer argumento pos es el acumulador del índice actual (por defecto 0).
(define list-find-position (lambda (lst v [pos 0])
   (cond
     [(null? lst) #f]
     [(equal? v (car lst)) pos]
     [else (list-find-position (cdr lst) v (+ pos 1))]
     )
))

; Convierte un booleano de Scheme al sistema aritmético de verdad del lenguaje:
; #t -> 1, #f -> 0
(define booleano-a-numero (lambda (boolean)
                        (if boolean 1 0)
                        ))


; Interpreta un número como valor de verdad del lenguaje:
; 0 es falso, cualquier otro valor (incluidos negativos) es verdadero
(define valor-verdad? (lambda (number)
                               (if (zero? number) #f #t)))




(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(define scheme-value? (lambda (v) #t))

; INTERPRETADOR
; Lee una expresión del usuario, la evalúa con eval-program y muestra el resultado.
; Usa el prompt "--> " para indicar que espera entrada.
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


(interpretador)
;SOLUCION DE LOS EJERCICIOS

;
;9a)Sumar Digitos
;declarar-recursivo(
;@helper=procedimiento(@n,@m,@i) {
;        Si ((@m * @i) <= @n) 
;        {Si ((@n ~ (@m * @i)) < @m) {(@n ~ (@m * @i))} sino {
;            evaluar @helper (@n,@m, (@i + 1)) finEval
;          }} sino {@n}
;      };
;@modulo=procedimiento(@n, @m) {
;    evaluar @helper(@n,@m, 0) finEval
;  };
;@div10=procedimiento(@n) {
;    ((@n ~ evaluar @modulo(@n,10) finEval) / 10)
;  };
;@sumarDigitos=procedimiento(@n) {
;    Si (@n == 0) {0} sino {(evaluar @modulo(@n,10) finEval +
;    evaluar @sumarDigitos (evaluar @div10 (@n) finEval) finEval)}
;  };
;) 
;  {
;    evaluar @sumarDigitos (111) finEval
;  }
;
; 9b) Factorial recursivo
; evaluar @factorial(5) finEval  -> 120

; declarar-recursivo (
;   @factorial = procedimiento (@n) {
;     Si @n {
;       (@n * evaluar @factorial(sub1(@n)) finEval)
;     } sino {
;       1
;     }
;   };
; ) {
;   evaluar @factorial(5) finEval
; }
;
; 9c) Potencia recursivo
; declarar-recursivo (
;   @potencia=procedimiento(@n, @m) {
;     Si (@m == 0) {1} sino {(@n * evaluar @potencia(@n,sub1(@m)) finEval)}
;    };
; ) {
;     evaluar @potencia(2,8) finEval
;   }
;
;9d) Suma rango
;     declarar-recursivo (
;   @sumaRango=procedimiento(@a, @b) {
;       Si (@a == @b) {@b} sino {(@a + evaluar @sumaRango(add1(@a), @b) finEval)}
;     };
; ) {
;    evaluar @sumaRango(2,5) finEval
;   }
;
;
; 9e)
;     declarar (
;  @integrantes = "Samuel_y_Sebastian_y_Camilo";
; ) {
;    declarar (
;      @saludar = procedimiento(@string) {
;        procedimiento() {
;          ("Hola:" concat @string)
;          }
;        };
;    ) {
;        declarar (
;          @decorate=evaluar @saludar(@integrantes) finEval;
;        ) {
;            evaluar @decorate() finEval
;          }
;      }
;  }
;
; 9f)
;declarar (
;  @integrantes = "Samuel_y_Sebastian_y_Camilo";
;) {
;    declarar (
;      @saludar = procedimiento(@string) {
;        procedimiento(@str) {
;          (("Hola:" concat @string) concat @str)
;          }
;        };
;    ) {
;        declarar (
;          @decorate=evaluar @saludar(@integrantes) finEval;
;        ) {
;            evaluar @decorate("Y_FLP") finEval
;          }
;      }
;  }
