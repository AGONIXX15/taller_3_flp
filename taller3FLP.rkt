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
  (text (letter (arbno (or letter digit "_"))) string)
  ))

; ESPECIFICACIÓN GRAMATICAL
; Define la estructura sintáctica del lenguaje usando los tokens anteriores.
; Cada regla tiene la forma: (no-terminal (producción) nombre-constructor)
(define grammar-simple-interpreter
  '(
    ; Un programa es exactamente una expresión
    (program (expression) a-program)

    ; EXPRESIONES BÁSICAS

    ; Literal numérico: cualquier número definido en el scanner
    (expression (number) number-exp)

    ; Variable: un identificador que empieza con @
    (expression (identifier) var-exp)

    ; Literal de texto: cualquier texto entre comillas dobles
    ; Las comillas se escapan con \" en la gramática
    (expression ("\"" text "\"") text-lit)

    ; EXPRESIONES CON PRIMITIVAS

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

    ; --- PROCEDIMIENTOS (funciones anónimas) ---
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


;COPIADO DEL PROFESOR
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;VERIFIQUEN CON ESTO QUE TODO ESTE CORRECTO PLS
(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

(define scheme-value? (lambda (v) #t))

; definimos el tipo de dato de environment
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursive-extended-env-record (proc-names (list-of symbol?))
                                 (ids (list-of (list-of symbol?)))
                                 (bodies (list-of expression?))
                                 (env environment?))
  )

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

; el ambiente inicial del interprete
(define init-env (lambda ()
      (extend-env '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (empty-env))
))

; busca un elemento en la lista
(define list-find-position (lambda (lst v [pos 0])
   (cond
     [(null? lst) #f]
     [(equal? v (car lst)) pos]
     [else (list-find-position (cdr lst) v (+ pos 1))]
     )
))


; buscar un simbolo en un ambiente
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

(define-datatype procval procval?
(closure (ids (list-of symbol?)) (body expression?) (env environment?))
)

(define valor-verdad? (lambda (boolean)
                        (if boolean 1 0)
                        ))

(define evaluate-prim-binary (lambda (left op right)
                              (cases prim-binary op
                                (prim-binary-add () (+ left right))
                                (prim-binary-sub () (- left right))
                                (prim-binary-div () (/ left right))
                                (prim-binary-mul () (* left right))
                                (prim-binary-equal () (valor-verdad? (equal? left right)))
                                (prim-binary-less () (valor-verdad? (< left right)))
                                (prim-binary-less-equal () (valor-verdad? (<= left right)))
                                (prim-binary-greater () (valor-verdad? (> left right)))
                                (prim-binary-greater-equal () (valor-verdad? (>= left right)))
                                (prim-binary-different () (valor-verdad? (not (equal? left right))))
                                (prim-binary-concat () (string-append left right))
                                                   )
                              )
                             )

(define evaluate-prim-unary (lambda (op left)
                              (cases prim-unary op
                                (prim-unary-length () (string-length left))
                                (prim-unary-add1 () (+ left 1))
                                (prim-unary-sub1 () (- left 1))
                                (prim-unary-neg () (not left))
                                )))

(define evaluate-list-expr (lambda (exprs env)
                             (map (lambda(x) (evaluate-expr x env)) exprs)))

(define proc-expr? (lambda (expr)
                     (cases expression expr
                       (procedimiento-ex (ids body) #t)
                       (else #f))))
(define get-proc-ids
  (lambda (expr)
    (cases expression expr
      (procedimiento-ex (ids body)
                        ids)
      (else eopl:error 'get-proc-ids "no es un procedimiento ~s" expr))))
; get-proc-body: <expression> -> expression: para no tener incovenientes deberia recibir un procedimiento-ex
; usar validacion para esto proc-expr?
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


(define evaluate-conditional (lambda (number)
                               (if (zero? number) #f #t)))

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
                                  (if (evaluate-conditional (evaluate-expr condition env)) ; haciendo if al if
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

