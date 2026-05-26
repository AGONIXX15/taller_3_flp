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