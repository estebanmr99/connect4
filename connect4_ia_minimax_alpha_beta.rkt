#lang racket

;; Constantes para el funcionamiento general del juego 
(define ROW_COUNT 7)
(define COLUMN_COUNT 6)

(define PLAYER 0)
(define IA 1)

(define EMPTY 0)
(define PLAYER_PIECE 1)
(define AI_PIECE 2)

(define WINDOW_LENGTH 4)

;; Se crea una varible para el tablero con todos los valores en 0
(define board (list (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)))

#| 
  Funcion: Retorna el valor que se encuentra en una fila y columna en una matriz.
    Parametros: matrix int int
    Retorna: int
|#
(define (get-element matrix row col)
  (list-ref (list-ref matrix row) col))

#| 
  Funcion: Retorna una copia de la matriz con el cambio del valor en una fila y columna.
    Parametros: matrix int int int
    Retorna: matrix
    Nota: No modifica la matriz original sino que retorna una nueva, si se quiere sobreescribir la original usar "set!" como la funcion comentada abajo
|#
(define (set-element matrix row col ele)
  (list-set matrix row (list-set (list-ref matrix row) col ele)))

#| 
  Funcion: Cambia un valor en una fila y columna en la matriz "board".
    Parametros: int int int
    Retorna: N/A
|#
(define (set-element-board row col ele)
  (set! board (list-set board row (list-set (list-ref board row) col ele))))

#| 
  Funcion: Imprime el tablero.
    Parametros: N/A
    Retorna: N/A
|#
(define (print-board) (print board))

#| 
  Funcion: Pone una pieza en una fila columna en una matriz dada.
    Parametros: matrix int int int
    Retorna: matrix
|#
(define (drop-piece board row col piece)
  (set-element-board board row col piece))

#| 
  Funcion: Verifica si una columna es valida para jugar.
    Parametros: matrix int
    Retorna: bool
|#
(define (is-valid-location board col)
  (eq? (get-element board (- ROW_COUNT 1) col) 0))

#| 
  Funcion: Retorna la fila valida donde poner una ficha dada una columna.
    Parametros: matrix int
    Retorna: int
|#
(define (get-next-open-row board col)
  (get-next-open-row-aux board col 0))

(define (get-next-open-row-aux board col row)
  (cond [(eq? row ROW_COUNT)]
        [(eq? (get-element board row col) 0) row]
        [(< row ROW_COUNT) (get-next-open-row-aux board col (add1 row))]))