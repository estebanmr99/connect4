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
  (cond [(eq? row ROW_COUNT)];ACA NUNCA VA A ENTRAR
        [(eq? (get-element board row col) 0) row]
        [(< row ROW_COUNT) (get-next-open-row-aux board col (add1 row))]))

#| 
  Funcion: Indica si el movimiento que se realiza del cual recibe la posicion
           (row,col) por parametro, es un movimiento ganador, es decir,
           el movimiento hace posible la conexion de cuatro fichas del mismo jugador
    Parametros: board, piece, row, col
    Retorna: boolean
|#
(define (is-winning-move board piece row col)
  (or (check-horizontal board piece row 0)
      (or (check-vertical board piece 0 col)
          (or (check-negative-diagonal board piece 3 0)
              (check-positive-diagonal board piece 0 0)))))

#| 
  Funcion: Verifica de manera horizontal, en una fila especifica (row),
           si existen cuatro fichas seguidas del mismo jugador. El parámetro
           col siempre debe empezar en cero cuando se llama a la funcion.
    Parametros: board, piece, row, col
    Retorna: boolean
|#
(define (check-horizontal board piece row col)
  (cond [(= (- COLUMN_COUNT 3) col) #f]
        [(and (= (get-element board row col) piece)
              (and (= (get-element board row (+ col 1)) piece)
                   (and (= (get-element board row (+ col 2)) piece)
                        (= (get-element board row (+ col 3)) piece)))) #t]
        [else (check-horizontal board piece row (add1 col))]))

#| 
  Funcion: Verifica de manera vertical, en una columna especifica (col),
           si existen cuatro fichas seguidas del mismo jugador. El parámetro
           row siempre debe empezar en cero cuando se llama a la funcion.
    Parametros: board, piece, row, col
    Retorna: boolean
|#
(define (check-vertical board piece row col)
  (cond [(= (- ROW_COUNT 3) row) #f]
        [(and (= (get-element board row col) piece)
              (and (= (get-element board (+ row 1) col) piece)
                   (and (= (get-element board (+ row 2) col) piece)
                        (= (get-element board (+ row 3) col) piece)))) #t]
        [else (check-vertical board piece (add1 row) col)]))

#| 
  Funcion: Verifica todas las diagonales de largo mayor a cuatro para ver
           si existen cuatro fichas seguidas del mismo jugador. El parámetro
           col siempre debe empezar en cero y row en tres cuando se llama a la funcion.
    Parametros: board, piece, row, col
    Retorna: boolean
|#
(define (check-negative-diagonal board piece row col)
  (cond [(= ROW_COUNT row) #f]
        [(= (- COLUMN_COUNT 3) col) #f]
        [(and (= (get-element board row col) piece)
              (and (= (get-element board (- row 1) (+ col 1)) piece)
                   (and (= (get-element board (- row 2) (+ col 2)) piece)
                        (= (get-element board (- row 3) (+ col 3)) piece)))) #t]
        [else (or
               (check-negative-diagonal board piece 3 (add1 col))
               (check-negative-diagonal board piece (add1 row) col))]))

#| 
  Funcion: Verifica todas las diagonales opuestas a la funcion check-negative-diagonal,
           las mismas de largo mayor a cuatro, esto para ver
           si existen cuatro fichas seguidas del mismo jugador. El parámetro
           col y row siempre debe empezar en cero cuando se llama a la funcion.
    Parametros: board, piece, row, col
    Retorna: boolean
|#
(define (check-positive-diagonal board piece row col)
  (cond [(= (- ROW_COUNT 3) row) #f]
        [(= (- COLUMN_COUNT 3) col) #f]
        [(and (= (get-element board row col) piece)
              (and (= (get-element board (+ row 1) (+ col 1)) piece)
                   (and (= (get-element board (+ row 2) (+ col 2)) piece)
                        (= (get-element board (+ row 3) (+ col 3)) piece)))) #t]
        [else (or
               (check-positive-diagonal board piece 0 (add1 col))
               (check-positive-diagonal board piece (add1 row) col))]))