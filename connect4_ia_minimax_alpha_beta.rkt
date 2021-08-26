#lang racket

;; Constantes para el funcionamiento general del juego 
(define ROW_COUNT 6)
(define COLUMN_COUNT 7)

(define PLAYER 0)
(define IA 1)

(define EMPTY 0)
(define PLAYER_PIECE 1)
(define AI_PIECE 2)

(define WINDOW_LENGTH 4)

;; Se crea una varible para el tablero con todos los valores en 0
(define board (list (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0)))

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

#| 
  Funcion: Dada una lista y un elemento retorna la cantidad de veces que se encuentra repetido
           dicho elemento.
    Parametros: list, any
    Retorna: int
|#
(define (count list element)
  (cond [(null? list) 0]
        [(eq? (first list) element) (+ 1 (count (rest list) element))]
        [else (+ 0 (count (rest list) element))]))

#| 
  Funcion: Dada una lista verifica y le pone un valor a dicha lista, la lista representa una agrupacion
           de 4 piezas consecutivas que pueden ser horizontal, vertical o diagonal.
    Parametros: list, int
    Retorna: int
|#
(define (evaluate-window window piece)
  (define score 0)
  (define opp_piece PLAYER_PIECE)

  (cond [(= piece PLAYER_PIECE) (set! opp_piece AI_PIECE)])

  (cond [(= (count window piece) 4) (set! score (+ score 100))]
        [(and (= (count window piece) 3) (= (count window EMPTY) 1)) (set! score (+ score 5))]
        [(and (= (count window piece) 2) (= (count window EMPTY) 2)) (set! score (+ score 2))])

  (cond [(and (= (count window opp_piece) 3) (= (count window EMPTY) 1)) (set! score (- score 4))])

  score)



(define (get-column board col)
  (get-column-aux board col 0))

(define (get-column-aux board col row)
  (cond [(>= col COLUMN_COUNT) empty]
        [(>= row ROW_COUNT) empty]
        [else (cons (get-element board row col) (get-column-aux board col (add1 row)))])
  )

(define (get-row board row)
  (cond [(< row 0) empty]
        [(= row 0) (first board)]
        [else (get-row (rest board) (- row 1))]))

(define (get-diagonal board row col)
  (cond [(>= col COLUMN_COUNT) empty]
        [(>= row ROW_COUNT) empty]
        [else (cons (get-element board row col) (get-diagonal board (add1 row) (add1 col)))]))

(define (mirror-board board)
  (cond [(empty? board) empty]
        [else (cons (reverse (first board)) (mirror-board (rest board)))]))

(define (slice list start length)
  (cond [(< start 0) empty]
        [(<= length 0) empty]
        [(empty? list) empty]
        [(= start 0) (cons (first list) (slice (rest list) start (- length 1)))]
        [else (slice (rest list) (- start 1) length)]))

;row y col deben empezar en cero
;ejemplo: (list (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 1 1 1 1 0) (list 0 0 1 1 1 2) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)) = 117
(define (horizontal-score board piece row col)
  (cond [(>= row ROW_COUNT) 0]
        [(>= col (- COLUMN_COUNT 3)) (horizontal-score board piece (add1 row) 0)]
        [else (+
               (evaluate-window (slice (get-row board row) col WINDOW_LENGTH) piece)
               (horizontal-score board piece row (add1 col)))]))

;row y col deben empezar en cero
;ejemplo: (list (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 1 1 1 1 0) (list 0 0 1 1 1 2) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)) = 18
(define (vertical-score board piece row col)
  (cond [(>= col COLUMN_COUNT) 0]
        [(>= row (- ROW_COUNT 3)) (vertical-score board piece 0 (add1 col))]
        [else (+
               (evaluate-window (slice (get-column board col) row WINDOW_LENGTH) piece)
               (vertical-score board piece (add1 row) col))]))

; no se necesita
(define (list-score list piece)
  (cond [(< (length list) 4) 0]
        [(+
          (evaluate-window (slice list 0 WINDOW_LENGTH) piece)
          (list-score (rest list) piece))]))

;ejemplo: (list (list 0 0 0 0 0 0) (list 0 0 0 0 0 0) (list 0 1 1 1 1 0) (list 0 0 1 1 1 2) (list 1 1 1 1 1 1) (list 0 0 0 0 0 2) (list 0 0 0 0 0 0)) = 26
;(list (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 1 1 1 1 0 0) (list 0 0 1 1 1 2 0) (list 1 1 1 1 1 1 0) (list 0 0 0 0 0 2 0))=29
(define (diagonal-score board piece row col)
  (cond [(>= row (- ROW_COUNT 3)) 0]
        [(>= col (- COLUMN_COUNT 3)) (diagonal-score board piece (add1 row) 0)]
        [else (+
               (evaluate-window (slice (get-diagonal board row col) 0 WINDOW_LENGTH) piece)
               (diagonal-score board piece row (add1 col)))]))

;ejemplo: (list (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 1 1 1 1 0 0) (list 0 0 1 1 1 2 0) (list 1 1 1 1 1 1 0) (list 0 0 0 0 0 2 0)) = 532
(define (score-position board piece)
  (+
   (+
    (+
     (+
      (* (count (get-column board (round (/ COLUMN_COUNT 2))) piece) 3)
      (horizontal-score board piece 0 0))
     (vertical-score board piece 0 0))
    (diagonal-score board piece 0 0))
   (diagonal-score (mirror-board board) piece 0 0)))