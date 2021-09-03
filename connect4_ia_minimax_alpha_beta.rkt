#lang racket/gui
(require racket/draw)

;; Constantes para el funcionamiento general del juego 
(define ROW_COUNT 6)
(define COLUMN_COUNT 7)

(define PLAYER 0)
(define IA 1)
(define TURN 0)

(define EMPTY 0)
(define PLAYER_PIECE 1)
(define AI_PIECE 2)

(define WINDOW_LENGTH 4)

(define MINUS_INF -100000000000000000)
(define INF 100000000000000000)

;; Se crea una varible para el tablero con todos los valores en 0
(define board (list (list 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0)
                    (list 0 0 0 0 0 0 0)))
;(list (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0) (list 0 0 0 0 0 0 0))
;(list (list 1 2 1 2 1 2 1) (list 2 1 2 1 2 1 2) (list 1 2 1 2 1 2 1) (list 1 2 1 2 1 2 1) (list 2 1 2 1 2 1 2) (list 1 2 1 2 1 2 1))
#|(define board (list (list 2 2 1 2 1 2 1)
                    (list 1 1 2 1 2 1 2)
                    (list 1 2 1 1 1 2 1)
                    (list 1 2 2 2 1 2 1)
                    (list 2 1 2 2 2 1 2)
                    (list 0 0 1 2 0 2 2)))|#

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
(define (drop-piece row col piece)
  (set-element-board row col piece))

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
    Parametros: matrix int int int
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
    Parametros: matrix int int int
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
    Parametros: matrix int int int
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
    Parametros: matrix int int int
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
    Parametros: matrix int int int
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
      (* (count (get-column board (floor (/ COLUMN_COUNT 2))) piece) 3)
      (horizontal-score board piece 0 0))
     (vertical-score board piece 0 0))
    (diagonal-score board piece 0 0))
   (diagonal-score (mirror-board board) piece 0 0)))


#| 
  Funcion: Dado un tablero devuelve todas las columnas donde se puede tirar una ficha.
    Parametros: matrix
    Retorna: list
|#
(define (get-valid-locations board)
   (get-valid-locations-aux board 0))

#| 
  Funcion: Auxiliar para ayudar a get-valid-locations con el conteo de 0 hasta COLUMN_COUNT.
    Parametros: matrix int
    Retorna: list
|#
(define (get-valid-locations-aux board col)
  (cond [(= col COLUMN_COUNT) '()]
        [(is-valid-location board col) (append (get-valid-locations-aux board (add1 col)) (list col))]
        [else (get-valid-locations-aux board (add1 col))]))


(define (random-choice list)
  (list-ref list (random (length list))))


(define (check-pos-max board depth alpha beta maximize max-value max-col valid-locations)
  (cond [(empty? valid-locations) (list max-col max-value)]
        [(>= alpha beta) (list max-col max-value)]
        [else
         (define col (first valid-locations))
         (define row (get-next-open-row board col))
         (define new-value (minimax
                            (set-element board row col maximize)
                            (- depth 1)
                            alpha beta row col (if (= maximize AI_PIECE) PLAYER_PIECE AI_PIECE)))
         (if (> (second new-value) max-value)
             (check-pos-max board depth (max alpha (second new-value) max-value) beta maximize (second new-value) col (rest valid-locations))
             (check-pos-max board depth (max alpha (second new-value) max-value) beta maximize max-value max-col (rest valid-locations)))]))


(define (check-pos-min board depth alpha beta maximize max-value max-col valid-locations)
  (cond [(empty? valid-locations) (list max-col max-value)]
        [(>= alpha beta) (list max-col max-value)]
        [else
         (define col (first valid-locations))
         (define row (get-next-open-row board col))
         (define new-value (minimax
                            (set-element board row col maximize)
                            (- depth 1)
                            alpha beta row col (if (= maximize AI_PIECE) PLAYER_PIECE AI_PIECE)))
         (if (< (second new-value) max-value)
             (check-pos-min board depth alpha (min beta (second new-value) max-value) maximize (second new-value) col (rest valid-locations))
             (check-pos-min board depth alpha (min beta (second new-value) max-value) maximize max-value max-col (rest valid-locations)))]))


(define (minimax board depth alpha beta row col maximize)
  (define valid-locations (get-valid-locations board))
  (cond [(= depth 0) (list "-" (score-position board AI_PIECE))]
        [(is-winning-move board AI_PIECE row col) '("-" 100000000000000)]
        [(is-winning-move board PLAYER_PIECE row col) '("-" -100000000000000)]
        [(= (length valid-locations) 0) '("-" 0)] ;game over, no more valid moves
        [(= maximize AI_PIECE) (check-pos-max board depth MINUS_INF INF maximize MINUS_INF (random-choice valid-locations) valid-locations)]
        [(= maximize PLAYER_PIECE) (check-pos-min board depth MINUS_INF INF maximize INF (random-choice valid-locations) valid-locations)]))


(define (ai-turn)
  (define best-move (minimax board 6 MINUS_INF INF 0 0 AI_PIECE))
  (define row (get-next-open-row board (first best-move)))
  (draw-face bm-dc row (first best-move))
  (drop-piece row (first best-move) AI_PIECE)
  (set! TURN PLAYER)
  (list row (first best-move)))


(define image (read-bitmap "background2.jpg"))

; Make a 800 x 650 frame
(define frame (new frame%
                   [label "Connect Four"]
                   [width 800]
                   [height 650]))


(define mode #f);if mode=1, then its one player game , if mode=2 then 2 player


; Make the drawing area with a paint callback

(define my-canvas%
  (class canvas%
    (define positions (list 0 0))
    (define/override (on-event event)
      (cond
        [(is-winning-move board PLAYER_PIECE (first positions) (second positions)) (send msg set-label "PLAYER wins")]
        [(is-winning-move board AI_PIECE (first positions) (second positions)) (send msg set-label "AI wins")]
        [(= (length (get-valid-locations board)) 0) (send msg set-label "It's a tie")]
        [else (if (= TURN PLAYER)
                  (set! positions (fun (send event button-down? 'left) (send event get-x)))
                  (set! positions (ai-turn)))])
    )   
    ; Call the superclass init, passing on all init args
    (super-new)))

(define canvas
  (new my-canvas%
       [parent frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

; ... pens, brushes, and draw-face are the same as above ...

(define (paint dc) 
                   
                   (send dc draw-bitmap face-bitmap 0 0))

; ... pens, brushes, and draw-face are the same as above ...

; Create a 800 x 650 bitmap
(define face-bitmap (make-object bitmap% 800 650))      ; Create a drawing context for the bitmap

(define bm-dc (make-object bitmap-dc% face-bitmap))     ; A bitmap's initial content is undefined; clear it before drawing
;(send bm-dc clear)

; Make some pens and brushes
(define blue-pen (make-object pen% "GREY" 4 'solid))
(define blue-pen1 (make-object pen% "GREY" 10 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))

(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "RED" 'solid))

; Define a procedure to draw a face
(define player 0);if player=0 its red's turn else yellow's
(send bm-dc draw-bitmap image 0 0)

(define (draw-face dc r c)

  (send canvas refresh)
    
  (send dc set-pen blue-pen)
  (if(= TURN PLAYER) (begin (send dc set-brush red-brush) (send msg set-label "player1's turn"))
     (begin (send dc set-brush yellow-brush) (send msg set-label "player2's turn")))
  (if r (begin (send dc draw-ellipse (+ 120 (* 80 c)) (- 480 (* 80 r)) 80 80))
      (void)
      )

  (send dc set-pen blue-pen1)  
  (send dc set-brush no-brush)
  (send dc draw-rounded-rectangle 120 80 560 480 10)
  (define (h-lines n1)
    (if(= n1 5) (send dc draw-line (+ 200 (* 80 n1)) 80 (+ 200 (* 80 n1)) 560)
       
       (begin (send dc draw-line (+ 200 (* 80 n1)) 80 (+ 200 (* 80 n1)) 560) (send dc draw-line 120 (+ 160 (* 80 n1)) 680 (+ 160 (* 80 n1))) (h-lines (add1 n1)))))
  (h-lines 0))


; Show the frame
(send frame show #t)

(define msg (new message% [parent frame]
                 [label "No events so far..."]))


(define (fun click? mouse-x)
  (if click? (begin (let* ([c (cond [(and (>= mouse-x 120) (< mouse-x 200)) 0]
                             [(and (>= mouse-x 200) (< mouse-x 280)) 1]
                             [(and (>= mouse-x 280) (< mouse-x 360)) 2]
                             [(and (>= mouse-x 360) (< mouse-x 440)) 3]
                             [(and (>= mouse-x 440) (< mouse-x 520)) 4]
                             [(and (>= mouse-x 520) (< mouse-x 600)) 5]
                             [(and (>= mouse-x 600) (< mouse-x 680)) 6])]
                    [r (get-next-open-row board c)]
                    )
               (if (void? r) (set! r #f) (begin (draw-face bm-dc r c) (user-turn r c)))))
      (begin (draw-face bm-dc #f #f) (list 0 0))))


(define (user-turn row col)
  (drop-piece row col PLAYER_PIECE)
  (set! TURN IA)
  (list row col))