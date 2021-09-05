#lang racket/gui
(require racket/draw)

;; Constantes para el funcionamiento general del juego 
(define ROW_COUNT 6)
(define COLUMN_COUNT 7)

(define PLAYER 0)
(define IA 1)
(define TURN 0)
(define DEPTH 6)

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


#|
  Funcion: Dado un tablero y un numero de columna del mismo, agrupa los valores de esa columna
           en una lista y la retona.
    Parametros: matrix, int
    Retorna: list
|#
(define (get-column board col)
  (get-column-aux board col 0))


#|
  Funcion: Funcion auxiliar para obtener una columna especifica en un tablero
    Parametros: matrix, int, int
    Retorna: list
|#
(define (get-column-aux board col row)
  (cond [(>= col COLUMN_COUNT) empty]
        [(>= row ROW_COUNT) empty]
        [else (cons (get-element board row col) (get-column-aux board col (add1 row)))])
  )

#|
  Funcion: Dado un tablero y un numero de fila del mismo, agrupa los valores de esa fila
           en una lista y la retona.
    Parametros: matrix, int
    Retorna: list
|#
(define (get-row board row)
  (cond [(< row 0) empty]
        [(= row 0) (first board)]
        [else (get-row (rest board) (- row 1))]))

#|
  Funcion: Dado un tablero y un numero de fila y columna del mismo, agrupa los valores de
           la diagonal a la que corresponde la posicion inicial recibida en una lista y la retona.
           Solo retorna diagonales cuyas posiciones siempre aumenten conforme se forma la misma.
    Parametros: matrix, int, int
    Retorna: list
|#
(define (get-diagonal board row col)
  (cond [(>= col COLUMN_COUNT) empty]
        [(>= row ROW_COUNT) empty]
        [else (cons (get-element board row col) (get-diagonal board (add1 row) (add1 col)))]))

#|
  Funcion: Dado un tablero, se encarga de formar el espejo del mismo y retornarlo.
    Parametros: matrix
    Retorna: matrix
|#
(define (mirror-board board)
  (cond [(empty? board) empty]
        [else (cons (reverse (first board)) (mirror-board (rest board)))]))

#|
  Funcion: Dada una lista, un inicio y un largo, obtiene un segmento de la lista
           desde el indice de inicio que se indica y a partir de este la cantidad de
           elementros que indica el largo
    Parametros: list, int, int
    Retorna: list
|#
(define (slice list start length)
  (cond [(< start 0) empty]
        [(<= length 0) empty]
        [(empty? list) empty]
        [(= start 0) (cons (first list) (slice (rest list) start (- length 1)))]
        [else (slice (rest list) (- start 1) length)]))

#|
  Funcion: Obtiene el puntaje de todas las filas del tablero que recibe, usando la funcion Eval del sistema.
           Este puntaje se calcula con una pieza en especifico. Debe recibir la fila y la columna como ceros. 
    Parametros: matrix, int, int, int
    Retorna: int
|#
(define (horizontal-score board piece row col)
  (cond [(>= row ROW_COUNT) 0]
        [(>= col (- COLUMN_COUNT 3)) (horizontal-score board piece (add1 row) 0)]
        [else (+
               (evaluate-window (slice (get-row board row) col WINDOW_LENGTH) piece)
               (horizontal-score board piece row (add1 col)))]))

#|
  Funcion: Obtiene el puntaje de todas las columnas del tablero que recibe, usando la funcion Eval del sistema.
           Este puntaje se calcula con una pieza en especifico. Debe recibir la fila y la columna como ceros. 
    Parametros: matrix, int, int, int
    Retorna: int
|#
(define (vertical-score board piece row col)
  (cond [(>= col COLUMN_COUNT) 0]
        [(>= row (- ROW_COUNT 3)) (vertical-score board piece 0 (add1 col))]
        [else (+
               (evaluate-window (slice (get-column board col) row WINDOW_LENGTH) piece)
               (vertical-score board piece (add1 row) col))]))


#|
  Funcion: Obtiene el puntaje de todas las diagonales del tablero que recibe, usando la funcion Eval del sistema.
           Solo obtiene el puntaje de diagonales cuyas posiciones siempre aumenten conforme se forma la misma.
           Este puntaje se calcula con una pieza en especifico. Debe recibir la fila y la columna como ceros. 
    Parametros: matrix, int, int, int
    Retorna: int
|#
(define (diagonal-score board piece row col)
  (cond [(>= row (- ROW_COUNT 3)) 0]
        [(>= col (- COLUMN_COUNT 3)) (diagonal-score board piece (add1 row) 0)]
        [else (+
               (evaluate-window (slice (get-diagonal board row col) 0 WINDOW_LENGTH) piece)
               (diagonal-score board piece row (add1 col)))]))

#|
  Funcion: Obtiene el puntaje completo del tablero que recibe (filas, columnas y diagonales), usando la funcion Eval del sistema.
           Para obtener los puntajes de ambas diagonales unas de ellas se calculan convirtiendo
           el tablero en un espejo del mismo.
           A la columna del centro se le multiplica el puntaje porque tiene mejor impacto en promedio en las jugadas que otras posiciones.
           Este puntaje se calcula con una pieza en especifico.
    Parametros: matrix, int
    Retorna: int
|#
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
   (get-valid-locations-aux board (list 3 2 4 1 5 0 6)))

#| 
  Funcion: Auxiliar para ayudar a get-valid-locations con el conteo de 0 hasta COLUMN_COUNT.
    Parametros: matrix int
    Retorna: list
|#
(define (get-valid-locations-aux board col)
  (cond [(empty? col) '()]
        [(is-valid-location board (first col)) (append (get-valid-locations-aux board (rest col)) (list (first col)))]
        [else (get-valid-locations-aux board (rest col))]))

#|
  Funcion: Obtiene un elemento aleatorio de la lista que recibe.
    Parametros: list
    Retorna: any
|#
(define (random-choice list)
  (list-ref list (random (length list))))

#|
  Funcion: Obtiene un numero de columna del tablero que representa el mejor movimiento para
           el jugador que se encarga de maximizar su puntaje. Esto lo hace utilizando la funcion de
           minimax con poda alfa-beta. 
    Parametros: matrix, int, int, int, int, int, int, list
    Retorna: int
|#
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

#|
  Funcion: Obtiene un numero de columna del tablero que representa el mejor movimiento para
           el jugador que se encarga de minimizar su puntaje. Esto lo hace utilizando la funcion de
           minimax con poda alfa-beta. 
    Parametros: matrix, int, int, int, int, int, int, list
    Retorna: int
|#
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

#|
  Funcion: Funcion principal de minimax que se encarga de obtener la columna de la mejor jugada
            para la IA segun el puntaje maximo que obtenga. Forma un arbol de minimax con la 
            profundidad que recibe por parametro, dependiendo de la profundidad y del avance del juego
            puede sacar solo un puntaje que puede no ser 100% acertado, así como puede obtener el movimiento
            que podría llevar al gane.
            Maximiza el puntaje de la IA y minimiza el del oponente, así se simula que los dos estén jugando
            su mejor jugada para poder derrotar al otro, con lo que se obtiene el mejor movimiento para la IA.
            Usa poda alpha-beta.
            Retorna una lista con el numero de columna y el puntaje correspondiente.
    Parametros: matrix, int, int, int, int, int, int
    Retorna: list
|#
(define (minimax board depth alpha beta row col maximize)
  (define valid-locations (get-valid-locations board))
  (cond [(= depth 0) (list "-" (score-position board AI_PIECE))]
        [(is-winning-move board AI_PIECE row col) '("-" 100000000000000)]
        [(is-winning-move board PLAYER_PIECE row col) '("-" -100000000000000)]
        [(= (length valid-locations) 0) '("-" 0)] ;game over, no more valid moves
        [(= maximize AI_PIECE) (check-pos-max board depth MINUS_INF INF maximize MINUS_INF (random-choice valid-locations) valid-locations)]
        [(= maximize PLAYER_PIECE) (check-pos-min board depth MINUS_INF INF maximize INF (random-choice valid-locations) valid-locations)]))

#|
  Funcion: Obtiene el movimiento (fila columna) que debe realizar la IA en su primer turno para maximizar su puntaje.
    Parametros: N/A
    Retorna: list
|#
(define first-turn #t)
(define (first-turn-move)
  (cond
    [(> (count (get-row board 0) PLAYER_PIECE) 0)
     (cond [(= (get-element board 0 1) PLAYER_PIECE) (list 0 2)]
           [(= (get-element board 0 5) PLAYER_PIECE) (list 0 4)]
           [else (list (get-next-open-row board 3) 3)])]
    [else (list 0 3)]))


#|
  Funcion: Obtiene el movimiento (fila columna) que debe realizar la IA para maximizar su puntaje.
           En esta funcion se llama a minimax para que realice el calculo, con excepcion del primer
           turno que siempre tiene una posicion fija que se sabe que es la mejor.
           En esta funcion también se realiza el cambio de turno.
    Parametros: N/A
    Retorna: list
|#
(define (ai-turn)
  (cond
    [(equal? first-turn #t) (set! first-turn #f)
                            (define positions (first-turn-move))
                            (draw-face bm-dc (first positions) (second positions))
                            (drop-piece (first positions) (second positions) AI_PIECE)
                            (set! TURN PLAYER)
                            (send msg set-label "User turn")
                            positions]
    [else
     (define best-move (minimax board DEPTH MINUS_INF INF 0 0 AI_PIECE))
     (define row (get-next-open-row board (first best-move)))
     (draw-face bm-dc row (first best-move))
     (drop-piece row (first best-move) AI_PIECE)
     (set! TURN PLAYER)
     (send msg set-label "User turn")
     (list row (first best-move))]))

#|
  Funcion: Coloca en el tablero de juego la ficha del usuario en la posicion que recibe por parametro.
            Cambia de turno.
    Parametros: int, int
    Retorna: list
|#
(define (user-turn row col)
  (drop-piece row col PLAYER_PIECE)
  (set! TURN IA)
  (send msg set-label "IA turn")
  (list row col))

;------------------ Interfaz Gráfica de Usuario ----------------------------------------
;---------------------------------------------------------------------------------------

;Imagen que se establece como el fondo del tablero del juego.
(define image (read-bitmap "background2.jpg"))

;Cuadro de 800 x 650 para montar la interfaz del juego
(define frame (new frame%
                   [label "Connect Four"]
                   [width 800]
                   [height 650]))


#|
  Funcion: Genera el area de dibujo (canvas) para pintar el tablero.
            Es la funcion encargada de inicar el sistema, por lo que se podría considerar 
            la funcion principal (main). Determina cuando gana un jugador, cuando hay un empate y termina 
            el juego o llama a las funciones correspondientes segun de quien sea el turno para que juegue
            ya sea la IA o el usuario.
    Parametros: N/A
    Retorna: N/A
|#
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

#|
Se rea un canvas del tipo my-canvas declarado previamente. Asigna el cuadro que ha sido declarado
anteriormente, como padre para que el canvas se muestre en dicho cuadro.
|#
(define canvas
  (new my-canvas%
       [parent frame]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

#|
Se define el contexto de dibujo (drawing context) para el tablero.
|#
(define (paint dc)
  (send dc draw-bitmap face-bitmap 0 0))


#| 
Se crea un bitmap de 800 x 650 para poder dibujar el tablero guiandose con los pixeles
|#
(define face-bitmap (make-object bitmap% 800 650))

;Se crea un contexto de dibujo para un bitmap 
(define bm-dc (make-object bitmap-dc% face-bitmap))    


;Lapiceros y brochas utilizadas para dibujar el ambiente grafico
(define blue-pen (make-object pen% "GREY" 4 'solid))
(define blue-pen1 (make-object pen% "GREY" 10 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-brush (make-object brush% "RED" 'solid))

; Llamada al procedimiento para dibujar el bitmap y montar la imagen de fondo
(send bm-dc draw-bitmap image 0 0)

#|
  Funcion: Se encarga de dibujar las fichas y las lineas que delimitan el tablero de juego.
            Segun el turno del jugador actual dibuja una ficha amarilla (IA) o roja (usuario), 
            esto en la posicion de la fila y columna que se indica por parametro. 
    Parametros: drawing context, int, int
    Retorna: N/A
|#
(define (draw-face dc r c)

  (send canvas refresh)
    
  (send dc set-pen blue-pen)
  (if (= TURN PLAYER) (begin (send dc set-brush red-brush))
     (begin (send dc set-brush yellow-brush)))
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


; Instruccion para mostrar el cuadro o ventana de juego
(send frame show #t)

; Mensaje para mostrar en la ventana de juego
(define msg (new message% [parent frame]
                 [label "Loading board..."]))

#|
  Funcion: Se encarga de captar los eventos de click en la ventana. Segun sea
            la posicion en la ventana donde se hizo click, obtiene las coordenadas y las 
            traduce en una posicion de columna. A partir de esta columna obtiene una fila
            válida para poder colocar la ficha y las retorna, si no hay una posicion valida para la columna retorna
            valores en cero y el programa se mantiene esperando a que se haga una jugada valida.
            Si la posicion es valida llama a la funcion encargada de dibujar la ficha y a la encargada
            de llevar a cabo la jugada del usuario en el tablero almacenado y cambiar de turno.
    Parametros: mouse-event%, mouse-event% 
    Retorna: list
|#
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

