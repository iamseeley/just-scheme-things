;; board
(define (create-board)
  (list (list 'empty 'empty 'empty)
        (list 'empty 'empty 'empty)
        (list 'empty 'empty 'empty)))

(define (display-board board)
  (for-each (lambda (row)
              (for-each (lambda (cell)
                          (display (if (eq? cell 'empty) "-" cell))
                          (display " "))
                        row)
              (newline))
            board))

;; making moves
(define (make-move board row col player)
  (let ((new-board (map list-copy board)))
    (let ((new-row (list-copy (list-ref new-board row))))
      (set-car! (list-tail new-row col) player)
      (set-car! (list-tail new-board row) new-row))
    new-board))

(define (list-copy lst)
  (if (null? lst)
      '()
      (cons (car lst) (list-copy (cdr lst)))))

(define (list-set! lst index value)
  (if (zero? index)
      (begin
        (set-car! lst value)
        lst)
      (begin
        (list-set! (cdr lst) (- index 1) value)
        lst)))

;; checking for win
(define (check-win board)
  (let ((rows (map (lambda (r) r) board))
        (cols (map (lambda (c) (map (lambda (r) (list-ref r c)) board)) '(0 1 2)))
        (diags (list (list (list-ref (list-ref board 0) 0)
                           (list-ref (list-ref board 1) 1)
                           (list-ref (list-ref board 2) 2))
                     (list (list-ref (list-ref board 0) 2)
                           (list-ref (list-ref board 1) 1)
                           (list-ref (list-ref board 2) 0)))))
    (or (check-lines rows)
        (check-lines cols)
        (check-lines diags))))

(define (check-lines lines)
  (define (check-line line)
    (and (not (eq? (car line) 'empty))
         (equal? (car line) (cadr line))
         (equal? (cadr line) (caddr line))))
  (cond ((null? lines) #f)
        ((check-line (car lines)) (car (car lines)))
        (else (check-lines (cdr lines)))))

;; game loop
(define (game-loop board player)
  (display-board board)
  (if (check-win board)
      (begin
        (display "Player ")
        (display player)
        (display " wins!")
        (newline))
      (let ((row (read))
            (col (read)))
        (game-loop (make-move board row col player) (if (eq? player 'X) 'O 'X)))))

(game-loop (create-board) 'X)

