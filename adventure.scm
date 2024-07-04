(define rooms
  (list 
   (list 'living-room "You are in the living room. There is a door to the north and a kitchen to the west.")
   (list 'kitchen "You are in the kitchen. There is a living room to the east.")
   (list 'hallway "You are in the hallway. There is a living room to the south and a bedroom to the north.")
   (list 'bedroom "You are in the bedroom. There is a hallway to the south.")))

(define room-connections
  (list
   (list 'living-room 'north 'hallway)
   (list 'living-room 'west 'kitchen)
   (list 'kitchen 'east 'living-room)
   (list 'hallway 'south 'living-room)
   (list 'hallway 'north 'bedroom)
   (list 'bedroom 'south 'hallway)))

(define (get-command)
  (begin
    (display "> ")
    (flush-output)
    (string->symbol (string-downcase (read-line)))))

(define (find-room name)
  (car (filter (lambda (room) (eq? (car room) name)) rooms)))

(define (describe-room room)
  (cadr room))

(define (find-connection current-room direction)
  (caddr (find (lambda (connection) (and (eq? (car connection) current-room)
                                         (eq? (cadr connection) direction)))
               room-connections)))

(define current-room 'living-room)

(define (move direction)
  (let ((new-room (find-connection current-room direction)))
    (if new-room
        (set! current-room new-room)
        (begin
          (display "You can't go that way.")
          (newline)))))

(define (describe-current-room)
  (display (describe-room (find-room current-room)))
  (newline))

(define (game-loop)
  (describe-current-room)
  (let ((command (get-command)))
    (cond ((eq? command 'north) (move 'north))
          ((eq? command 'south) (move 'south))
          ((eq? command 'east) (move 'east))
          ((eq? command 'west) (move 'west))
          ((eq? command 'quit) (display "Goodbye!")
                                (newline)
                                (exit))
          (else (display "Invalid command.")
                (newline)))
    (game-loop)))

;; Start the game
(game-loop)
