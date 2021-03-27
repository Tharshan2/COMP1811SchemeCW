(require racket/trace)

;; hang-man for REPL Scheme


;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-wrds-from filename)
  (let* ((port (open-input-file filename))
         (res (read-wrd-lst port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-wrd-lst port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-wrd-lst port
                        (cons (symbol->string stuff) acc)))))

(define lst-of-wrds (read-wrds-from source-name)) ;; define a function called list of word from the glossary file

;; STATE OF THE GAME
;;This function selects a random word from the glossary
(define (wrd-to-guess)              ;;function without arguments
  (define (random-wrd lst rand i)   ;;inner function with a counter
    (if (= i rand)                   ;;if the random number equals to the counter
        (string->list(car lst))      ;;display the words from the text file as a list of chars
        (random-wrd (cdr lst) rand (+ i 1))))  ;;recursivly calls itself
  (random-wrd lst-of-wrds (random (length lst-of-wrds)) 0))

;;this turns makes a list of star symbols with the length of the word
(define partial-sol
  (string->list(make-string (length (wrd-to-guess)) #\*)))

(define hits 0) ;;define a function called hits as 0
(define plays 0) ;;define a function called plays as 0
(define failures 0) ;;define a function called failures as 0
(define total-failures 6) ;;define a function called total-failures as 6
(define total-hits (length (wrd-to-guess))) ;;define a function called total-hits to the length of the word that is set to guess
(define glossary (map string->list lst-of-wrds)) ;;define a function called glossary which maps the word from a string to list




;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string wrd-to-guess) ")")))))

          

;;
;;  PURELY FUNCTIONAL
;;

;; The function to count the number of occurrences of a letter in a word 
(define (occurrences wrd char)
  (if (null? wrd)              ;;if the list/word is empty display 0
      0
      (if (eq? (car wrd) char) ;;ensure that the char is identical to the fisrt atom of the list
          (+ 1 (occurrences (cdr wrd) char)) ;;if this is true add 1
          (occurrences (cdr wrd) char)))) ;;return char with the whole word


;; Shows the position of the letter in the word

(define (indices wrd char)
  (define indices-lmbd
    (lambda (wrd char i) ;;with lambda establish an inner function  with a counter
      (if (null? wrd)    ;;if the word is void return an empty list
          '()
          (if (equal?(car wrd) char)                        ;;verify that the fisrt atom of the list is the letter
              (cons i (indices-lmbd (cdr wrd) char (+ i 1))) ;;find the sum of the list and if true use recursion to check the rest of the list
              (indices-lmbd (cdr wrd) char (+ i 1))))))      ;;calls itself and adds 1 to the counter
  (indices-lmbd wrd char 0))                                 ;;invokes the indices-lmbd with starting counter of 0

;;Replace selected indices with the selected char
(define (replace-indices wrd idx new)
  (define (replace-idx wrd idx new i)  ;;establish an inner function with a counter
    (if (null? idx)                     ;;if the list is void restore the original list/word
        wrd
        (if (eq? (car idx) i)           ;;verify that the first atom of the index list is the same as the counter
            (cons new (replace-idx (cdr wrd) (cdr idx) new (+ i 1))) ;;create a new list with new char if true
            (cons (car wrd) (replace-idx (cdr wrd) idx new (+ i 1)))))) ;;if false repeat recursive iteration
  (replace-idx wrd idx new 0))


;;Number of hits function
(define (noOfHits hidden)
  (if (null? hidden)                     ;;display 0 once there are no matches found
      0
      (if (not (eq? (car hidden) #\*))   ;;verify that the first atom is a *
          (+ 1 (noOfHits (cdr hidden)))  ;;if the first atom is not a * increase the count by 1
          (noOfHits (cdr hidden)))))     ;;if true repeat recursion adn check the next atom


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SIDE EFFECTS
;; IO(String)
(define (restart)
  (begin
    (set! hits 0)      ;this will set all the hits back to 0
    (set! plays 0)     ;this will set all the plays back to 0
    (set! failures 0)  ;this will set all the failiures back to 0
    (set! total-failures 6)  ;this will set all the total-failiures back to 0
    (set! total-hits (length(wrd-to-guess)))  ;this will set all the total-hits to the length of the word that is set to guess
    (set! partial-sol (string->list(make-string (length (wrd-to-guess)) #\*)))
    ;;(display (list->string(word-to-guess)))
    ;; last statement
    (game-status)))


;; Char -> IO(String)
(define (guess char)
  (begin
    (set! plays (+ plays 1))                        ;;adds one to plays
    (set! hits (if (> (occurrences (wrd-to-guess) char) 0)     ;;adds the number of occurrences to the hits
                   (+ (occurrences (wrd-to-guess) char) hits)
                   hits))
    (set! failures (if (zero? (occurrences (wrd-to-guess) char)) ;;adds 1 to failures if occurrences are 0
                       (+ failures 1)
                       failures))
    (set! partial-sol (if (null? (indices (wrd-to-guess) char))  ;;updates the stars in the guess
                           partial-sol
                          (replace-indices partial-sol (indices (wrd-to-guess) char) char)))
    (game-status)))  ;;shows game progress


;; IO(String)
(define (solve wrd) (void))


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
(define (wrds-containing all-wrds char ) null)


;; p: all-words as list of list of char
;;  : chars as a list of char
(define (wrds-containing-ext all-wrds chars) null)

;; IO([String])
;; this is very hard.
(define (sieve chars) (void))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; TESTS;;;;;;;;;;;;;;


;;(trace occurrences)
;;(occurrences '(#\a #\a #\a #\*) #\b)