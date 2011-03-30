(require (lib "trace.ss"))
(require mzlib/pregexp)
(require graphics)

;;citire din fisier
(define in (open-input-file "prog.turtle"))
;;citire date pana la str
(define (getData str)
  (let iter ((l (list (read-line in 'return-linefeed))))   
    (if (string=? (car l) str)
        l
        (iter (cons (read-line in 'return-linefeed) l)))))
      
(define var (getData "endvar")) ;; variabile
(define instr (getData "done")) ;; instructiuni
(define d 7) ;; lungimea liniei

(define (variabile l val)
  (cond ((string=? (car l) "variables") val)
        ((string=? (car l) "endvar")
         (variabile (cdr l) val))
        (else
          (variabile (cdr l) (cons (pregexp-split #rx" *= *" (car l) 0) val)))))

(define (instruct i v val)
  (cond ((string=? (car i) "") val)
        ((string=? (car i) "done") 
         (instruct (cdr i) v val))
        ((string=? (car i) "forward") 
         (instruct (cdr i) v (cons (car i) val)))
        (else (instruct (cdr i) v (cons (let iter ((l v) (inloc (car i)) )
                                          (if (null? v) 
                                              (cons (car (pregexp-split #rx" " inloc 0)) (list (cadr (pregexp-split #rx" " inloc 0))))
                                              (cond ((string=? (car (cdr (pregexp-split #rx" " inloc 0))) (car (car l)))
                                                     (cons (car (pregexp-split #rx" " inloc 0))  (list (car(cdr(car l))))))
                                                    ((string=? (car (cdr (pregexp-split #rx" -*" inloc 0))) (car (car l)))
                                                     (if  (= (length (pregexp-split #rx"-" (car (cdr (car l))))) 2 )
                                                          (cons (car (pregexp-split #rx" " inloc 0)) (list (car (cdr (pregexp-split #rx"-" (car (cdr (car l))))))))
                                                          (cons (car (pregexp-split #rx" " inloc 0)) (list (string-append "-" (car(cdr(car l))))))))
                                                    (else (iter (cdr l) inloc))))) val))))) 



(define (forward p1 p2 color)
  ((draw-line vp) p1 p2 color))
(define (hex-conv car)
  (cond ((eq? car #\F) 15)
        ((eq? car #\A) 10)
        ((eq? car #\B) 11)
        ((eq? car #\C) 12)
        ((eq? car #\D) 13)
        ((eq? car #\E) 14)
        (else (string->number (list->string (list car))))))
(define (convert-angle a)
  (/ (* pi a) 180))
(define (cadran u)
  (cond ((or (and (>= -270 u) (<= -360 u) ) (and (>= 90 u) (<= 0 u))) 1)
        ((or (and (>= -180 u) (< -270 u) ) (and (>= 180 u) (< 90 u))) 2)
        ((or (and (>= -90 u) (< -180 u) ) (and (>= 270 u) (< 180 u))) 3)
        (else 4)))

(define (coord pos angle)
   (make-posn (+ (posn-x pos) (* d (cos (convert-angle angle)))) (+ (posn-y pos) (* d (sin (convert-angle angle))))))
(define (color list)
  (make-rgb (/ (car list) 255) (/ (cadr list) 255) (/ (caddr list) 255)))
(define (convert s)
  (let iter ((st s) (i 1) (rez '()) (val 0))
    (cond ((null? st) rez)
          (else
           (cond ((= (modulo i 2) 0) 
                 (iter (cdr st) (add1 i) (cons (+ val  (hex-conv (car st))) rez) 0))
                 (else (iter (cdr st) (add1 i) rez  (* (hex-conv (car st)) 16))))))))

(define (main linstr)
  (define init (make-posn 200 300))
  (let iter ((i linstr)(pos1 init) (col "black") (ang 0))
    (if (null? i)
        (display "end")
        (if (list? (car i))
            (if (string=? (caar i) "turn" ) 
                (iter (cdr i) pos1 col  (+ ang (string->number (cadar i))))
                (iter (cdr i) pos1 (color (convert (string->list (cadar i)))) ang))
            (if (void? (forward pos1 (coord pos1 ang) col))
                (iter (cdr i) (coord pos1 ang) col ang)
                (display "ceva"))))))

(variabile var '())
(open-graphics)
(define vp (open-viewport "Turtle graphics" 1000 800))
(main (instruct instr (variabile var '()) '()))