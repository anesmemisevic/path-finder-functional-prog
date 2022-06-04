#lang scheme


(define (buildPath rows)
(cond
((null? rows) null)
(else (cons (buildPath (cdr rows))
(car rows)))))


(define sample-path (buildPath
'(
("S" "-" "-" "-" "-")
("E" "-" "-" "-" "-")
("E" "E" "E" "E" "E")
("-" "-" "-" "-" "E")
("-" "-" "-" "-" "F")
)
))

; Define two functions "getHeight" and "getWidth" which take a path as an input and return the height and the width of the path.


(define (getWidth path)
    (cond
     [(empty? path) 0]
     [else (length (cdr path))]))

(define countHeight 0)
(define (getHeight path)
    (cond
     [(null? path) 0]
     [else (+ 1 countHeight (getHeight (car path)))]))


;Define a function "getLetter" which takes a path, a row number and a column number.
;Then it returns the letter from the path on the corresponding location [row, column]
;(getLetter sample-path 0 0) → should return S
;(getLetter sample-path 1 0) → should return E
;(getLetter sample-path 1 1) → should return -
;(getLetter sample-path 4 4) → should return F

(define (getLetter_zero path row column)
    (when (= row 0)
        (list-ref (cdr path) column))
  )

(define (checkIfNegative x)
  (define (aux a b) 
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
          (
           else (aux (sub1 a) (add1 b)))
          ))
  (aux x x))

(define (getLetter path row column)
  (define c 0)
  (define (extract path count)
  (define count row)
  (define new_path path)
    ;(display "before")
    ;(display count)
   (cond
      [(= count 0) (when (= row 0)
        (list-ref (cdr path) column))]
      [else
     (set! count (- count 1))
     (set! new_path (car path)) ; take from car
     ;(display "after")
     ;(display count)
     (getLetter new_path count column)
    ])) (if (or
             (checkIfNegative row) (checkIfNegative column) (>= column (getWidth path))) "-"
                                                                                         (extract path c)))

(define (join list1 list2)
        (if (null? list1) list2 (cons (car list1) (join (cdr list1) list2))))

(define (stepAssertion path assertRow assertColumn direction)
    (and (not (equal? direction reverse)) (or (string-ci=? (getLetter path assertRow assertColumn) "F") (string-ci=? (getLetter path assertRow assertColumn) "E"))))

(define (solvePath path)
  (define x 0)
  (define y 0)
  (define reverseBar "-")
  (define returnList (list))
  (traverse path x y returnList reverseBar))
(define (traverse path X Y nextFreeLetter reverse)
  (define (try? assertRow assertColumn direction) (stepAssertion path assertRow assertColumn direction))
  (cond
    [(string-ci=? (getLetter path X Y) "F") nextFreeLetter]
    [(try? (+ X 1) Y 'D) (traverse path (+ X 1) Y (join nextFreeLetter '(D)) 'U)]
    [(try? X (+ Y 1) 'R) (traverse path X (+ Y 1) (join nextFreeLetter '(R)) 'L)]
    [(try? X (- Y 1) 'L) (traverse path X (- Y 1) (join nextFreeLetter '(L)) 'R)]
    [(try? (- X 1) Y 'U) (traverse path (- X 1) Y (join nextFreeLetter '(U)) 'D)]  
    )
  )


;(display "my getWidth ")
(getWidth sample-path)

;(display "my getHeigth ")
(getHeight sample-path)

(getLetter sample-path 0 0) 
(getLetter sample-path 1 0) 
(getLetter sample-path 1 1)
(getLetter sample-path 4 4)

(solvePath sample-path)


