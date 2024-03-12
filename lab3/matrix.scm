(define (make-matrix matrix)
    (let ((matrix matrix) )
        (define (GetFRow) (car matrix))
        (define (GetSRow) (car (cdr matrix)))
        (define (GetFCol) 
            (list
                (car (GetFRow))
                (car (GetSRow))
            )
        )
        (define (GetSCol) 
            (list
                (car (cdr (GetFRow)))
                (car (cdr (GetSRow)))
            )
        )
        (define (Display) 
            (format #t "|~a|\n|~a|\n\n" (GetFRow) (GetSRow))
        )
        (define (GetValueOnPos x y) 
            (let ((fRow (GetFRow)) (sRow (GetSRow)) (x x) (y y))
                (if (= x 1)
                    (if (= y 1)
                        (car fRow)
                        (car (cdr fRow))
                    )
                    (if (= y 1)
                        (car sRow)
                        (car (cdr sRow))
                    )
                )
            )    
        )
    
    
        (define (Plus matrix2)
            (make-matrix 
                (list
                    (map + (GetFRow)
                        (matrix2 'GetFRow)
                    )
                    (map + (GetSRow)
                        (matrix2 'GetSRow)
                    )
                )
            )
        )
    
        (define (Minus matrix2)
            (make-matrix 
                (list
                    (map - (GetFRow)
                        (matrix2 'GetFRow)
                    )
                    (map - (GetSRow)
                        (matrix2 'GetSRow)
                    )
                )
            )
        )
    
        (define (MatMalt matrix2)
            (make-matrix
                (list
                    (list
                        (apply + 
                            (map * (GetFRow)
                                (matrix2 'GetFCol)
                            )   
                        )
                        (apply + 
                            (map * (GetFRow)
                                (matrix2 'GetSCol)
                            )   
                        )
                    )
                
                    (list
                        (apply + 
                            (map * (GetSRow)
                                (matrix2 'GetFCol)
                            )  
                        )
                        (apply + 
                            (map * (GetSRow)
                                (matrix2 'GetSCol)
                            )   
                        )
                    )
                )
            )    
        )
    
        (define (VecMalt vec)
            (list
                (apply + 
                    (map * (GetFRow)
                        vec
                    )   
                )
                (apply + 
                    (map * (GetSRow)
                        vec
                    )   
               )  
            )
        )
    
        (define (LamMalt n)
            (make-matrix
                (list
                    (map * (list n n) (GetFRow))   
                    (map * (list n n) (GetSRow)) 
                )
            )
        )
    
    
        (define (Transpose)
            (make-matrix
                (list
                    (GetFCol)
                    (GetSCol)
                )
            )
        )
    
    
        (lambda args
            (apply
                (case (car args)
                    ((GetFRow) GetFRow)
                    ((GetSRow) GetSRow)
                    ((GetFCol) GetFCol)
                    ((GetSCol) GetSCol)
                    ((Display) Display)
                    ((Plus) Plus)
                    ((Minus) Minus)
                    ((MatMalt) MatMalt)
                    ((VecMalt) VecMalt)
                    ((LamMalt) LamMalt)
                    ((Transpose) Transpose)
                    ((GetValueOnPos) GetValueOnPos)
                    (else (begin (display "Invalid method\n") (exit 1) ))
                )
                (cdr args)
            )
        )
    )
)
 


(define M1 (make-matrix (list (list 1 2) (list 3 4))))
(define M2 (make-matrix (list (list 5 6) (list 7 8))))
(M1 'Display)  
(M2 'Display) 
 
((M1 'Plus M2) 'Display) 

((M1 'Minus M2) 'Display) 

((M1 'LamMalt 2) 'Display) 

((M1 'MatMalt M2) 'Display)  

(display (M1 'VecMalt (list 2 3))) (newline) 

((M1 ' Transpose) 'Display) (newline)

(display (M1 'GetFRow)) (newline) 
(display (M1 'GetSRow)) (newline) 

(display (M1 'GetFCol)) (newline) 
(display (M1 'GetSCol)) (newline) 


(display (M1 'GetValueOnPos 2 2)) (newline) 
