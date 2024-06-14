(define-syntax useMemo
    (syntax-rules ()
        ((useMemo (define func (lambda (args ...) body ...)))
            (begin
                (define todo_func (lambda (args ...) body ...))
                (define func 
                    (letrec ((memo-data (make-hash-table 'equal?)))
                        (lambda (args ...)
                            (let ((key (list args ...)))
                                (if (hash-table-exists? memo-data key) 
                                    '()
                                    (begin
                                        (display "Calculating")(newline)
                                        (hash-table-set! memo-data key (todo_func args ...))
                                    )
                                     
                                )
                                (hash-table-ref memo-data key)  
                            )
                        )    
                    )
                )
            )         
        )    
    )
)


(useMemo 
(define fib
  (lambda (n i)
    (if (or (= n 1)
            (= n 2))
        1
        (+ (fib (- n 1) i)
          (fib (- n 2) i)
          i)))))

(display (fib 10 1))
(newline) 
(display (fib 10 1))
(newline) 