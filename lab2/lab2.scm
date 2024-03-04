(define data (list 9 8 -7 6 5 -4 3 -2 -1 0))

(define MakeListMore
    (lambda (l P)
        (let loop ((l l) (P P) (res `()))
            (begin
                 (if (null? l)
                    res
                    (if (> (car l) P) (loop (cdr l) P (cons (car l) res))
                        (loop (cdr l) P res)
                    )
                )
            )
        )
    )
)


(define MakeListLess
    (lambda (l P)
        (let loop ((l l) (P P) (res `()))
            (begin
                 (if (null? l)
                    res
                    (if (<= (car l) P) (loop (cdr l) P (cons (car l) res))
                        (loop (cdr l) P res)
                    )
                )
            )
        )
    )
)


(define Reverse 
    (lambda (l)
        (let loop ((l l) (res '()))
            (if (null? l) res
                (loop (cdr l) (cons (car l) res))
            )
        )
    )
)


(define MyCons
    (lambda (l1 l2)
        (let loop ((l1 (Reverse l1)) (l2 l2))
            (if (null? l1) l2
                (loop (cdr l1) (cons (car l1) l2))
            )
        )
    )
)


(define QuickSort
    (lambda (l)  
        (let loop ((l l))
            (if (or (null? l) (null? (cdr l))) l
                (MyCons (loop (MakeListLess l (car l))) (loop (MakeListMore l (car l))))
            )
        )
    )
)


(format #t "~a" (QuickSort data))

