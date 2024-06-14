(define Tree1 '(
							 ( 
								(() 4 ()) 
								2 
								(() 5 ()) 
							 ) 
							 1 
							 ( 
								(() 6 ()) 
								3 
								(() 7 ())
							 )
							)
)

(define Tree2 '(
							 ( 
								(() 4 ()) 
								2 
								(() 5 ()) 
							 ) 
							 1 
							 ( 
								(() 6 ()) 
								3 
								(() 7 ())
							 )
							)
)
(define left-branch car)
(define right-branch caddr)
(define node-value cadr)	

(define (Traverse-rec tree callback) 
 (if (not (null? tree))
	(begin
	 (Traverse-rec (left-branch tree) callback)
	 (callback (node-value tree))
	 (Traverse-rec (right-branch tree) callback)
	)
 )
)


(define (make-tree-iterator tree)
 (let ((caller #f))
  (letrec ((traverse  ;; traverse procedure
						(lambda ()
						 (let loop ((tree tree))
							(if (not (null? tree))
							 (begin
								(loop (left-branch tree))
								;; -- do st-th with key
								(call/cc (lambda (rest-of-tree) 
													(set! traverse (lambda () (rest-of-tree 'dummy)))
													(caller (node-value tree))
												 )
								)
								(loop (right-branch tree))
							 )
							)
						 )
						 (caller 'end)
							
						)
					)) ;; letrec

	 (lambda () ;; iterator procedure
		(call/cc (lambda (k) (set! caller k) (traverse)))
	 ) ;; iterator proc end
	)
 )
)

(define (compare-tree t1 t2)
    (let    (	(continue #f)
                (iter1 (make-tree-iterator t1))
                (iter2 (make-tree-iterator t2))        
            )
        (letrec ((loop (lambda (v1 v2)
                    (cond 
                        ((eq? v1 'end) (continue (eq? v2 'end)))
                        ((eq? v2 'end) (continue #f))
                        ((eq? v1 v2) (loop (iter1) (iter2)))
                        (else (continue #f))
                    )              
                )))
                (call/cc
                    (lambda (context)
                        (set! continue context)
                        (loop (iter1) (iter2))
                    )    
                )   
        )
    )    
)

(display (compare-tree Tree1 Tree2))(newline)



