(define TrigTable
    (begin
		(format #t "~4a ~4a ~4a ~4a\n" "deg" "sin" "cos" "tan")
		(lambda (count)
			(let loop ((count count))
				 (if (= count 180) `quit
					 (begin
						 (format #t "~4a ~4f ~4f ~4f\n" count (sin count) (cos count) (tan count))
						 (loop (+ 5 count))
					 )
				 )
			)
		)
	)
)


(format #t "~a\n" (TrigTable 0))


