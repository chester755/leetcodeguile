;; Given a non-empty array nums containing only positive integers, find if the array can be partitioned into two subsets such that the sum of elements in both subsets is equal.



;; Example 1:

;; Input: nums = [1,5,11,5]
;; Output: true
;; Explanation: The array can be partitioned as [1, 5, 5] and [11].
;; Example 2:

;; Input: nums = [1,2,3,5]
;; Output: false
;; Explanation: The array cannot be partitioned into equal sum subsets.


;; Constraints:

;; 1 <= nums.length <= 200
;; 1 <= nums[i] <= 100

;; Try a working solution recursive


;; Idea here is to build a tree to solve it.


(define sumlist
  (lambda (nolist value)
    (if (null? nolist)
	value
	(sumlist (cdr nolist) (+ value (car nolist))))))

(define treebranch
  (lambda (nolist target sum)
    (let ((reached #f))
      (display nolist)
      (while (not (null? nolist))
	(format #t "sum~^:~_~d~_target~^:~_~d~_list~^:~_~{~d~_~}~%" sum target nolist)
	(cond
	 ((eq? sum target) (set! reached #t) (display "reached") (break))
	 ((eq? (+ sum (car nolist)) target) (set! reached #t) (display "reached2") (break))
	 ((< (+ sum (car nolist)) target) (set! sum (+ sum (car nolist))) (set! nolist (cdr nolist)))
	 (else (set! nolist (cdr nolist)))))
      reached)
    ))
	
	 
    ;; (let ((indexi 0))
    ;;   (while (< indexi (length nolist))
    ;; 	(let ((indexj indexi))
    ;; 	  (while (< indexj (length nolist))
    ;; 	    (+ sum (list-ref nolist indexj))
    ;; 	    (cond
    ;; 	     ((eq? sum target) break)
    ;; 	     ((> sum target) break)
    ;; 	     (else (+ indexj 1))))
    ;; 	  (+ indexi 1))))))


(define check
  (lambda (nolist)
    (do ((indexi 0 (+ indexi 1)))
	((>= indexi (length nolist)))
      (do ((indexj indexi (+ indexj 1)))
	  ((>= indexj (length nolist)))
	(display (list-ref nolist indexj))
	(format #t "~%")))))



;;This will need an another rapper
(define recursive-solve
  (lambda (nolist targetvalue)
    (cond
     ;;If nothing was return unless the last one, clearly it is faults
     ((null? (cdr nolist)) #f)
     ((treebranch nolist targetvalue 0) #t)
     (else (recursive-solve (cdr nolist) targetvalue)))))


(define changelistvalue
  (lambda (carlist cdrlist curpos pos value)
    (cond
     ((eq? curpos pos) (append (append carlist (list value)) (cdr cdrlist)))
     ((< curpos pos) (changelistvalue (append carlist (list (car cdrlist))) (cdr cdrlist) (+ curpos 1) pos value)))))
     
(define setlist
  (lambda (nolist value pos)
    (changelistvalue '() nolist 0 pos value)))
    

(define knapback
  (lambda (nolist targetvalue)
    (let ((truelist (make-list (+ targetvalue 1) #f)))
      (set! truelist (setlist truelist #t 0))
      (while (not (null? nolist))
	;;(format #t "~{~d~_~}~%" nolist)
	(display truelist)
	(format #t "~%")
	(do ((value targetvalue (- value 1)))
	    ((<= value 0))
	  (format #t "~d~_~d~%" value (car nolist))
	  (if (>= value (car nolist))
	      (set! truelist (setlist truelist (or (list-ref truelist value)
						   (list-ref truelist (- value (car nolist)))) value))))
	(set! nolist (cdr nolist)))
      (display truelist)
      (format #t "~%")
      (list-ref truelist targetvalue))))


	  
(define partition
  (lambda (nolist)
    (let ((total (sumlist nolist 0)))
      (if (odd? total)
	  #f
	  (recursive-solve nolist (/ total 2))))))
