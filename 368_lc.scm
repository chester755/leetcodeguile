;; 368. Largest Divisible Subset
;; Medium

;; 1962

;; 93

;; Add to List

;; Share
;; Given a set of distinct positive integers nums, return the largest subset answer such that every pair (answer[i], answer[j]) of elements in this subset satisfies:

;; answer[i] % answer[j] == 0, or
;; answer[j] % answer[i] == 0
;; If there are multiple solutions, return any of them.



;; Example 1:

;; Input: nums = [1,2,3]
;; Output: [1,2]
;; Explanation: [1,3] is also accepted.
;; Example 2:

;; Input: nums = [1,2,4,8]
;; Output: [1,2,4,8]


;; Constraints:

;; 1 <= nums.length <= 1000
;; 1 <= nums[i] <= 2 * 109
;; All the integers in nums are unique.

;;Very similar to 300 we essentially have to build a list and check if value is dividable
;;Generally speaking if we can find the vlaue that is just before and after that it should just work


(use-modules ((rnrs) :version (6)))


(define comp
  (lambda (a b)
    (if (> a b)
	#t
	#f)))

(define replaceValue
  (lambda (firstlist secondlist value curpos pos)
    (cond
     ((eq? curpos pos) (append (append firstlist (list value)) (cdr secondlist)))
     (else
      (replaceValue (append firstlist (list (car secondlist))) (cdr secondlist) value (+ curpos 1) pos)))))

(define setlist
  (lambda (sublist pos value)
    (replaceValue '() sublist value 0 pos)))

(define half
  (lambda (start end)
    (+ start (round (/ (- end start) 2)))))

(define last
  (lambda (curlist)
    (cond
     ((null? (cdr curlist)) (car curlist))
     (else (last (cdr curlist))))))


(define divConqur
  (lambda (sublist value start end)
    (let ((midpoint (half start end)))
      (display sublist)
      (format #t "~%~d~%~d~%~d~%" value start end)
      (cond
       ;;First one if it is empty always have one
       ((null? sublist) (append sublist (list value)))
       ;;If there is value smaller than first one give
       ((< value (car sublist))
	(cond
	 ((eq? (remainder (car sublist) value) 0) (reverse (append (reverse sublist) (list value))))
	 (else sublist)
	 ;;((eq? (remainder value (car (cdr sublist)) 0)) (setlist sublist 0 value))
	 ))
       ((> value (last sublist))
	(if (eq? (remainder value (last sublist)) 0)
	    (append sublist (list value))
	    sublist))       
       ((and (< value (list-ref sublist midpoint))
	     (> value (list-ref sublist (- midpoint 1))))
	(if (and (eq? (remainder value (list-ref sublist midpoint)) 0)
		 (eq? (remainder value (list-ref sublist (- midpoint 1))) 0))
	    (setlist sublist midpoint value)
	    sublist))
       ((> value (list-ref sublist midpoint)) (divConqur sublist value midpoint end))
       ((< value (list-ref sublist midpoint)) (divConqur sublist value start midpoint))))))
		 


(define throughList
  (lambda (nolist newlist)
    (cond
     ;;Finished iterating first list then we return
     ((null? nolist) newlist)
     (else (throughList (cdr nolist) (divConqur newlist (car nolist) 0 (length newlist)))))))
     
     
(define largestDivSub
  (lambda (nolist)
    (let ((sorted-nolist (reverse (list-sort comp nolist))))
      (throughList sorted-nolist '()))))
      
	  
