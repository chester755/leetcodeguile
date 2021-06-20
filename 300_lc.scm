;; Description
;; Solution
;; Discuss (999+)
;; Submissions
;; 300. Longest Increasing Subsequence
;; Medium

;; 7556

;; 165

;; Add to List

;; Share
;; Given an integer array nums, return the length of the longest strictly increasing subsequence.

;; A subsequence is a sequence that can be derived from an array by deleting some or no elements without changing the order of the remaining elements. For example, [3,6,2,7] is a subsequence of the array [0,3,1,6,2,2,7].

 

;; Example 1:

;; Input: nums = [10,9,2,5,3,7,101,18]
;; Output: 4
;; Explanation: The longest increasing subsequence is [2,3,7,101], therefore the length is 4.
;; Example 2:

;; Input: nums = [0,1,0,3,2,3]
;; Output: 4
;; Example 3:

;; Input: nums = [7,7,7,7,7,7,7]
;; Output: 1
 

;; Constraints:

;; 1 <= nums.length <= 2500
;; -104 <= nums[i] <= 104
 

;; Follow up: Can you come up with an algorithm that runs in O(n log(n)) time complexity?

(define compReturn
  (lambda (first second)
    (if (> first second)
	first
	second)))

(define displayValue
  (lambda (value)
    (display value)))

(define LISTLength
  (lambda (redList prevValue longestLength curLength)
    (cond
     ((and (null? redList)
	   (> curLength longestLength)) curLength)
     ((null? redList) longestLength)
     ((> curLength longestLength) (LISTLength redList prevValue curLength curLength))
     ((< prevValue (car redList)) (LISTLength (cdr redList) (car redList) longestLength (+ curLength 1)))
     (else (compReturn (LISTLength (car redList) (car redList) longestLength curLength) (LISTLength (cdr redList) (car redList) longestLength 1)))
     ;(else (displayValue (LISTLength (cdr redList) (car redList) longestLength (+ curLength 1))))
     )))
	  
(define LISub
  (lambda (nolist)
    (if (list? nolist)
					;(LISTLength (cdr nolist) (car nolist) 1 1)
	(locList (cdr nolist) '() (car nolist) 0 (length nolist))
	(format #t "Sorry wrong input~%"))))

(define half
  (lambda (start end)
    (+ start (ceiling (/ (- end start) 2)))))

(define replace-n
  (lambda (orglist firstlist secondlist value curpos position)
    (cond
					;3 sanity check if curpos > position it return false
					;if position > length of list than it is wrong
					;if list is null than obviously wrong
					;then we check if curpos ==  position if so, return replace the contains and return it
					;else we just keep update it
     ((> curpos position) #f)
     ((> position (length orglist)) #f)
     ((null? orglist) #f)
     ((eq? curpos position) (append (append firstlist (list value)) (cdr secondlist)))
     ((< curpos position) (replace-n orglist (append firstlist (cdr secondlist)) (cdr secondlist) value (+ curpos 1) position))
     )))
					;(con firstlist secondlist)


(define throughList

  ;;This function should divide and conqur the list. i.e. find and replace. until found one 
(define locList
  ;;One of the best way is divide and conqur, so we know the length of the element, and we find the middle and see which partition it is in and keep doing that
  (lambda (orglist sublist value startpos endpos)
    (let ((halfpoint (half startpos endpos)))
      (cond
					;First condition if empty list let's fill it
       ((null? sublist) (append sublist (list (car orglist))) (loc)
					;Second case if it is smaller than first one, let's replace first one
       ((< value (car sublist)) (list-set! sublist 0 (list (car orglist)))) ;(replace-n sublist '() sublist value 0 1))
					;Third case if we found the correct one
       ((and (< value (list-ref sublist halfpoint))
	     (> value (list-ref sublist (- halfpoint 1)))) (list-set! sublist (- halfpoint 1) value));(replace-n orglist firstlist secondlist value 0 halfpoint))
       ((> value (list-ref sublist halfpoint)) (locList orglist sublist value halfpoint endpos))
       ((< value (list-ref sublist halfpoint)) (locList orglist sublist value startpos halfpoint))
       ))))


	    
  

;; (define SeqList
;;   (lambda (nolist sublist)
;;     (cond
;;      ((null? nolist) sublist)
;;      ((< (reverse sublist) (car nolist))
      
