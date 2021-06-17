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
	(first)
	(second))))
  

(define LISTLength
  (lambda (redList prevValue longestLength curLength)
    (cond
     ((and (null? redList)
	   (> curLength longestLength)) curLength)
     ((null? redList) longestLength)
     ((> curLength longestLength) (LISTLength redList prevValue curLength curLength))
     ((< prevValue (car redList)) (LISTLength (cdr redList) (car redList) longestLength (+ curLength 1)))
     (else (compReturn (LISTLength (cdr redList) (car redList) longestLength (+ curLength 1)) (LISTLength (cdr redList) (car redList) longestLength '1)))
     )))
	  
(define LISub
  (lambda (nolist)
    (if (list? nolist)
	(LISTLength (cdr nolist) (car nolist) 1 1)
	(format #t "Sorry wrong input~%"))))
	
	
