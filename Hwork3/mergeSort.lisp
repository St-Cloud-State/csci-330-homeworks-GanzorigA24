;; Partition function
(defun partition (lst)
  (let ((half (floor (/ (length lst) 2))))
    (list (subseq lst 0 half) (subseq lst half))))

;; Merge function
(defun merge-lists (lst1 lst2)
  (cond
    ((null lst1) lst2) ; If lst1 is empty, return lst2
    ((null lst2) lst1) ; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2)) ; If the first element of lst1 is smaller
     (cons (car lst1) (merge-lists (cdr lst1) lst2))) ; Cons it and recurse
    (t ; Otherwise, cons the first element of lst2 and recurse
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

;; Mergesort function
(defun mergesort (lst)
  (if (<= (length lst) 1) ; Base case: if the list has 0 or 1 element, it's already sorted
      lst
      (let ((halves (partition lst))) ; Split the list into two halves
        (merge-lists (mergesort (first halves)) ; Sort the first half
                     (mergesort (second halves)))))) ; Sort the second half

;; Test the mergesort function
(format t "Sorted list: ~a~%" (mergesort '(4 3 1 7 4 9)))