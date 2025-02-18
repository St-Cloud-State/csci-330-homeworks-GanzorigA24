;; Merge function (from Question 2)
(defun merge-lists (lst1 lst2)
  (cond
    ((null lst1) lst2) ; If lst1 is empty, return lst2
    ((null lst2) lst1) ; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2)) ; If the first element of lst1 is smaller
     (cons (car lst1) (merge-lists (cdr lst1) lst2))) ; Cons it and recurse
    (t ; Otherwise, cons the first element of lst2 and recurse
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

;; Function to partition the list into sorted pairs
(defun partition-into-pairs (lst)
  (if (null lst)
      nil
      (let ((pair (if (null (cdr lst)) ; Check if there's only one element left
                      (list (car lst)) ; Create a single-element list
                      (list (car lst) (cadr lst))))) ; Otherwise, create a pair
        (cons (sort pair #'<) ; Sort the pair or single element
              (partition-into-pairs (if (null (cdr lst))
                                        nil
                                        (cddr lst))))))) ; Recurse on the rest of the list

;; Function to merge adjacent lists in a list of lists
(defun merge-adjacent-lists (lst)
  (if (null (cdr lst))
      lst
      (cons (merge-lists (car lst) (cadr lst)) ; Merge the first two lists
            (merge-adjacent-lists (cddr lst))))) ; Recurse on the rest of the lists

;; Bottom-up Mergesort
(defun bottom-up-mergesort (lst)
  (let ((sorted-pairs (partition-into-pairs lst))) ; Step 1: Partition into sorted pairs
    (do ((merged sorted-pairs (merge-adjacent-lists merged))) ; Step 2: Merge adjacent lists
        ((<= (length merged) 1) (car merged))))) ; Step 3: Repeat until one list remains

;; Test the bottom-up Mergesort function
(format t "Sorted list: ~a~%" (bottom-up-mergesort '(1 5 7 2 4 3 9 2 4 3)))