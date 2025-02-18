(defun insert (item sorted)
  "Insert an item into the correct position in a sorted list."
  (cond
    ((null sorted) (list item)) ; Base case: insert at the end
    ((<= item (car sorted)) (cons item sorted)) ; Insert at the beginning
    (t (cons (car sorted) (insert item (cdr sorted)))))) ; Recursively insert

(defun insertion-sort (unsorted &optional (sorted nil))
  "Sort a list using insertion sort."
  (cond
    ((null unsorted) sorted) ; Base case: return the sorted list
    (t (insertion-sort (cdr unsorted) (insert (car unsorted) sorted)))))

;; Example usage:
(insertion-sort '(2 8 9 7 6 5 3))
;; Output: (1 2 3 4 5 6 7 8)