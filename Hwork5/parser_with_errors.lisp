;;; Token management
(defparameter *tokens* nil)  ; Global list to hold tokens
(defparameter *current-token* nil)  ; Current token being processed

(defun next-token ()
  "Advance to the next token."
  (setq *current-token* (pop *tokens*)))

(defun match (expected)
  "Check if the current token matches the expected token, else throw an error."
  (if (equal *current-token* expected)
      (next-token)
      (error "Unexpected token: ~a, expected: ~a" *current-token* expected)))

;;; Parser functions for each non-terminal
(defun parse-E ()
  "Parse the E rule."
  (cond ((equal *current-token* 'x) (next-token))
        ((equal *current-token* 'y) (next-token))
        ((equal *current-token* 'z) (next-token))
        (t (error "Unexpected token in E rule: ~a" *current-token*))))

(defun parse-L ()
  "Parse the L rule."
  (if (equal *current-token* 's)
      (progn
        (next-token)
        (when (equal *current-token* 's)  ; Check if another 's' follows
          (parse-L)))
      (error "Unexpected token in L rule: ~a, expected: s" *current-token*)))

(defun parse-S ()
  "Parse the S rule."
  (cond ((equal *current-token* 's) (next-token))
        ((equal *current-token* 'd) 
         (next-token) 
         (parse-L) 
         (match 'b))
        (t (error "Unexpected token in S rule: ~a, expected: s or d" *current-token*))))

(defun parse-I ()
  "Parse the I rule."
  (if (equal *current-token* 'i)
      (progn
        (next-token)
        (parse-E)
        (parse-S))
      (error "Unexpected token in I rule: ~a, expected: i" *current-token*)))

(defun parse (input)
  "Initialize the token list and start parsing."
  (setq *tokens* input)
  (setq *current-token* nil)  ; Reset current token
  (next-token)  ; Get first token
  (handler-case
      (progn
        (parse-I)
        (if *current-token*  ; Check if there are remaining tokens
            (error "Extra tokens at the end: ~a" *current-token*)
            (format t "Parsing successful!~%")))
    (error (c)
      (format t "Error: ~a~%" c))))

;; Testing functions
(defun generate-valid-tests ()
  "Generate 7 valid test cases conforming to the grammar."
  '((i x s)
    (i y s)
    (i z s)
    (i x d s b)
    (i y d s s b)
    (i z d s s s b)
    (i x d s s s s b)))

(defun generate-invalid-tests ()
  "Generate 7 invalid test cases by perturbing valid ones."
  '((i x)          ; Missing 'S'
    (i y d s b s)  ; Extra token at end
    (i x d x b)    ; 'x' where 's' expected in 'L' rule
    (d s b)        ; Missing 'i' at start
    (x i s)        ; Wrong order of tokens
    (i x d b)      ; Missing 's' in dLb
    (i a s)))      ; Invalid token 'a' in E rule

(defun test-parser ()
  "Test the parser with valid and invalid inputs."
  (with-open-file (log-file "test_results.txt" :direction :output :if-exists :supersede)
    (format log-file "=== Running Tests ===~%~%")
    
    (format t "~%=== Testing Valid Inputs ===~%")
    (format log-file "=== Testing Valid Inputs ===~%")
    (dolist (test (generate-valid-tests))
      (format t "Testing input: ~a~%" test)
      (format log-file "Testing input: ~a~%" test)
      (handler-case
          (progn
            (parse test)
            (format log-file "Result: Parsing successful!~%~%"))
        (error (c)
          (format t "Error: ~a~%" c)
          (format log-file "Result: Error: ~a~%~%" c))))
    
    (format t "~%=== Testing Invalid Inputs ===~%")
    (format log-file "~%=== Testing Invalid Inputs ===~%")
    (dolist (test (generate-invalid-tests))
      (format t "Testing input: ~a~%" test)
      (format log-file "Testing input: ~a~%" test)
      (handler-case
          (progn
            (parse test)
            (format log-file "Result: Parsing successful! (Unexpected)~%~%"))
        (error (c)
          (format t "Error: ~a~%" c)
          (format log-file "Result: Error: ~a~%~%" c))))
    
    (format t "~%Test results saved to test_results.txt~%")))

(defun test-errors-only ()
  "Run parser tests and only show error messages."
  (with-open-file (log-file "error_results.txt" :direction :output :if-exists :supersede)
    (dolist (test (append (generate-valid-tests) (generate-invalid-tests)))
      (handler-case
          (parse test)  ; Try parsing
        (error (c)
          (format t "Testing input: ~a~%Error: ~a~%~%" test c)
          (format log-file "Testing input: ~a~%Error: ~a~%~%" test c)))))
  (format t "Error results saved to error_results.txt~%"))

;; Function to run all tests
(defun run-all-tests ()
  (format t "~%=== Running Parser Tests ===~%")
  (test-parser))