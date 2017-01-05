;utility
(defmacro for (var start stop &body body)
   (let ((gstop (gensym)))
       `(do ((,var ,start (1+ ,var))
	         (,gstop ,stop))
		    ((> ,var ,gstop))
		   ,@body)))

(defun slice (lst a b)
   (if (zerop b)
       (list (car lst))
       (if (zerop a)
           (cons (car lst) (slice (cdr lst) a (1- b)))
           (slice (cdr lst) (1- a) (1- b)))))

(defun find-element (lst)
   (for i 1 9
        (if (find i lst) 
            nil
            (return-from find-element i))))

(defun array2d (lst a b)
   (nth b (nth a lst)))

;sudoku
(setf sboard '((1 2 3 4 5 6 7 8 9)
              (4 5 6 7 8 9 1 2 3)
              (0 8 9 1 2 3 4 5 6)
              (2 3 4 5 6 7 8 9 1)
              (5 6 7 8 9 1 2 3 4)
              (8 9 1 2 3 4 5 6 7)
              (3 4 5 6 7 8 9 1 2)
              (6 7 8 9 1 2 3 4 5)
              (9 1 2 3 4 5 6 7 8)))


(defun check-num (numlist &optional (lst nil))
   (if (null numlist)
       (sort lst #'<)
       (if (or (zerop (car numlist)) (find (car numlist) lst))
           (check-num (cdr numlist) lst)
           (check-num (cdr numlist) (cons (car numlist) lst)))))

(defun set-row (board num)
   (nth num board ))

(defun set-column (board num)
   (if (null board)
       nil
       (cons (nth num (car board))
             (set-column (cdr board) num))))

(defun search-block (row clmn)
   (let ((x (truncate row 3))
         (y (truncate clmn 3)))
        (list x y)))

(defun take-box (board point)
   (list (slice (nth (* (cadr point) 3) board)
                (* (car point) 3)
                (+ (* (car point) 3) 2))
         (slice (nth (+ (* (cadr point) 3) 1) board)
                (* (car point) 3)
                (+ (* (car point) 3) 2))
         (slice (nth (+ (* (cadr point) 3) 2) board)
                (* (car point) 3)
                (+ (* (car point) 3) 2))))

(defun set-box (board row clmn)
       (let ((box (take-box board (search-block row clmn))))
            (append (car box) (cadr box) (caddr box))))

(defun finish? (board)
   (for i 0 8
        (for j 0 8
             (if (zerop (array2d board i j))
                 (return-from finish? nil))
                 (if (= i j 8) (return-from finish? t)))))
			                 
(defun sudoku (board)
   (let ((condidate nil))
        (for row 0 8
             (for colmn 0 8
                  (progn (setf condidate (check-num (set-box board colmn row) 
                                         (check-num (set-column board colmn)
                                         (check-num (set-row board row)))))
                               (if (= 8 (length condidate))
                                   (setf (nth colmn (nth row board)) (find-element condidate))))))))

(defun run (board)
   (when (not (finish? board))
         (sudoku board)))
