(setf note-table
      '((c . 1)
        (c-sharp . 2)
        (d . 3)
        (d-sharp . 4)
        (e . 5)
        (f . 6)
        (f-sharp . 7)
        (g . 8)
        (g-sharp . 9)
        (a . 10)
        (a-sharp . 11)
        (b . 12)))

(defun numbers (x)
  (mapcar #'(lambda (e)
              (cdr (assoc e note-table)))
          x))

(defun notes (x)
  (mapcar #'(lambda (e)
              (car (rassoc e note-table)))
          x))

(defun raise (n x)
  (mapcar #'(lambda (e) (+ e n))
          x))

(defun normalize (x)
  (mapcar #'(lambda (e)
              (cond ((< e 1) (+ e 12))
                    ((> e 12) (- e 12))
                    (t e)))
          x))

(defun transpose (n x)
  (notes (normalize (raise n (numbers x)))))



