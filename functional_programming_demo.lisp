(setf database
      '((b1 shape brick)
        (b1 color green)
        (b1 size small)
        (b1 supported-by b2)
        (b1 supported-by b3)
        (b1 shape brick)
        (b2 color red)
        (b2 size small)
        (b2 supports b1)
        (b2 left-of b3)
        (b3 shape brick)
        (b3 color red)
        (b3 size small)
        (b3 supports b1)
        (b3 right-of b2)
        (b4 shape pyramid)
        (b4 color blue)
        (b4 size large)
        (b4 supported-by b5)
        (b5 shape cube)
        (b5 color green)
        (b5 size large)
        (b5 supports b4)
        (b6 shape brick)
        (b6 color purple)
        (b6 size large)))

(defun match-element (e q)
  (or (equal e q)
      (equal q '?)))

(defun match-triple (x part)
  (every #'match-element
         x
         part))

(defun fetch (pat)
  (remove-if-not
   #'(lambda (x) (match-triple x pat))
   database))

(defun color-pattern (block)
  (list block 'color '?))

(defun supporters (block)
  (mapcar #'first
          (fetch (list '? 'supports block))))

(defun supp-cube (block)
  (member 'cube
          (mapcar
           #'(lambda (b) (third (first (fetch (list b 'shape '?)))))
           (supporters block))))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'rest (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))
