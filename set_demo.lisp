(setf male-first-names
      '(john kim richard fred george))

(setf female-first-names
      '(jane mary wanda barbara kim))

(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(defun malep (name)
  (and (member name male-first-names)
       (not (member name female-first-names))))

(defun femalep (name)
  (and (member name female-first-name)
       (not (member name male-first-names))))

(defun give-title (name)
  "Returns a name with an appropriate title on the font."
  (cond ((titledp name) name)
        ((malep (first name)) (cons 'mr name))
        ((femalep (first name)) (cons 'ms name))
        (t (append '(mr or ms) name))))

(defun gender-ambiguous-names ()
  (intersection male-first-names female-first-names))

(defun uniquely-male-names ()
  (set-difference male-first-names female-first-names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun right-side (x)
  (rest (member '-vs- x)))

(defun left-side (x)
  (right-side (reverse x)))

(defun count-common (x)
  (length (intersection (left-side x) (right-side x))))

(defun compare (x)
  (list (count-common x) 'common 'features))
