(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))

(defun all-oddp (x)
  (cond ((null x) t)
        ((evenp (first x)) nil)
        (t (all-oddp (rest x)))))

(defun rec-member (e x)
  (cond ((null x) nil)
        ((equal e (first x)) x)
        (t (rec-member e (rest x)))))

(defun rec-assoc (key table)
  (cond ((null table) nil)
        ((equal key (car (first table))) (first table))
        (t (rec-assoc (rest table)))))

(defun rec-nth (n x)
  (cond ((zerop n) (first x))
        (t (rec-nth (- n 1) (rest x)))))

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

(defun fib (n)
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(defun find-first-odd (x)
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (find-first-odd (rest x)))))

(defun last-element (x)
  (cond ((atom (cdr x)) (car x))
        (t (last-element (cdr x)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (- n 1))))))


(defun all-equal (x)
  (cond ((null (rest x)) t)
        ((not (equal (first x) (second x))) nil)
        (t (all-equal (rest x)))))

(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

(defun applic-fact (n)
  (reduce #'* (count-down n)))

(defun count-down (n)
  (cond ((equal -1 n) nil)
        (t (cons n (count-down (- n 1))))))

(defun count-down (n)
  (cond ((zerop n) (list n))
        (t (cons n (count-down (- n 1))))))

(defun square-list (x)
  (cond ((null x) nil)
        (t (cons (* (first x) (first x))
                 (square-list (rest x))))))

(defun my-nth (n x)
  (cond ((null x) nil)  ; stop if list is empty.
        ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))

(defun my-member (e x)
  (cond ((null x) nil)
        ((equal e (first x)) x)
        (t (my-member e (rest x)))))

(defun my-assoc (key table)
  (cond ((null table) nil)
        ((equal key (first (car table))) (first table))
        (t (my-assoc key (rest table)))))

(defun compare-length (x y)
  (cond ((and (null x) (null y)) 'same-length)
        ((null x) 'second-is-long)
        ((null y) 'first-is-long)
        (t (compare-lengths (rest x)
                            (rest y)))))

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
        ((numberp (first x))
         (+ (first x)
            (sum-numeric-elements (rest x))))
        (t (sum-numeric-elements (rest x)))))

(defun my-remove (e x)
  (cond ((null x) nil)
        ((equal e (first x))
         (my-remove e (rest x)))
        (t (cons (first x) (my-remove e (rest x))))))

(defun my-intersection (x y)
  (cond ((null x) nil)
        ((member (first x) y)
         (cons (first x)
               (my-intersection (rest x) y)))
        (t (my-intersection (rest x) y))))

(defun my-set-difference (x y)
  (cond ((null x) nil)
        ((not (member (first x) y))
         (cons (first x)
               (my-set-difference (rest x) y)))
        (t (my-set-difference (rest x) y))))

(defun count-odd-conditional (x)
  ;; conditional augmentation version
  (cond ((null x) 0)
        ((oddp (first x))
         (+ 1 (count-odd-conditional (rest x))))
        (t (count-odd-conditional (rest x)))))

(defun count-odd (x)
  ;; regular augmentation version
  (cond ((null x) 0)
        (t (+ (if (oddp (first x))
                  1
                  0)
              (count-odd (rest x))))))

(defun find-number (x)
  (cond ((numberp x) x)
        ((atom x) nil)
        (t (or (find-number (car x))
               (find-number (cdr x))))))

(defun atoms-to-q (x)
  (cond ((null x) nil)
        ((atom x) 'q)
        (t (cons (atoms-to-q (car x))
                 (atoms-to-q (cdr x))))))

(defun count-atoms (x)
  (cond ((atom x) 1)
        (t (+ (count-atoms (car x))
              (count-atoms (cdr x))))))

(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
        (t (cons cnt
                 (count-up-recursively
                  (+ cnt 1)
                  n)))))

(defun count-up (n)
  (cond ((zerop n) nil)
        (t (append (count-up (- n 1))
                   (list n)))))

(defun make-loaf (n)
  (if (zerop n) nil
      (cons 'x (make-loaf (- n 1)))))

(defun bury (x n)
  (cond ((zerop n) x)
        (t (list (bury x (- n 1))))))

(defun pairings (x y)
  (cond ((null x) nil)
        (t (cons (list (first x) (first y))
                 (pairings (rest x) (rest y))))))

(defun sublists (x)
  (cond ((null x) nil)
        (t (cons x (sublists (rest x))))))

(defun my-reverse (x)
  (reverse-recursively x nil))

(defun reverse-recursively (x y)
  (cond ((null x) y)
        (t (reverse-recursively (rest x)
                                 (cons (first x) y)))))

(defun my-union (x y)
  (append x (union-recursively x y)))

(defun union-recursively (x y)
  (cond ((null y) nil)
        ((member (first y) x)
         (union-recursively x (rest y)))
        (t (cons (first y)
                 (union-recursively
                  x
                  (rest y))))))

(defun largest-even (x)
  (cond ((null x) 0)
        ((oddp (first x))
         (largest-even (rest x)))
        (t (max (first x)
                (largest-even (rest x))))))

(defun huge (x)
  (huge-helper x x))

(defun huge-helper (x n)
  (cond ((equal n 0) 1)
        (t (* x (huge-helper x (- n 1))))))


(defun tr-count-up (n)
  (tr-count-up1 n nil))

(defun tr-count-up1 (n result)
  (declare (notinline tr-count-up1))
  (cond ((zerop n) result)
        (t (tr-count-up1
            (- n 1)
            (cons n result)))))

(defun tr-fact (n)
  (tr-fact1 n 1))

(defun tr-fact1 (n result)
  (declare (notinline tr-fact1))
  (cond ((zerop n) result)
        (t (tr-fact1 (- n 1) (* n result)))))

(defun my-mapcar (fn x)
  (cond ((null x) nil)
        (t (cons (funcall fn (first x))
                 (my-mapcar (fn (rest x)))))))

(defun tree-find-if (pred tree)
  (cond ((and tree
              (atom tree)
              (funcall pred tree))
         tree)
        ((atom tree) nil)
        (t (or (tree-find-if
                pred (car tree))
               (tree-find-if
                pred (cdr tree))))))

(defun count-up (n)
  (labels ((count-up-recursively (cnt)
             (if (> cnt n) nil
                 (cons cnt
                       (count-up-recursively
                        (+ cnt 1))))))
    (count-up-recursively 1)))


(defun tr-count-slices (x)
  (labels ((trc1 (x n)
             (if x
                 (trc1 (rest x)
                       (+ n 1))
                 n)))
    (trc1 x 0)))

(defun tr-reverse (x)
  (labels ((trrev1 (x r)
             (if x
                 (trrev1 (rest x)
                         (cons (first x) r))
                 r)))
    (trrev1 x nil)))
