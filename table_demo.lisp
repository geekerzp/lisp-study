(setf things '((object1 large green shiny cube)
               (object2 small red dull metal cube)
               (object3 red small dull plastic cube)
               (object4 small dull blue metal cube)
               (object5 small shiny red four-sided pyramid)
               (object6 large shiny green sphere)))

(setf quality-table '((large . size)
                      (small . size)
                      (red   . color)
                      (green . color)
                      (blue  . color)
                      (shiny . luster)
                      (dull  . luster)
                      (metal . material)
                      (plastic . material)
                      (cube    . shape)
                      (sphere  . shape)
                      (pyramid . shape)
                      (four-sided . shape)))

(defun description (x)
  (rest (assoc x things)))

(defun differences (x y)
  (set-exclusive-or (description x)
                    (description y)))

(defun quality (x)
  (cdr (assoc x quality-table)))

(defun quality-difference (x y)
  (quality (first (differences x y))))

(defun contrast (x y)
  (remove-duplicates
   (sublis quality-table (differences x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf nerd-states
      '((sleeping . eating)
        (eating . waiting)
        (waiting . programming)
        (programming . debugging)
        (debugging . sleeping)))

(defun nerdus (x)
  (cdr (assoc x nerd-states)))

(defun sleepless-nerd (x)
  (let ((y (nerdus x)))
    (if (equal y 'sleeping)
        (nerdus y)
        y)))

(defun nerd-on-caffeine (x)
  (nerdus (nerdus x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun swap-first-last (x)
  (let* ((a (reverse (rest x)))
         (b (reverse (rest a))))
    (cons (first a)
          (append b (list (first x))))))

(defun rotate-left (x)
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  (let ((r (reverse x)))
    (cons (first r)
          (reverse (rest r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf rooms
      '((living-room
         (north front-stairs)
         (south dining-room)
         (east  kitchen))
        (upstairs-bedroom
         (west library)
         (south front-stairs))
        (dining-room
         (north living-room)
         (east pantry)
         (west downstairs-bedroom))
        (kitchen
         (west living-room)
         (south pantry))
        (pantry
         (north kitchen)
         (west dining-room))
        (downstairs-bedroom
         (north back-stairs)
         (east dining-room))
        (back-stairs
         (south downstairs-bedroom)
         (north libary))
        (front-stairs
         (north upstairs-bedroom)
         (south living-room))
        (library
         (east upstairs-bedroom)
         (south back-stairs))))

(defun choices (room)
  (rest (assoc room rooms)))

(defun look (dir room)
  (second (assoc dir (choices room))))

(setf loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting the variable LOC."
  (setf loc place))

(defun how-many-choice ()
  (length (choices loc)))

(defun upstairsp (x)
  (or (equal x 'library)
      (equal x 'upstairs-bedroom)))

(defun onstairsp (x)
  (or (equal x 'back-stairs)
      (equal x 'front-stairs)))

(defun where ()
  (if (onstairsp loc)
      (list 'robbie 'is 'on 'the loc)
      (list 'robbie 'is
            (if (upstairsp loc)
                'upstairs
                'downstairs)
            'in 'the loc)))

(defun move (dir)
  (let ((new-loc (look dir loc)))
    (cond ((null new-loc)
           '(ouch! robbie hit a wall))
          (t (set-robbie-location new-loc)
             (where)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun royal-we (sent)
  (subst 'we 'i sent))
