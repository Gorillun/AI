(defun mc ()
  ( establish-world )
  ( init-move-list )
  ( make-moves )
)
(defun establish-world()
  ( setf *left-bank* '(m m m c c c b))
  ( setf *right-bank* '())
)

(defun init-move-list()
  ( setf *move-list* '())
)

(defun display-world ()
  ( format t "left-bank: ~A~%" *left-bank* )
  ( format t "right-bank: ~A~%" *right-bank* )
)

(defun applicable-p(lst &aux m c num)
  (setf m (num-of-missionaries lst))
  (setf c (num-of-cannibals lst))
  (cond
   (( and
      ( check-for 'B lst)
      ( <= m (num-of-missionaries (current-bank)))
      ( <= c (num-of-cannibals (current-bank)))
      )
    T
    )
   (T
    nil
    )
   )
)
(defun check-for(n lst)
  (cond
   ((null lst)
    nil
    )
   ((equal (car lst) n)
    t
    )
   (t
    (check-for n (cdr lst))
    )
   )
  )
(defun goalp ()
  (cond
   (( and
      (equal 3 ( num-of-missionaries *right-bank*))
      (equal 3 ( num-of-cannibals *right-bank*))
      
      )
    t
    )
    (t
     nil
     )
    )
   
  )

(defun num-of-missionaries(lst)
  (cond
   ((null lst)
    0
    )
   ((equal 'm (car lst))
    (+ 1 (num-of-missionaries(cdr lst)))
    )
   (t
    (num-of-missionaries(cdr lst))
    )
   )
)

(defun feast-state-p()
  (cond
   (( or
      (and
       ( > ( num-of-cannibals *right-bank*) (num-of-missionaries *right-bank*) )
       ( > (num-of-missionaries *right-bank*) 0 ))
      (and
       ( > ( num-of-cannibals *left-bank*) (num-of-missionaries *left-bank*) )
       ( > (num-of-missionaries *left-bank*) 0 ))
      )
    t
    )
   (t
    nil
    )
   )
  )
(defun num-of-cannibals(lst)
  (cond
   ((null lst)
    0
    )
   ((equal 'c (car lst))
    (+ 1 (num-of-cannibals(cdr lst)))
    )
   (t
    (num-of-cannibals(cdr lst))
    )
   )
)

(defun make-moves ()
  (display-world)
  (cond
   ( ( goalp )
     ( write-line "good work!")
     nil
     )
   ( (feast-state-p)
     ( write-line "yummy yummy yummy, I got Good in my tummy!!")
     nil
     )
   ( t
     ( let ( ( m ( read ) ) )
       ( if ( applicable-p m )
	  ( let () (perform-move m ) ( make-moves) )
	 ( let () ( write-line "move innapplicable") nil )
	 )
       )
     )
   )
)
(defun perform-move ( move )
  ( setf *move-list* ( snoc move *move-list*))
  ( if ( equal ( current-bank ) *left-bank* )
    ( move-lr move )
    ( move-rl move)
    )
)

(defun current-bank()
  (cond
   ( (check-bank *left-bank* )
     *left-bank*
     )
   (( check-bank *right-bank* )
    *right-bank*
    )
   (t
    nil
    )
   )
)
(defun check-bank(lst)
  (cond
   ((null lst)
    nil
    )
   ((equal 'b (car lst))
    t
    )
   (t
    (check-bank (cdr lst))
    )
   )
)
(defun move-lr ( ml )
  ( if ( null ml ) (return-from move-lr))
  ( move-lr-l ( first ml ))
  ( move-lr ( rest ml ))
)

(defun move-rl ( ml )
  ( if ( null ml ) (return-from move-rl))
  ( move-rl-l ( first ml ) )
  ( move-rl ( rest ml ) )
)

(defun SNOC(x lst)
  (cond
   (( null lst)
    ( list x)
    )
   (t
    (cons (car lst)(snoc x(cdr lst)))
    )
   )
)

(defun move-lr-l(n)
  (setf *left-bank* ( remove n *left-bank* :count 1))
  (setf *right-bank* (snoc n *right-bank* ))
)

(defun move-rl-l(n)
  (setf *right-bank* ( remove n *right-bank* :count 1))
  (setf *left-bank* ( snoc n *left-bank* ))
)

