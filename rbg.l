(defconstant *limit* 25)

(defmethod rbg(&aux n)
  (nth (random 3) '(r b g))  
  )

(defmethod rbg-string(&optional (lst ()) &aux c lst2)
  (setf c (nth (random 3) '(r b g)))
  (setf lst2 (cons c lst))
  (cond
   ((equal (list-length lst2) 25)
    lst
    )
   (t
    (rbg-string lst2)
    )
   )
  )

(defmethod mutation((rbg-str list ) &aux p q)
  (setf p (random ( length rbg-str)))
  (setf q (others '(r b q) (nth p rbg-str)))
  (change rbg-str (pick q) p )
  )

(defun others()
  )

(defun change()
  )
(defun pick()
  )

