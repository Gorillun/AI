(defclass die()
  (
   (number :accessor number-of-faces :initarg :number :initform 6)
   (top :accessor top-face :initarg :number :initform 1)
   (history :accessor face-history :initform ())
   )
  )

(defmethod roll-die((d die))
  (setf (top-face d) (+ 1 ( random (number-of-faces d))))
  (setf (face-history d) (cons (top-face d) (face-history d)))
  )

(defmethod roll-for-pair((d1 die) (d2 die))
  (roll-die d1)
  (roll-die d2)
  (cond
   ((equal (top-face d1) (top-face d2))
    nil
    )
   (t
    (roll-for-pair d1 d2)
    )
   )
  )

(defmethod display((d die))
  (format nil "[~A,~A,~A]" (top-face d) (number-of-faces d) (face-history d))
  )

(defmethod roll-for-sum((d1 die)(d2 die) n)
  (roll-die d1)
  (roll-die d2)
  (cond
   ((equal n (+ (top-face d1) (top-face d2)))
    nil
    )
   (t
    (roll-for-sum d1 d2 n)
    )
   )
  )


