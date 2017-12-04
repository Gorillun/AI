(defclass accumulator()
  (
   (value :accessor accumulator-value :initform 0)
   )
  )


(defmethod inc ((a accumulator) i)
  (setf (accumulator-value a) (+ i (accumulator-value a)))
  )

(defmethod dec((a accumulator) d)
  (setf (accumulator-value a) (- (accumulator-value a) d))
  )

(defmethod reset ((a accumulator))
  (setf (accumulator-value a) 0 )
  )
