;;
(defconstant *colors* '( red blue black white silver))
(defconstant *styles* '( Soft-Tail Hard-Tail Wide-Glide Electra-Glide Sportster))

(defun style(style)
  (mapcar #'list *colors* (make-list 5 :initial-element style))
  )

(defun color()
  (mapcan #'style *styles*)
)
