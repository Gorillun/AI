;;
(defconstant *ranks* '(2 3 4 5 6 7 8 9 J Q K A))
(defconstant *suits* '(hearts spades clubs diamonds))

(defun new-suit(suit)
  (mapcar #'list *ranks* (make-list 13 :initial-element suit))
)

(defun new-deck()
  (mapcan #'new-suit *suits*)
)
