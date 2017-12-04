;;
(defun Singleton-P (LST)
  (cond
    ((Null LST)
     NIL
     )
    ((Null (CDR LST))
     T
     )
    ( T
      NIL
      )
    )
)

(defun RAC (lst)
  (cond
   (( Singleton-p LST)
    (car LST)
    )
   ( T
     (RAC (CDR LST))
     )
   )
)

(defun RDC (LST)
  (cond 
   (( Singleton-P LST)
    ()
    )
   ( t
     (cons (car lst)(rdc(cdr lst)))
     )
   )
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

(defun palindrome-p(lst)
  (cond
   (( null lst)
    t)
   ((singleton-p lst)
    t
   )
   ((equal (rac lst)(car lst))
    (palindrome-p (rdc (cdr lst)))
    )
   )
)
   
(defun select(x lst)
  (cond
   (( equal x 0)
    (car lst)
    )
   ((> x 0)
   (select (- x 1) (cdr lst))
   )
   )
)

(defun pick(lst &aux r)
  (setq r (random (list-length lst)))
  (select r lst ) 
)

(defun sum(lst &aux result)
  (cond
   ((null lst)
    0
    )
   ((singleton-p lst)
    (car lst)
    )
   ((singleton-p (rest lst))
    (setq result (+ (car lst) (car (rest lst))))
    result
    )
   (t
   (setq result ( + (car lst) (car (rest lst))))
   (sum (cons result (rest (rest lst))))
   
   )
   )
)
(defun product(lst &aux result)
  (cond
   ((null lst)
    0
    )
   ((singleton-p lst)
    (car lst)
    )
   ((singleton-p (rest lst))
    (setq result (* (car lst) (car (rest lst))))
    result
    )
   (t
   (setq result ( * (car lst) (car (rest lst))))
   (product (cons result (rest (rest lst))))

   )
   )
)

(defun iota(lst &aux r)
  (cond
   ((atom lst)
    (iota (cons lst ())
    )
    )
   (t
    (setq r (- (car lst) 1))
    (cond 
     ((<= r 0)
      lst
      )
     (t
      (iota (cons r lst))
      )
     )
   )
   )
)

(defun duplicate(n lo &aux r)
  (cond
   ((atom lo)
    (setq r (- n 1))
    (duplicate r (cons lo ()))
   )
   (t
    (cond
     ((equal n 0)
      lo
      )
     (t
      (setq r (- n 1))
      (duplicate r (cons (car lo) lo))
     )
    )
   )
   )
)

(defun factorial(n)
  (product (iota n) )
)

(defun power(n e)
  (product (duplicate n e)) 
)
  

(defun filter-in(p lst &aux filtered)
  (cond
   ((string-equal p "even")
    (setf filtered ())
     (if(equal (rem (nth 0 lst) 2) 0)
      (setf filtered (cons (nth 0 lst) filtered))
      )
     (if(equal (rem (nth 1 lst) 2) 0)
      (setf filtered (snoc (nth 1 lst) filtered))
      )
     (if(equal (rem (nth 2 lst) 2) 0)
      (setf filtered (snoc (nth 2 lst) filtered))
      )
     (if(equal (rem (nth 3 lst) 2) 0)
      (setf filtered (snoc (nth 3 lst) filtered))
      )
    )
  (t
   (setf filtered ())
     (if(> (rem (nth 0 lst) 2) 0 )
      (setf filtered (cons (nth 0 lst) filtered))
      )
     (if(> (rem (nth 1 lst) 2) 0 )
      (setf filtered (snoc (nth 1 lst) filtered))
      )
     (if(> (rem (nth 2 lst) 2) 0 )
      (setf filtered (snoc (nth 2 lst) filtered))
      )
     (if(> (rem (nth 3 lst) 2) 0 )
      (setf filtered (snoc (nth 3 lst) filtered))
      )
     )
   )
  filtered
  )

(defun filter-out(p lst &aux filtered)
  (cond
   ((string-equal p "odd")
    (setf filtered ())
     (if(equal (rem (nth 0 lst) 2) 0)
      (setf filtered (cons (nth 0 lst) filtered))
      )
     (if(equal (rem (nth 1 lst) 2) 0)
      (setf filtered (snoc (nth 1 lst) filtered))
      )
     (if(equal (rem (nth 2 lst) 2) 0)
      (setf filtered (snoc (nth 2 lst) filtered))
      )
     (if(equal (rem (nth 3 lst) 2) 0)
      (setf filtered (snoc (nth 3 lst) filtered))
      )
    )
  (t
   (setf filtered ())
     (if(> (rem (nth 0 lst) 2) 0 )
      (setf filtered (cons (nth 0 lst) filtered))
      )
     (if(> (rem (nth 1 lst) 2) 0 )
      (setf filtered (snoc (nth 1 lst) filtered))
      )
     (if(> (rem (nth 2 lst) 2) 0 )
      (setf filtered (snoc (nth 2 lst) filtered))
      )
     (if(> (rem (nth 3 lst) 2) 0 )
      (setf filtered (snoc (nth 3 lst) filtered))
      )
     )
   )
  filtered
  )

(defun take-from(p lst &aux taken)
  (setf taken ())
  (if(not (string-equal p (nth 0 lst)) )
      (setf taken (cons (nth 0 lst) taken))
      ) 
  (if(not (string-equal p (nth 1 lst)) )
      (setf taken (cons (nth 1 lst) taken))
      )
  (if(not (string-equal p (nth 2 lst)) )
      (setf taken (cons (nth 2 lst) taken))
      )
  (if(not (string-equal p (nth 3 lst)) )
      (setf taken (cons (nth 3 lst) taken))
      )
  taken
)

(defun random-permutation(lst &aux r hi temp permutated)
  (setq hi (list-length lst) + 1)
  (setq r (random hi))
  (setf permutated ())
  (setf temp lst)
  (cond
   ((equal r 1 )
    lst
   )
   (t
   ; (setf permutated (cons (nth r lst) permutated))
    (setf permutated (cons (nth r temp) permutated))
    (setf temp (remove (car permutated) temp))
    (random-permutation ( cons (car permutated) temp))
    )
   )
)
