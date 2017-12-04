;;;; file: mc.l 
;;;; one solution to the missionaries - cannibals problem


;; gloabal variables: *left-bank*, *right-bank*, *move-list*



( DEFUN mc ()
	( establish-world )
	( init-move-list )
	( make-moves )
)



( DEFUN establish-world () 
	( setf *left-bank* '( M M M C C C B ) )
	( setf *right-bank* '( ) )
)


( DEFUN init-move-list ()
	( setf *move-list* '( ) )
)


( DEFUN display-world ()
	( format t "left-bank: ~A~%" *left-bank* )
	( format t "right-bank: ~A~%" *right-bank* )
)


( DEFUN make-moves ()
	( display-world )
	( COND
		( ( goalp ) ( write-line "Good Work!" ) NIL )
		
		( ( feast-state-p ) ( write-line "yummy yummy yummy, I got good in my tummy!!" ) NIL )

		( T 
		   ( let ( ( m ( read ) ) )
			( if ( applicable-p m )
			   ( let () ( perform-move m ) ( make-moves ) )
			   ( let () ( write-line "move inapplicable" ) NIL )
			)
		   )
		)	
	)
)


( DEFUN goalp ( )
	( COND 
		( ( AND
			( = 3 ( missionary-count *right-bank* ) ) 
			( = 3 ( cannibal-count *right-bank* ) ) 
			( equal (current-bank) *right-bank* )  
		   ) T
		)
		( T  NIL )
	)
)


( DEFUN missionary-count ( lst )
	( COND
		( ( NULL lst ) 0 ) 		

		( ( eq ( CAR lst) 'm )  ( + 1 ( missionary-count ( CDR lst ) ) ) )   
		
		( T ( missionary-count ( CDR lst ) ) ) 
	)
)


( DEFUN cannibal-count ( lst )
	( COND
		( ( NULL lst ) 0 ) 		

		( ( eq ( CAR lst) 'c ) ( + 1 ( cannibal-count ( CDR lst ) ) ) )   
		
		( T ( cannibal-count ( CDR lst ) ) ) 
	)
)


( DEFUN current-bank () 
	( COND 
		( ( contains 'B *left-bank* ) *left-bank* )

		( ( contains 'B *right-bank* ) *right-bank* )
		
		( T NIL )
	)
)


( DEFUN feast-state-p ( ) 
	( COND 
		( 
		   ( OR 
			( AND ( > ( cannibal-count *right-bank* ) ( missionary-count *right-bank* ) ) (> (missionary-count *right-bank*) 0 ) )
			( AND ( > ( cannibal-count *left-bank* ) ( missionary-count *left-bank* ) ) (> (missionary-count *left-bank*) 0 ) )
		   ) T
		)
		( T NIL )		
	)
)


( DEFUN applicable-p ( lst &aux m-to-move m-available c-to-move c-available current-lst)
	( setf m-to-move ( missionary-count lst ) )
	( setf c-to-move ( cannibal-count lst ) ) 
	( setf current-list ( current-bank ) )
	( setf m-available ( missionary-count current-list ) )
	( setf c-available ( cannibal-count current-list ) ) 
	( COND 
		( ( AND 
			( contains 'B lst ) 
			( > ( length lst ) 1 )
			( < ( length lst ) 4 )
			( <= m-to-move m-available )
			( <= c-to-move c-available )
		  ) T 
		)
		( T NIL )
	)      
)




( DEFUN snoc ( x lst )
	( COND
		( ( NULL lst ) ( LIST x ) )
		
		( ( singleton-p lst ) ( LIST ( CAR lst ) x ) )

		( (CONS ( CAR lst ) ( snoc x (CDR lst) ) ) )
	)
)


( DEFUN contains ( x lst )
	( COND 
		( ( NULL lst ) NIL )
		
		( ( equal x ( CAR lst ) ) T )

		( T ( contains x ( CDR lst ) ) )
	)
)



( DEFUN perform-move ( move )
	( setf *move-list* ( snoc move *move-list* ) )
	( if ( equal ( current-bank ) *left-bank* ) 
		( move-lr move )
		( move-rl move )
	)
)


( DEFUN move-lr ( ml )
	( if ( null ml ) ( return-from move-lr ) )
	( move-lr-l ( first ml ) )
	( move-lr ( rest ml ) )
)


( DEFUN move-rl ( ml )
	( if ( null ml ) ( return-from move-rl ) ) 
	( move-rl-l ( first ml ) )
	( move-rl ( rest ml ) )
)


( DEFUN move-lr-l ( x ) 
	( setf *left-bank* ( remove x *left-bank* :count 1 ) )
	( setf *right-bank* ( snoc x *right-bank* ) )
)

( DEFUN move-rl-l ( x ) 
	( setf *right-bank* ( remove x *right-bank* :count 1 ) )
	( setf *left-bank* ( snoc x *left-bank* ) )
)


( DEFUN singleton-p ( lst )
	( COND 
		( ( NULL lst ) NIL )
		  
		( ( NULL ( CDR lst ) ) T )
	
		(  T NIL )
	)
)
	


