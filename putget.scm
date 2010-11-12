;; putget.scm : explore reading and writing from files, and hashtables.

;; store 4 names in a file

(define a '("hannah","sarah","leah","jimmy"))
(define oport (open-output-file "test.dat"))
(write a oport)
(close-output-port oport)

;; define some shortcuts file-put and file-get, to make that easier.

(define file-put (lambda (x path)
	       (define oport (open-output-file path))
	       (write a oport)
	       (close-output-port oport)
	       )
   )

(define file-get (lambda (path)
	       (let (
		     (iport (open-input-file path))
		     )
		  (read iport a)
		  (close-input-port iport)
		  a
		  )))



;; retreive those names

(define iport (open-input-file "test.dat"))
(define b (read iport))
(close-input-port iport)

;; better yet, name what we get, and use our new function file-get
(define a (file-get "test.dat"))


;; make a hash-table for them

(define h (make-hashtable))

(hashtable? h)

(hashtable-size h) ;; 0 when nothing in it.

(hashtable-contains? h "hello")  ;; #f at this point

(hashtable-put! h "hello" 5)
(hashtable-put! h "world" 6)
(hashtable-put! h "whatacute" 7)
(hashtable-put! h "puppy" 50)

(hashtable-contains? h "hello") ;; #t now

(hashtable-get h "hello") ;; returns 5

(hashtable-get h "hello-nonexistant") ;; returns #f

(hashtable-get h "puppy") ;; returns 50

(hashtable-remove! h "nowhere-man") ;; return #f, since not present.

(hashtable-remove! h "whatacute")  ;; returns #t, and now "whatacute" has been deleted.


(hashtable-put! h "puppy" 47)  ;; #replace mapping of (puppy . 50) with (puppy . 47 ); returns old binding.

(hashtable-get h "puppy") ;; returns 47 now.

(hashtable-for-each h (lambda (key val) ((display key) ) ))


(define pr (lambda (key val) ((display key) (display val))))

(define show (lambda (myhash) (hashtable-for-each myhash (lambda (key obj) (print (cons key obj))))))

(show h)

; output of (show h):
;
; (world . 6)
; (hello . 5)
; (puppy . 47)
; #f

(hashtable-size h) ;;  now 3




