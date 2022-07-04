(in-package :om)

(defvar *melodizer-sources-dir* nil)
(setf *melodizer-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))


(mapc 'compile&load (list
                     (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-utils" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csp" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csts" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer" :type "lisp")
                     (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "dummy-problem" :type "lisp")
                     ))

;; (defvar *melodizer-pkg* (omng-make-new-package 'melodizer))

;; (AddLispFun2Pack '(
;;     melodizer
;;     search-next
;;     all-different-notes
;;     )
;; *melodizer-pkg*)

;; ;(AddClass2Pack my-object *melodizer-pkg*)

;; (AddPackage2Pack *melodizer-pkg* *om-package-tree*)


;; remplir à la fin
(fill-library '(("ALL" nil 
                 (mldz::melodizer) 
                 (mldz::melody-finder
                 mldz::get-events-from-rtree
                 ; mldz::another-function
                                 ) nil)




                     ("UTILS" Nil Nil (mldz::get-voice
                                       mldz::to-midicent
                                       ) nil)


))


(print "Melodizer Loaded")