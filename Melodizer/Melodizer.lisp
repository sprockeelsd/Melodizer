(require-library "GiL")
(defvar *melodizer-sources-dir* nil)
(setf *melodizer-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))

(compile&load (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-utils" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csp" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "melodizer-csts" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *melodizer-sources-dir*) :name "my-object" :type "lisp"))

(defvar *melodizer-pkg* (omng-make-new-package 'melodizer))

(AddLispFun2Pack '(
    melodizer
    search-next
    all-different-notes
    )
*melodizer-pkg*)

;(AddClass2Pack my-object *melodizer-pkg*)

(AddPackage2Pack *melodizer-pkg* *om-package-tree*)

(print "Melodizer Loaded")