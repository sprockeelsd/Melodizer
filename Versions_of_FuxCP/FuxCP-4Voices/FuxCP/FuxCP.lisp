; if problem with rpath, use 
; install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode libgecode.dylib

;; (in-package :om)

(defvar *fuxcp-sources-dir* nil)
(setf *fuxcp-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))                                                            

(defvar *libgecode* nil)

; trying to load mac library and then linux library if mac doesn't work
(handler-case
    (progn
        (setf *libgecode* (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "libgecode.dylib")) 
        (if (equal (cffi:load-foreign-library  *libgecode*) nil)
            (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    (t (c)
       (progn
           (setf *libgecode* (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "libgecode.so")) 
           (if (equal (cffi:load-foreign-library  *libgecode*) nil)
                (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    )
)
       

(compile&load (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "package" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "problem-wrapper" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *fuxcp-sources-dir*) :name "interface" :type "lisp"))


(fill-library '(
    ("Solver" nil (fuxcp::cp-params) nil)
))

(print "FuxCP Loaded")