; if problem with rpath, use 
; install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode libgecode.dylib

(defvar *gecode-sources-dir* nil)
(setf *gecode-sources-dir* (make-pathname :directory (append (pathname-directory *load-pathname*) '("sources"))))                                                            

(defvar *libgecode* nil)

; trying to load mac library and then linux library if mac doesn't work
(handler-case
    (progn
        (setf *libgecode* (make-pathname :directory (pathname-directory *gecode-sources-dir*) :name "libgecode.dylib")) 
        (if (equal (cffi:load-foreign-library  *libgecode*) nil)
            (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    (t (c)
       (progn
           (setf *libgecode* (make-pathname :directory (pathname-directory *gecode-sources-dir*) :name "libgecode.so")) 
           (if (equal (cffi:load-foreign-library  *libgecode*) nil)
                (print "There is a problem loading the Framework. Please double check that Gecode is correctly installed and you are using the appropriate version of GiL for your Operative System")))
    )
)
       


(compile&load (make-pathname :directory (pathname-directory *gecode-sources-dir*) :name "gecode-wrapper" :type "lisp"))
(compile&load (make-pathname :directory (pathname-directory *gecode-sources-dir*) :name "gecode-wrapper-ui" :type "lisp"))
