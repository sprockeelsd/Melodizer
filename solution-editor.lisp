;;;===============================
;;; Minimal example of an object and its editor in OM 
;;;===============================

(in-package :cl-user)

(om::defclass! my-object () 
   ((slot1 :accessor slot1 :initarg :slot1 :initform nil :documentation "slot 1")
    (slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2"))
   (:icon 1)
   (:doc "This class implements my object")
   )


    
;;; OBJECT EDITOR 
(defclass my-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self my-object)) t)
(defmethod om::get-editor-class ((self my-object)) 'my-editor)

(defmethod om::om-draw-contents ((view my-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
     view
     ;;; DRAW SOMETHING ?
     )))


(defmethod initialize-instance ((self my-editor) &rest args)
  
  ;;; do what needs to be done by default
  (call-next-method)
  
  (om::om-add-subviews 
   self
   
   (om::om-make-dialog-item 
    'om::om-button
    (om::om-make-point 10 10) ; position
    (om::om-make-point 80 20) ; size
    "Click me!"
    :di-action #'(lambda (b)
                   (print "Do something!"))
    )
   )
  
  ; return the editor:
  self)



    







