;;;===============================
;;; Object to find a desired solution amongst the possible solutions
;;;===============================

(in-package :om)

(om::defclass! melody-finder () 
  ((input-chords :accessor input-chords :initarg :init-chords :initform (make-instance 'voice) :documentation "the input chords on top of which the melody will be played")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melody-finder")
)


    
;;; OBJECT EDITOR 
(defclass my-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melody-finder)) t)
(defmethod om::get-editor-class ((self melody-finder)) 'my-editor)

(defmethod om::om-draw-contents ((view my-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)


(defmethod initialize-instance ((self my-editor) &rest args)
  
  ;;; do what needs to be done by default
  (call-next-method)
  
  (om::om-add-subviews 
    self
    ; button to find the next solution
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 10 10) ; position
      (om::om-make-point 80 20) ; size
      "Next"
      :di-action #'(lambda (b)
                   (print "Finding the next solution"))
    )
    ; button to restart the search
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 100 10) ; position
      (om::om-make-point 80 20) ; size
      "Restart"
      :di-action #'(lambda (b)
                   (print "Restarting the search with the new constraints"))
    )
  )
  ; return the editor:
  self
)



    







