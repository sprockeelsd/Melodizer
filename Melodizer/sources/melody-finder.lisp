;;;===============================
;;; Object to find a desired solution among the possible solutions
;;;===============================
(in-package :om)

;change the input from a voice to 2 inputs : the tree and the pitches?
(om::defclass! melody-finder () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "the input chords on top of which the melody will be played in the form of a voice object")
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform nil :documentation "rhythm of the melody in the form of a rhythm tree. To make the rhythm tree,
    express the notes with respect to a whole note (1/2 for half note, 1/4 for quarter note,... and the time signature"); maybe allow the rhythm to be a voice object as well?
    (key :accessor key :initarg :key :initform 60 :documentation "The key of the melody is in (default : C") ; maybe change that to the name of the note (C#, E,...)
    (mode :accessor mode :initarg :mode :initform "major" :documentation "the mode the melody is in (default : major) ")
    (tool-mode :accessor tool-mode :initarg :tool-mode :initform "Melody-Finder" :documentation "The mode of the tool, 
    e.g given Melody-Finder if we want to find a melody, Accompagnement-Finder if we want to find an accompagnement,...")
    (search-engine :accessor search-engine :initarg :search-engine :initform nil :documentation "search engine for the CSP, shouldn't be touched")
    (cspsol :accessor cspsol :initarg :cspsol :initform nil :documentation "solution of the CSP, shouldn't be touched")
    (solution :accessor solution :initarg :solution :initform nil :documentation "solution of the CSP in the form of a voice object")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melody-finder.
        UPDATE THIS to a complete description of the tool")
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

  ; To access the melody-finder object, (object self)
  
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?, calculate the list of fundamentals, seconds,...
  
  (om::om-add-subviews 
    self
    ;pop-up list to select the mode of the tool (melody-finder, accompagnement finder, ...)
    (om-make-dialog-item 
      'om::pop-up-menu 
      (om-make-point 30 20 ) 
      (om-make-point 200 20) 
      "Mode selection"
      :range '("Melody-Finder" "Accompagnement-Finder" "Ornement")
      :di-action #'(lambda (m)
        ;(print (nth (om-get-selected-item-index m) (om-get-item-list m))); display the selected option
        (setf (tool-mode (object self)) (nth (om-get-selected-item-index m) (om-get-item-list m))) ; set the tool-mode according to the choice of the user
      )
    )

    ; button to start or restart the search, not sure if I will keep it here
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 10 500) ; position
      (om::om-make-point 80 20) ; size
      "Start"
      :di-action #'(lambda (b) 
                    ;(dolist (e (chords (input-chords (object self))))
                    ;  (print (lmidic e))
                    ;)
                    (let init 
                      (setq init (voicemelodizer (input-chords (object self)) (input-rhythm (object self)) (key (object self)) (mode (object self)))); get the search engine and the first solution of the CSP
                      ; update the fields of the object to their new value
                      (setf (search-engine (object self)) (first init))
                      (setf (cspsol (object self)) (second init))
                      (print "csp constructed")
                    )
                  )
      ;:di-action #'(lambda (b)
      ;             (print "Restarting the search with the new constraints"))
    )
    ; button to find the next solution
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 100 500) ; position
      (om::om-make-point 80 20) ; size
      "Next"
      :di-action #'(lambda (b)
                    (print "Searching for the next solution")
                    (let sol 
                      (setf (solution (object self)) (search-next-voice (list (search-engine (object self)) (cspsol (object self))) (input-rhythm (object self))))
                    )
                  )
    )
    ;example of a checkbox
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 200 500) ; position
      (om::om-make-point 20 20) ; size
      "Test"
      :di-action #'(lambda (c)
                    (print "checked")
                  )
    )
    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 10 550) ; position
      (om::om-make-point 80 20) ; size
      "Slider"
      :direction "horizontal"
      :range '(1 100)
      :increment 1
      :value 1
      :di-action #'(lambda (s)
                    (print (om-slider-value b))
                  )
    )
  )
  ; return the editor:
  self
)



