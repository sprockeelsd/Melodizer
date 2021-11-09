(in-package :mldz)

;;;====================
;;;= MELODIZER OBJECT =
;;;====================

;change the input-rhythm to be a voice object where we don't care about the pitch instead of a rhythm tree
(om::defclass! melodizer () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "The input chords on top of which the melody will be played in the form of a voice object.")
    ; maybe allow the rhythm to be a voice object as well?
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform (make-instance 'voice) :documentation "The rhythm of the melody in the form of a rhythm tree. To make the rhythm tree, express the notes with respect to a whole note (1/2 for half note, 1/4 for quarter note,... and the time signature.")
    (key :accessor key :initarg :key :initform 60 :documentation "The key of the melody is in (default : C).")
    (mode :accessor mode :initarg :mode :initform "major" :documentation "The mode the melody is in (default : major).")
    (tool-mode :accessor tool-mode :initarg :tool-mode :initform "Melody-Finder" :documentation "The mode of the tool, e.g given Melody-Finder if we want to find a melody, Accompagnement-Finder if we want to find an accompagnement, Ornement if we want to complexify the melody,...")
    (result :accessor result :initarg :result :initform (list) :documentation "A temporary list holder to store the result of the call to melody-finder, shouldn't be touched.")
    (stop-search :accessor stop-search :initarg :stop-search :initform nil :documentation "A boolean variable to tell if the user wishes to stop the search or not")
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (solutions-list :accessor solutions-list :initarg :solution-list :initform '() :documentation "The list of solutions saved by the user.")
    (output-solution :accessor output-solution :initarg :output-solution :initform nil :documentation "The selected solution")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melodizer.
        UPDATE THIS to a complete description of the tool")
)


;;; OBJECT EDITOR 
(defclass my-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melodizer)) t)
(defmethod om::get-editor-class ((self melodizer)) 'my-editor)

(defmethod om::om-draw-contents ((view my-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self my-editor) &rest args)

  ; To access the melodizer object, (object self)
  
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?, calculate the list of fundamentals, seconds,...
  
  (om::om-add-subviews 
    self

;;; pop-up menus

    ;pop-up list to select the mode of the tool (melodizer, accompagnement finder, ...)
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 30 50) 
      (om::om-make-point 200 20) 
      "Tool Mode selection"
      :range '("Melody-Finder" "Accompagnement-Finder" "Ornement")
      :di-action #'(lambda (m)
        ;(print (nth (om-get-selected-item-index m) (om-get-item-list m))); display the selected option
        (setf (tool-mode (om::object self)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))) ; set the tool-mode according to the choice of the user
      )
    )

    ;pop-up list to select the key of the melody
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 30 90) 
      (om::om-make-point 200 20) 
      "Key selection"
      :range '("C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :value (note-value-to-name (key (om::object self)))
      :di-action #'(lambda (m)
        (setf (key (om::object self)) (name-to-note-value (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))) ; set the key according to the choice of the user
      )
    )

    ;pop-up list to select the mode of the melody
    (om::om-make-dialog-item 
      'om::pop-up-menu 
      (om::om-make-point 30 130) 
      (om::om-make-point 200 20) 
      "Mode selection"
      :range '("major" "minor"); add pentatonic, chromatic, ... ?
      :value (mode (om::object self))
      :di-action #'(lambda (m)
        (setf (mode (om::object self)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))) ; set the mode according to the choice of the user
      )
    )

    ;pop-up list to select the desired solution
    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 350 130)
      (om::om-make-point 320 20)
      "Solution selection"
      :range (solutions-list (om::object self))
      ;:value todo
      :di-action #'(lambda (m); change the representation so the name of the object is not displayed but something like "solution 2"
        (setf (output-solution (om::object self)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

;;; text boxes (change that because for now they can be edited!)

    ;; ;text for the slider
    ;; (om::om-make-dialog-item
    ;;   'om::text-box
    ;;   (om::om-make-point 660 50) 
    ;;   (om::om-make-point 180 20) 
    ;;   "Variety of the solutions" 
    ;;   :font om::*om-default-font1* 
    ;; )

;;; buttons

    ; button to start or restart the search
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 350 50) ; position (horizontal, vertical)
      (om::om-make-point 100 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b) 
                    ;(dolist (e (chords (input-chords (object self))))
                    ;  (print (lmidic e))
                    ;)
                    ; reset the solutions
                    (setf (solutions-list (om::object self)) '())
                    (setf (solution (om::object self)) nil)
                    ; reset the boolean
                    (setf (stop-search (om::object self)) nil)
                    (cond
                      ((string-equal (tool-mode (om::object self)) "Melody-Finder"); melody finder mode, where the user gives as input a voice with chords
                        (let init; list to take the result of the call to melody-finder
                          (setq init (melody-finder (input-chords (om::object self)) (om::tree (input-rhythm (om::object self))) (key (om::object self)) (mode (om::object self)))); get the search engine and the first solution of the CSP
                          ; update the fields of the object to their new value
                          (setf (result (om::object self)) init); store the result of the call to melody finder
                        )
                      )
                      ((string-equal (tool-mode (om::object self)) "Accompagnement-Finder"); not supported yet
                        (print "This mode is not supported yet")
                      )
                      ((string-equal (tool-mode (om::object self)) "Ornement"); not supported yet
                        (print "This mode is not supported yet")
                      )
                    )
        )
    )

    ; button to find the next solution
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 460 50) ; position
      (om::om-make-point 100 20) ; size
      "Next"
      :di-action #'(lambda (b)
                    (print "Searching for the next solution")
                    ;reset the boolean
                    (setf (stop-search (om::object self)) nil)
                    ;get the next solution  
                    (setf (solution (om::object self)) (search-next-melody-finder (result (om::object self)) (om::tree (input-rhythm (om::object self))) (om::object self)))
      )
    )

    ; button to stop the search if the user wishes to
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 570 50)
      (om::om-make-point 100 20)
      "Stop"
      :di-action #'(lambda (b)
        (setf (stop-search (om::object self)) t) ; set the boolean to true
      )
    )

    ; button to open the voice object of the solution, for now print the object
    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 350 90); position
      (om::om-make-point 160 20); size
      "See solution"
      :di-action #'(lambda (b)
                      (print (solution (om::object self)))
                      ; open the voice object
      )  
    )

    ;button to add the solution to the list of solutions (if we find it interesting and want to keep it)
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 510 90) ; position
      (om::om-make-point 160 20) ; size
      "Keep Solution"
      :di-action #'(lambda (b); seems to work now
                    (if (typep (solution (om::object self)) 'null)
                      (error "There is no solution to keep.")
                    )
                    (print "before")
                    (print (solutions-list (om::object self)))
                    ;add the element to the list
                    (if (typep (solutions-list (om::object self)) 'null); if it's the first solution
                      (setf (solutions-list (om::object self)) (list (solution (om::object self)))); initialize the list
                      (nconc (solutions-list (om::object self)) (list (solution (om::object self)))); add it to the end
                    )
                    (print "after")
                    (print (solutions-list (om::object self)))       
      )
    )

;;; check-boxes

    ;checkbox for constraint 1
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 30 230) ; position
      (om::om-make-point 20 20) ; size
      "Constraint 1"
      :di-action #'(lambda (c)
                    (print "Constraint 1 checked")
                  )
    )

    ;checkbox for constraint 2
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 30 270) ; position
      (om::om-make-point 20 20) ; size
      "Constraint 2"
      :di-action #'(lambda (c)
                    (print "Constraint 2 checked")
                  )
    )

    ;checkbox for constraint 3
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 30 310) ; position
      (om::om-make-point 20 20) ; size
      "Constraint 3"
      :di-action #'(lambda (c)
                    (print "Constraint 3 checked")
                  )
    )

    ;checkbox for constraint 4
    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 150 230) ; position
      (om::om-make-point 20 20) ; size
      "Constraint 4"
      :di-action #'(lambda (c)
                    (print "Constraint 4 checked")
                  )
    )

;;; sliders

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 30 170) ; position
      (om::om-make-point 200 20) ; size
      "Slider"
      :range '(1 100)
      :increment 1
      :value 1
      :di-action #'(lambda (s)
                    (print (om::om-slider-value s))
                  )
    )
  ); end of add subviews
  ; return the editor:
  self
)



