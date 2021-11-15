(in-package :mldz)

;;;====================
;;;= MELODIZER OBJECT =
;;;====================

(om::defclass! melodizer () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "The input chords on top of which the melody will be played in the form of a voice object.")
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform (make-instance 'voice) :documentation "The rhythm of the melody in the form of a voice object. ")
    (key :accessor key :initarg :key :initform 60 :documentation "The key the melody is in (default : C).")
    (mode :accessor mode :initarg :mode :initform "major" :documentation "The mode the melody is in (default : major).")
    (tool-mode :accessor tool-mode :initarg :tool-mode :initform "Melody-Finder" :documentation "The mode of the tool, e.g given Melody-Finder if we want to find a melody, Accompagnement-Finder if we want to find an accompagnement, Ornement if we want to complexify the melody,...")
    (variety :accessor variety :initarg :variety :initform 1 :documentation "The minimal variety we want for the solution, expressed as a number of notes.")
    (result :accessor result :initarg :result :initform (list) :documentation "A temporary list holder to store the result of the call to melody-finder, shouldn't be touched.")
    (stop-search :accessor stop-search :initarg :stop-search :initform nil :documentation "A boolean variable to tell if the user wishes to stop the search or not.")
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (solutions-list :accessor solutions-list :initarg :solution-list :initform '() :documentation "The list of all the solutions saved by the user.")
    (output-solution :accessor output-solution :initarg :output-solution :initform nil :documentation "The selected solution.")
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melodizer.
        UPDATE THIS to a complete description of the tool")
)


; the editor for the object
(defclass melodizer-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melodizer)) t)
(defmethod om::get-editor-class ((self melodizer)) 'melodizer-editor)

(defmethod om::om-draw-contents ((view melodizer-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

; To access the melodizer object, (object self)

(defmethod initialize-instance ((self melodizer-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?, calculate the list of fundamentals, seconds,...
  (make-my-interface self)
)

; function to update the list of solutions in the pop-up menu without having to close and re-open the window
(defun update-solutions-list (self my-panel)
  (om::om-add-subviews my-panel
    (om::om-make-dialog-item 
      'om::om-pop-up-dialog-item 
      (om::om-make-point 5 130) 
      (om::om-make-point 320 20) 
      "list of solutions"
      :range (loop for item in (make-data-sol (solutions-list (om::object self))) collect (car item)); make a list with the number of the solution to make it clearer for the user
      ;:value (mode (object self)); change so it goes to the newest added solution? 
      :di-action #'(lambda (m)
                    (setf (output-solution (om::object self)) (nth (om::om-get-selected-item-index m) (solutions-list (om::object self)))); set the output solution to the currently selected solution
                    (let ((indx (om::om-get-selected-item-index m)))
                      (om::openeditorframe ; open the editor of the selected solution
                        (om::omNG-make-new-instance 
                          (nth indx (solutions-list (om::object self))); the selected voice object
                          (format nil "melody ~D" (1+ indx)); name of the window
                        )
                      )
                    )
      )
    )
  )
)

; function to create the tool's interface
(defmethod make-my-interface ((self melodizer-editor))
  
  ;;;;;;;;;;;;;;;;;
  ;;; main view ;;;
  ;;;;;;;;;;;;;;;;;

  ; background colour
  (om::om-set-bg-color self om::*om-light-gray-color*) ;;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)

  ; title
  (om::om-add-subviews
    self
    (om::om-make-dialog-item 
      'om::om-static-text 
      (om::om-make-point 350 2) 
      (om::om-make-point 120 20) 
      "Melodizer"
      :font om::*om-default-font3b*
    )
  )

  ;;;;;;;;;;;;;;;;;
  ;;; sub views ;;;
  ;;;;;;;;;;;;;;;;;

  (let* 
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ; The coordinates here are coordinates in the main view
      (input-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with input
        :size (om::om-make-point 400 300)
        :position (om::om-make-point 5 30)
        :bg-color om::*azulito*) 
      )
      (search-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with the search for solutions
        :size (om::om-make-point 400 300)
        :position (om::om-make-point 410 30)
        :bg-color om::*azulito*)
      )
      (constraints-panel (om::om-make-view 'om::om-view ; part of the display for everything that has to do with adding new constraints to the problem
        :size (om::om-make-point 805 300)
        :position (om::om-make-point 5 335)
        :bg-color om::*azulito*)
      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; creating the input panel ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ; coordinates here are local to input-panel
      (elements-input-panel
        (om::om-add-subviews
          input-panel

          ; title
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 140 2) 
            (om::om-make-point 120 20) 
            "Parameters"
            :font om::*om-default-font1b*
          )

          ;pop-up list to select the mode of the tool (melodizer, accompagnement finder, ...)
          (om::om-make-dialog-item 
            'om::pop-up-menu 
            (om::om-make-point 5 25) 
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
            (om::om-make-point 5 60) 
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
            (om::om-make-point 5 85) 
            (om::om-make-point 200 20) 
            "Mode selection"
            :range '("major" "minor"); add pentatonic, chromatic, ... ?
            :value (mode (om::object self))
            :di-action #'(lambda (m)
              (setf (mode (om::object self)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m))) ; set the mode according to the choice of the user
            )
          )

          ;button to edit the input chords
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 200 60)
            (om::om-make-point 200 20)
            "Edit input chords"
            :di-action #'(lambda (b)
              (om::openeditorframe ; open a voice window displaying the input chords
                (om::omNG-make-new-instance 
                  (input-chords (om::object self))
                  "input chords" ; name of the window
                )
              )
            )
          )

          ;button to edit the input rhythm
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 200 85)
            (om::om-make-point 200 20)
            "Edit melody rhythm"
            :di-action #'(lambda (b)
              (om::openeditorframe ; open a voice window displaying the input rhythm
                (om::omNG-make-new-instance 
                  (input-rhythm (om::object self))
                  "input rhythm" ; name of the window
                )
              )
            )
          )

          ;text for the slider
          (om::om-make-dialog-item
            'om::om-static-text 
            (om::om-make-point 10 115) 
            (om::om-make-point 200 20) 
            "Variety of the solutions"
            :font om::*om-default-font1*
          )

          ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
          (om::om-make-dialog-item
            'om::om-slider
            (om::om-make-point 5 135) ; position
            (om::om-make-point 200 20) ; size
            "Slider"
            :range '(1 100)
            :increment 1
            :value (* 100 (/ (variety (om::object self)) (n-pulses (input-rhythm (om::object self)))))
            :di-action #'(lambda (s)
                          ; set the value of variety to  the value of the pointer times n-values / 100, rounded down
                          (setf 
                            (variety (om::object self))
                            (floor ;division rounding down
                              (*
                                (om::om-slider-value s)
                                (om::n-pulses (input-rhythm (om::object self)))
                              )
                              100
                            )
                          )
                          ;(print (variety (om::object self)))
            )
          )

          ; button to reset the input 
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 200 25)
            (om::om-make-point 200 20)
            "Reset input"
            :di-action #'(lambda (b)
              (setf (input-chords (om::object self)) (make-instance 'voice))
              (setf (input-rhythm (om::object self)) (make-instance 'voice))
              (setf (key (om::object self)) 60)
              (setf (mode (om::object self)) "major")
              (setf (tool-mode (om::object self)) "Melody-Finder") 
              (setf (variety (om::object self)) 0)
            )
          )
        )
      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; creating the search panel ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ; coordinates here are local to search-panel
      (elements-search-panel
        (om::om-add-subviews
          search-panel

          ; title
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 190 2) 
            (om::om-make-point 120 20) 
            "Search"
            :font om::*om-default-font1b*
          )

          ;pop-up list to select the desired solution
          ;this is only for the start, as a new pop-up menu is created with every new solution
          ;/!\ if you move this, you also have to move the new ones that are generating every time the list is modified! see update-solutions-list function
          (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 5 130)
            (om::om-make-point 320 20)
            "Solution selection"
            :range (solutions-list (om::object self))
            :di-action #'(lambda (m)
              (setf (output-solution (om::object self)) (nth (om::om-get-selected-item-index m) (solutions-list (om::object self)))); set the output to the selected solution
            )
          )

          ; button to start or restart the search
          (om::om-make-dialog-item 
            'om::om-button
            (om::om-make-point 5 50) ; position (horizontal, vertical)
            (om::om-make-point 100 20) ; size (horizontal, vertical)
            "Start"
            :di-action #'(lambda (b) 
                          ;(dolist (e (chords (input-chords (object self))))
                          ;  (print (lmidic e))
                          ;)
                          ; reset the solutions for the new search
                          (setf (solutions-list (om::object self)) '())
                          (setf (solution (om::object self)) nil)
                          (progn
                            (update-solutions-list self search-panel); update the pop-up menu with the list of the solutions selected by the user
                            (oa::om-invalidate-view self)
                          )
                          ; reset the boolean that tells wether we want to stop the search or not
                          (setf (stop-search (om::object self)) nil)
                          (cond
                            ((string-equal (tool-mode (om::object self)) "Melody-Finder"); melody finder mode, where the user gives as input a voice with chords
                              (let init; list to take the result of the call to melody-finder
                                (setq init (melody-finder (input-chords (om::object self)) (om::tree (input-rhythm (om::object self))) (key (om::object self)) (mode (om::object self)))); get the search engine and the first solution of the CSP
                                ; update the fields of the object to their new value
                                (setf (result (om::object self)) init); store the result of the call to melody finder
                              )
                            )
                            ((string-equal (tool-mode (om::object self)) "Accompagnement-Finder")
                              (print "This mode is not supported yet")
                            )
                            ((string-equal (tool-mode (om::object self)) "Ornement")
                              (print "This mode is not supported yet")
                            )
                          )
            )
          )

          ; button to find the next solution
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 115 50) ; position
            (om::om-make-point 100 20) ; size
            "Next"
            :di-action #'(lambda (b)
                          (print "Searching for the next solution")
                          ;reset the boolean because we want to continue the search
                          (setf (stop-search (om::object self)) nil)
                          ;get the next solution  
                          (mp:process-run-function ; start a new thread for the execution of the next method
                            "next thread" ; name of the thread, not necessary but useful for debugging
                            nil ; process initialization keywords, not needed here
                            (lambda () ; function to call
                              (setf (solution (om::object self)) (search-next-melody-finder (result (om::object self)) (om::tree (input-rhythm (om::object self))) (om::object self)))
                            )
                            ; arguments if necessary
                          )
                          ;(setf (solution (om::object self)) (search-next-melody-finder (result (om::object self)) (om::tree (input-rhythm (om::object self))) (om::object self)))
            )
          )

          ; button to stop the search if the user wishes to
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 225 50)
            (om::om-make-point 100 20)
            "Stop"
            :di-action #'(lambda (b)
              (setf (stop-search (om::object self)) t) ; set the boolean to true so when the timestop object tells the search engine it stopped, it can check and see the user stopped the search
            )
          )

          ; button to open the voice object editor of the current solution
          (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 5 90); position
            (om::om-make-point 160 20); size
            "See solution"
            :di-action #'(lambda (b)
                            ;(print (solution (om::object self)))
                            (om::openeditorframe ; open a voice window displaying the selected solution
                              (om::omNG-make-new-instance 
                                (solution (om::object self)); the last solution
                                "current solution" ; name of the window
                              )
                            )
            )  
          )

          ;button to add the solution to the list of solutions (if we find it interesting and want to keep it)
          (om::om-make-dialog-item 
            'om::om-button
            (om::om-make-point 165 90) ; position
            (om::om-make-point 160 20) ; size
            "Keep Solution"
            :di-action #'(lambda (b)
                          (if (typep (solution (om::object self)) 'null); if there is no solution to add
                            (error "There is no solution to keep.")
                          )
                          ;add the element to the list
                          (if (typep (solutions-list (om::object self)) 'null); if it's the first solution
                            (setf (solutions-list (om::object self)) (list (solution (om::object self)))); initialize the list
                            (nconc (solutions-list (om::object self)) (list (solution (om::object self)))); add it to the end
                          )   
                          (progn
                            (update-solutions-list self search-panel); update the pop-up menu with the list of the solutions selected by the user
                            (oa::om-invalidate-view self)
                            ;(print "updated solutions")
                          )
            )
          )
        )
      )

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; creating the constraints panel ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ; coordinates here are local to constraint-panel
      (elements-constraints-panel
        (om::om-add-subviews
          constraints-panel

          ; title
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 350 2) 
            (om::om-make-point 200 20) 
            "Additional constraints"
            :font om::*om-default-font1b*
          )

          ;checkbox for constraint 1
          (om::om-make-dialog-item
            'om::om-check-box
            (om::om-make-point 10 50) ; position
            (om::om-make-point 20 20) ; size
            "Constraint 1"
            :di-action #'(lambda (c)
                          (print "Constraint 1 checked")
                        )
          )

          ; name for constraint 1
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 30 50) 
            (om::om-make-point 200 20) 
            "Constraint 1"
            :font om::*om-default-font1*
          )

          ;checkbox for constraint 2 (under constraint 1)
          (om::om-make-dialog-item
            'om::om-check-box
            (om::om-make-point 10 80) ; position
            (om::om-make-point 20 20) ; size
            "Constraint 2"
            :di-action #'(lambda (c)
                          (print "Constraint 2 checked")
                        )
          )

          ; name for constraint 2
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 30 80) 
            (om::om-make-point 200 20) 
            "Constraint 2"
            :font om::*om-default-font1*
          )

          ;checkbox for constraint 3 (to the right of constraint 1)
          (om::om-make-dialog-item
            'om::om-check-box
            (om::om-make-point 230 50) ; position
            (om::om-make-point 20 20) ; size
            "Constraint 3"
            :di-action #'(lambda (c)
                          (print "Constraint 3 checked")
                        )
          )

          ; name for constraint 3
          (om::om-make-dialog-item 
            'om::om-static-text 
            (om::om-make-point 250 50) 
            (om::om-make-point 200 20) 
            "Constraint 3"
            :font om::*om-default-font1*
          )
        )
      )

      ;;; add new panel here

    )

    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      input-panel
      search-panel
      constraints-panel
    )
  )
  ; return the editor
  self






  ;; ;;; text boxes (they can be edited!)

  ;;     ;; (om::om-make-dialog-item
  ;;     ;;   'om::text-box
  ;;     ;;   (om::om-make-point 660 50) 
  ;;     ;;   (om::om-make-point 180 20) 
  ;;     ;;   "Variety of the solutions" 
  ;;     ;;   :font om::*om-default-font1* 
  ;;     ;; )

)

