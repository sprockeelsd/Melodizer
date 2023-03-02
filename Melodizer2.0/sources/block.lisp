(in-package :mldz)

;;;====================
;;;= BLOCK OBJECT =
;;;====================

(om::defclass! block ()
  ;attributes
  ((block-list :accessor block-list :initarg :block-list :initform nil :documentation "")
   (melody-source :accessor melody-source :initarg :melody-source :initform nil :documentation "")
   (position-list :accessor position-list :initarg :position-list :initform nil :documentation "")
   (bar-length :accessor bar-length :initform 0 :type integer)
   (beat-length :accessor beat-length :initform 0 :type integer)
   (voices :accessor voices :initform nil :type integer)
   (min-pushed-notes :accessor min-pushed-notes :initform nil :type integer)
   (max-pushed-notes :accessor max-pushed-notes :initform nil :type integer)
   (min-notes :accessor min-notes :initform nil :type integer)
   (max-notes :accessor max-notes :initform nil :type integer)
   (min-added-notes :accessor min-added-notes :initform nil :type integer)
   (max-added-notes :accessor max-added-notes :initform nil :type integer)
   (min-note-length-flag :accessor min-note-length-flag :initform nil :type integer)
   (min-note-length :accessor min-note-length :initform 0 :type integer)
   (max-note-length-flag :accessor max-note-length-flag :initform nil :type integer)
   (max-note-length :accessor max-note-length :initform 192 :type integer)
   (quantification :accessor quantification :initform nil :type string)
   (note-repartition-flag :accessor note-repartition-flag :initform nil :type integer)
   (note-repartition :accessor note-repartition :initform nil :type integer)
   (rhythm-repetition :accessor rhythm-repetition :initform nil :type string)
   (pause-quantity-flag :accessor pause-quantity-flag :initform nil :type integer)
   (pause-quantity :accessor pause-quantity :initform 0 :type integer)
   (pause-repartition-flag :accessor pause-repartition-flag :initform nil :type integer)
   (pause-repartition :accessor pause-repartition :initform 0 :type integer)
   (key-selection :accessor key-selection :initform nil :type string)
   (mode-selection :accessor mode-selection :initform nil :type string)
   (chord-key :accessor chord-key :initform nil :type string)
   (chord-quality :accessor chord-quality :initform nil :type string)
   (all-chord-notes :accessor all-chord-notes :initform nil :type integer)
   (min-pitch :accessor min-pitch :initform 1 :type integer)
   (min-pitch-flag :accessor min-pitch-flag :initform nil :type integer)
   (max-pitch :accessor max-pitch :initform 127 :type integer)
   (max-pitch-flag :accessor max-pitch-flag :initform nil :type integer)
   (pitch-direction :accessor pitch-direction :initform nil :type string)
   (golomb-ruler-size :accessor golomb-ruler-size :initform 0 :type integer)
   (note-repetition-flag :accessor note-repetition-flag :initform nil :type integer)
   (note-repetition-type :accessor note-repetition-type :initform "Random" :type string)
   (note-repetition :accessor note-repetition :initform 0 :type integer)
  )
  (:icon 225)
  (:documentation "This class implements Melodizer.
        Melodizer is a constraints based application aiming to improve composer's expression and exploration abilities
        by generating interesting and innovative melodies based on a set of constraints expressing musical rules.
        More information and a tutorial can be found at https://github.com/sprockeelsd/Melodizer")
)

(om::defclass! search ()
  ;attributes
  (
    (block-csp :accessor block-csp :initarg :block-csp :initform nil)
    (solution :accessor solution :initarg :solution :initform nil :documentation "The current solution of the CSP in the form of a voice object.")
    (result :accessor result
      :result :initform (list) :documentation
      "A temporary list holder to store the result of the call to the CSPs, shouldn't be touched.")
    (stop-search :accessor stop-search :stop-search :initform nil :documentation
      "A boolean variable to tell if the user wishes to stop the search or not.")
    (input-rhythm :accessor input-rhythm :input-rhythm :initform (make-instance 'voice) :documentation
      "The rhythm of the melody or a melody in the form of a voice object. ")
    (tempo :accessor tempo :initform 120 :type integer :documentation
      "The tempo (BPM) of the project")
    (branching :accessor branching :initform "Top down" :type string :documentation
      "The tempo (BPM) of the project")
    (percent-diff :accessor percent-diff :initform 0 :type integer)
  )
  (:icon 225)
  (:documentation "This class implements Melodizer.
        Melodizer is a constraints based application aiming to improve composer's expression and exploration abilities
        by generating interesting and innovative melodies based on a set of constraints expressing musical rules.
        More information and a tutorial can be found at https://github.com/sprockeelsd/Melodizer")
)

; the editor for the object
(defclass block-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self block)) t)
(defmethod om::get-editor-class ((self block)) 'block-editor)

(defmethod om::om-draw-contents ((view block-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

; To access the melodizer object, (om::object self)

(defmethod initialize-instance ((self block-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self block-editor))

  ; create the main view of the object
  (make-main-view self)

  ;;;;;;;;;;;;;;;;;
  ;;; sub views ;;;
  ;;;;;;;;;;;;;;;;;

  (let*
    (
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;; setting the different regions of the tool ;;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (block-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )


      ; part of the display for everything that has to do with adding new constraints to the problem
      (time-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 410 5)
        :bg-color om::*azulito*)
      )
      ; part of the display to put different solutions together
      (pitch-constraints-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 815 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-block-constraints-panel (make-block-constraints-panel self block-constraints-panel))

    (setf elements-time-constraints-panel (make-time-constraints-panel self time-constraints-panel))
    ; create the pitch constrains panel
    (setf elements-pitch-constraints-panel (make-pitch-constraints-panel self pitch-constraints-panel))
    ; add the subviews for the different parts into the main view
    (om::om-add-subviews
      self
      block-constraints-panel
      time-constraints-panel
      pitch-constraints-panel
    )
  )
  ; return the editor
  self
)



    ;;;;;;;;;;;;;;;;;
    ;;; main view ;;;
    ;;;;;;;;;;;;;;;;;

; this function creates the elements for the main panel
(defun make-main-view (editor)
  ; background colour
  (om::om-set-bg-color editor om::*om-light-gray-color*) ;pour changer le bg color. om peut fabriquer sa propre couleur: (om-make-color r g b)
)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; creating the constraints panel ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-block-constraints-panel (editor block-constraints-panel)
  (om::om-add-subviews
    block-constraints-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Block constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Bar length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Bar length"
      :range (loop :for n :from 0 :upto 32 collect n)
      :di-action #'(lambda (m)
        (setf (bar-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    ; (om::om-make-dialog-item
    ;   'om::om-static-text
    ;   (om::om-make-point 15 100)
    ;   (om::om-make-point 200 20)
    ;   "Beat length"
    ;   :font om::*om-default-font1b*
    ; )
    ;
    ; (om::om-make-dialog-item
    ;   'om::pop-up-menu
    ;   (om::om-make-point 170 100)
    ;   (om::om-make-point 200 20)
    ;   "Beat length"
    ;   :range '(0 1 2 3)
    ;   :di-action #'(lambda (m)
    ;     (setf (beat-length (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
    ;   )
    ; )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Voices"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Voices"
      :range (append '("None") (loop :for n :from 0 :upto 15 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (voices (om::object editor)) nil)
          (setf (voices (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Minimum pushed notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Minimum pushed notes"
      :range (append '("None") (loop :for n :from 0 :upto 10 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (min-pushed-notes (om::object editor)) nil)
          (setf (min-pushed-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Maximum pushed notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Maximum pushed notes"
      :range (append '("None") (loop :for n :from 0 :upto 10 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (max-pushed-notes (om::object editor)) nil)
          (setf (max-pushed-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Minimum notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 250)
      (om::om-make-point 200 20)
      "Minimum notes"
      :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (min-notes (om::object editor)) nil)
          (setf (min-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Maximum notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 300)
      (om::om-make-point 200 20)
      "Maximum notes"
      :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (max-notes (om::object editor)) nil)
          (setf (max-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 350)
      (om::om-make-point 200 20)
      "Minimum added notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 350)
      (om::om-make-point 200 20)
      "Minimum added notes"
      :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (min-added-notes (om::object editor)) nil)
          (setf (min-added-notes (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 400)
      (om::om-make-point 200 20)
      "Maximum added notes"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 400)
      (om::om-make-point 200 20)
      "Maximum added notes"
      :range (append '("None") (loop :for n :from 0 :upto 100 collect n))
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (typep check 'string)
          (setf (max-added-notes (om::object editor)) nil)
          (setf (max-added-notes (om::object editor)) check))
      )
    )
  )


)

; this function creates the elements of the main additional constraints panel
; coordinates here are local to constraint-panel
(defun make-time-constraints-panel (editor time-constraints-panel)
  (om::om-add-subviews
    time-constraints-panel

    ; title
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 120 20)
      "Time constraints"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Minimum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 50)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-note-length-flag (om::object editor)) 1)
                      (setf (min-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 190 50)
      (om::om-make-point 180 20); size
      "Minimum note length"
      :range '(0 192)
      :increment 1
      :di-action #'(lambda (s)
        (setf (min-note-length (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Maximum note length"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-note-length-flag (om::object editor)) 1)
                      (setf (max-note-length-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 190 100)
      (om::om-make-point 180 20); size
      "Maximum note length"
      :range '(0 192)
      :increment 1
      :di-action #'(lambda (s)
        (setf (max-note-length (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Quantification"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Quantification"
      :range '("None" "1 bar" "1/2 bar" "1 beat" "1/2 beat" "1/4 beat" "1/8 beat" "1/3 bar" "1/6 bar" "1/3 beat" "1/6 beat" "1/12 beat")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (quantification (om::object editor)) nil)
          (setf (quantification (om::object editor)) check))
      )
    )

    ; (om::om-make-dialog-item
    ;   'om::om-static-text
    ;   (om::om-make-point 15 200)
    ;   (om::om-make-point 200 20)
    ;   "Note repartition"
    ;   :font om::*om-default-font1b*
    ; )
    ;
    ; (om::om-make-dialog-item
    ;   'om::om-check-box
    ;   (om::om-make-point 170 200)
    ;   (om::om-make-point 200 20)
    ;   ""
    ;   :di-action #'(lambda (c)
    ;                 (if (om::om-checked-p c)
    ;                   (setf (note-repartition-flag (om::object editor)) 1)
    ;                   (setf (note-repartition-flag (om::object editor)) nil)
    ;                 )
    ;   )
    ; )
    ;
    ; ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    ; (om::om-make-dialog-item
    ;   'om::om-slider
    ;   (om::om-make-point 190 200)
    ;   (om::om-make-point 180 20); size
    ;   "Note repartition"
    ;   :range '(1 100)
    ;   :increment 1
    ;   :di-action #'(lambda (s)
    ;     (setf (note-repartition (om::object editor)) (om::om-slider-value s))
    ;   )
    ; )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Rhythm repetition"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Rhythm repetition"
      :range '("None" "1 bar" "1/2 bar" "1 beat" "1/2 beat" "1/4 beat" "1/8 beat" "1/3 bar" "1/6 bar" "1/3 beat" "1/6 beat" "1/12 beat")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (rhythm-repetition (om::object editor)) nil)
          (setf (rhythm-repetition (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Pause quantity"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 250)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (pause-quantity-flag (om::object editor)) 1)
                      (setf (pause-quantity-flag (om::object editor)) nil)
                    )
      )
    )

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 190 250)
      (om::om-make-point 180 20); size
      "Pause quantity"
      :range '(1 192)
      :increment 1
      :di-action #'(lambda (s)
        (setf (pause-quantity (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Pause repartition"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 300)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (pause-repartition-flag (om::object editor)) 1)
                      (setf (pause-repartition-flag (om::object editor)) nil)
                    )
      )
    )

    ; slider to express how different the solutions should be (100 = completely different, 1 = almost no difference)
    (om::om-make-dialog-item
      'om::om-slider
      (om::om-make-point 190 300)
      (om::om-make-point 180 20); size
      "Pause repartition"
      :range '(0 191)
      :increment 1
      :di-action #'(lambda (s)
        (setf (pause-repartition (om::object editor)) (om::om-slider-value s))
      )
    )
  )
)

(defun make-pitch-constraints-panel (editor pitch-constraints-panel)
  (om::om-add-subviews
    pitch-constraints-panel

    ; title
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 150 2)
      (om::om-make-point 200 20)
      "Pitch constraints"
      :font om::*om-default-font1b*
    )

    ; Key

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 50)
      (om::om-make-point 200 20)
      "Key selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 50)
      (om::om-make-point 200 20)
      "Key selection"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (key-selection (om::object editor)) nil)
          (setf (key-selection (om::object editor)) check))
      )
    )

    ; Mode
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Mode selection"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Mode selection"
      :range '("None" "ionian (major)" "dorian" "phrygian" "lydian" "mixolydian" "aeolian (natural minor)" "locrian" "pentatonic" "harmonic minor" "chromatic")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (mode-selection (om::object editor)) nil)
          (setf (mode-selection (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Chord key"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Chord key"
      :range '("None" "C" "C#" "D" "Eb" "E" "F" "F#" "G" "Ab" "A" "Bb" "B")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-key (om::object editor)) nil)
          (setf (chord-key (om::object editor)) check))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Chord quality"
      :range '("None" "Major" "Minor" "Augmented" "Diminished" "Major 7" "Minor 7" "Dominant 7" "Minor 7 flat 5" "Diminished 7" "Minor-major 7"
        "Major 9" "Minor 9" "9 Augmented 5" "9 flatted 5" "7 flat 9" "Augmented 9" "Minor 11" "Major 11" "Dominant 11" "Dominant # 11" "Major # 11")
      :di-action #'(lambda (m)
        (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
        (if (string= check "None")
          (setf (chord-quality (om::object editor)) nil)
          (setf (chord-quality (om::object editor)) check))
      )
    )

    ; ;checkbox for all-different constraint
    ; (om::om-make-dialog-item
    ;   'om::om-check-box
    ;   (om::om-make-point 170 250)
    ;   (om::om-make-point 200 20)
    ;   "All chord notes"
    ;   ;:checked-p (find "all-different-notes" (optional-constraints (om::object editor)) :test #'equal)
    ;   :di-action #'(lambda (c)
    ;                 (if (om::om-checked-p c)
    ;                   (setf (all-chord-notes (om::object editor)) 1)
    ;                   (setf (all-chord-notes (om::object editor)) nil)
    ;                 )
    ;   )
    ;   :font om::*om-default-font1*
    ; )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 250)
      (om::om-make-point 200 20)
      "Minimum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 250)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (min-pitch-flag (om::object editor)) 1)
                      (setf (min-pitch-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 190 250)
      (om::om-make-point 180 20)
      "Minimum pitch"
      :range '(1 127)
      :increment 1
      :di-action #'(lambda (s)
        (setf (min-pitch (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 300)
      (om::om-make-point 200 20)
      "Maximum pitch"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 300)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (max-pitch-flag (om::object editor)) 1)
                      (setf (max-pitch-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 190 300)
      (om::om-make-point 180 20)
      "Maximum pitch"
      :range '(1 127)
      :increment 1
      :di-action #'(lambda (s)
        (setf (max-pitch (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 350)
      (om::om-make-point 200 20)
      "Note repetition"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-check-box
      (om::om-make-point 170 350)
      (om::om-make-point 20 20)
      ""
      :di-action #'(lambda (c)
                    (if (om::om-checked-p c)
                      (setf (note-repetition-flag (om::object editor)) 1)
                      (setf (note-repetition-flag (om::object editor)) nil)
                    )
      )
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 190 350)
      (om::om-make-point 180 20)
      "Note repetition"
      :range '(0 100)
      :increment 1
      :di-action #'(lambda (s)
        (setf (note-repetition (om::object editor)) (om::om-slider-value s))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 400)
      (om::om-make-point 200 20)
      "Repetition type"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
       'om::pop-up-menu
       (om::om-make-point 170 400)
       (om::om-make-point 200 20)
       "Repetition type"
       :range '("Random" "Soft" "Hard")
       :di-action #'(lambda (m)
          (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
          (setf (note-repetition-type (om::object editor)) check)
       )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 450)
      (om::om-make-point 200 20)
      "Pitch direction"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
       'om::pop-up-menu
       (om::om-make-point 170 450)
       (om::om-make-point 200 20)
       "Pitch direction"
       :range '("None" "Increasing" "Strictly increasing" "Decreasing" "Strictly decreasing")
       :di-action #'(lambda (m)
         (setq check (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
         (if (string= check "None")
           (setf (pitch-direction (om::object editor)) nil)
           (setf (pitch-direction (om::object editor)) check))
       )
    )

   (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 500)
      (om::om-make-point 200 20)
      "Golomb ruler size"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
       'om::pop-up-menu
       (om::om-make-point 170 500)
       (om::om-make-point 200 20)
       "Golomb ruler size"
       :range '("None" "1" "2" "3" "4" "5" "6" "7" "8" "9")
       :di-action #'(lambda (m)
         (setf (golomb-ruler-size (om::object editor)) (om::om-get-selected-item-index m))
       )
    )

  )
)

; the editor for the object
(defclass search-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self search)) t)
(defmethod om::get-editor-class ((self search)) 'search-editor)

(defmethod om::om-draw-contents ((view search-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view
      view
      ;;; DRAW SOMETHING ?
    )
  )
)

(defmethod initialize-instance ((self search-editor) &rest args)
  ;;; do what needs to be done by default
  (call-next-method) ; start the search by default?
  (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self search-editor))

  ; create the main view of the object
  (make-main-view self)

  (let*
    (
      (search-panel (om::om-make-view 'om::om-view
        :size (om::om-make-point 400 605)
        :position (om::om-make-point 5 5)
        :bg-color om::*azulito*)
      )
    )

    (setf elements-search-panel (make-search-panel self search-panel))

    (om::om-add-subviews
      self
      search-panel
    )
  )
  self
)

(defun make-search-panel (editor search-panel)
  (om::om-add-subviews
    search-panel
    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 145 2)
      (om::om-make-point 120 20)
      "Search Parameters"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 5 50) ; position (horizontal, vertical)
      (om::om-make-point 130 20) ; size (horizontal, vertical)
      "Start"
      :di-action #'(lambda (b)
        (let init
          (setq init (new-melodizer (block-csp (om::object editor)) (percent-diff (om::object editor)) (branching (om::object editor))))
          (setf (result (om::object editor)) init)
          ; TO TEST THE GOLOMB RULER PROGRAM
          ;(setq init (golomb-ruler 5))
          ;(setf (result (om::object editor)) init)
        )
      )
    )

    (om::om-make-dialog-item
      'om::om-button
      (om::om-make-point 135 50) ; position
      (om::om-make-point 130 20) ; size
      "Next"
      :di-action #'(lambda (b)
        (if (typep (result (om::object editor)) 'null); if the problem is not initialized
          (error "The problem has not been initialized. Please set the input and press Start.")
        )
        (print "Searching for the next solution")
        ;reset the boolean because we want to continue the search
        (setf (stop-search (om::object editor)) nil)
        ;get the next solution
        (mp:process-run-function ; start a new thread for the execution of the next method
          "next thread" ; name of the thread, not necessary but useful for debugging
          nil ; process initialization keywords, not needed here
          (lambda () ; function to call
            (setf (solution (om::object editor)) (new-search-next (result (om::object editor)) (om::object editor)))
            ;TO TEST THE GOLOMB-RULER PROGRAM
            ;(setf (solution (om::object editor)) (search-next-golomb-ruler (result (om::object editor))))
            (om::openeditorframe ; open a voice window displaying the solution
              (om::omNG-make-new-instance (solution (om::object editor)) "current solution")
            )
          )
        )
      )
    )

    (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 265 50) ; position (horizontal, vertical)
        (om::om-make-point 130 20) ; size (horizontal, vertical)
        "Stop"
        :di-action #'(lambda (b)
          (setf (stop-search (om::object editor)) t)
        )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 100)
      (om::om-make-point 200 20)
      "Tempo (BPM)"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 100)
      (om::om-make-point 200 20)
      "Tempo"
      :range (loop :for n :from 30 :upto 200 collect n)
      :di-action #'(lambda (m)
        (setf (tempo (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )

    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 150)
      (om::om-make-point 200 20)
      "Branching"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::pop-up-menu
      (om::om-make-point 170 150)
      (om::om-make-point 200 20)
      "Branching"
      :range '("Top down" "Full" "Top down random")
      :di-action #'(lambda (m)
        (setf (branching (om::object editor)) (nth (om::om-get-selected-item-index m) (om::om-get-item-list m)))
      )
    )


    (om::om-make-dialog-item
      'om::om-static-text
      (om::om-make-point 15 200)
      (om::om-make-point 200 20)
      "Difference Percentage"
      :font om::*om-default-font1b*
    )

    (om::om-make-dialog-item
      'om::slider
      (om::om-make-point 170 200)
      (om::om-make-point 200 20)
      "Difference Percentage"
      :range '(0 100)
      :increment 1
      :di-action #'(lambda (s)
        (setf (percent-diff (om::object editor)) (om::om-slider-value s))
      )
    )
  )
)
