(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains all the cp-params interface.
; That is to say the interface blocks, as well as the global variables updated via the interface.

;;;====================
;;;= cp-params OBJECT =
;;;====================

(print "Loading cp-params object...")

(om::defclass! cp-params ()
;attributes
(
    ; ---------- Input cantus firmus ----------
    (cf-voice :accessor cf-voice :initarg :cf-voice :initform nil :documentation "")
    ; ---------- Melodic parameters ----------
    (m-step-cost-param :accessor m-step-cost-param :initform "No cost" :type string :documentation "")
    (m-third-cost-param :accessor m-third-cost-param :initform "Low cost" :type string :documentation "")
    (m-fourth-cost-param :accessor m-fourth-cost-param :initform "Low cost" :type string :documentation "")
    (m-tritone-cost-param :accessor m-tritone-cost-param :initform "Forbidden" :type string :documentation "")
    (m-fifth-cost-param :accessor m-fifth-cost-param :initform "Medium cost" :type string :documentation "")
    (m-sixth-cost-param :accessor m-sixth-cost-param :initform "Medium cost" :type string :documentation "")
    (m-seventh-cost-param :accessor m-seventh-cost-param :initform "Medium cost" :type string :documentation "")
    (m-octave-cost-param :accessor m-octave-cost-param :initform "Low cost" :type string :documentation "")
    ; ---------- Global parameters (species 1) ----------
    (borrow-mode-param :accessor borrow-mode-param :initform "Major" :type string :documentation "")
    (borrow-cost-param :accessor borrow-cost-param :initform "High cost" :type string :documentation "")
    (h-fifth-cost-param :accessor h-fifth-cost-param :initform "Low cost" :type string :documentation "")
    (h-octave-cost-param :accessor h-octave-cost-param :initform "Low cost" :type string :documentation "")
    (con-motion-cost-param :accessor con-motion-cost-param :initform "No cost" :type string :documentation "")
    (obl-motion-cost-param :accessor obl-motion-cost-param :initform "Low cost" :type string :documentation "")
    (dir-motion-cost-param :accessor dir-motion-cost-param :initform "Medium cost" :type string :documentation "")
    (penult-rule-check-param :accessor penult-rule-check-param :initform t :type boolean :documentation "")
    ; ---------- Species parameters ----------
    ; Species 2
    (penult-sixth-cost-param :accessor penult-sixth-cost-param :initform "Last resort" :type string :documentation "")
    ; Species 3
    (non-cambiata-cost-param :accessor non-cambiata-cost-param :initform "High cost" :type string :documentation "")
    (two-beats-apart-cost-param :accessor two-beats-apart-cost-param :initform "Low cost" :type string :documentation "")
    (con-m-after-skip-check-param :accessor con-m-after-skip-check-param :initform nil :type boolean :documentation "")
    ; Species 4
    (two-bars-apart-cost-param :accessor two-bars-apart-cost-param :initform "High cost" :type string :documentation "")
    (no-syncopation-cost-param :accessor no-syncopation-cost-param :initform "Last resort" :type string :documentation "")
    ; Species 5
    (pref-species-slider-param :accessor pref-species-slider-param :initform 50 :type integer :documentation "")
    ; ---------- Solver parameters ----------
    (species-param :accessor species-param :initform "5th" :type string :documentation "")
    (voice-type-param :accessor voice-type-param :initform "Above" :type string :documentation "")
    (irreverence-slider-param :accessor irreverence-slider-param :initform 0 :type integer :documentation "")
    (min-skips-slider-param :accessor min-skips-slider-param :initform 0 :type integer :documentation "")
    ; ---------- Output & Stop ----------
    (current-csp :accessor current-csp :initform nil :documentation "")
    (result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
)
    (:icon 225)
    (:documentation "This class implements FuxCP.
    FuxCP is a constraints programming based tool aiming to generate counterpoints based on cantus firmus.")
)

; the editor for the object
(defclass params-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self cp-params)) t)
(defmethod om::get-editor-class ((self cp-params)) 'params-editor)

(defmethod om::om-draw-contents ((view params-editor))
    (let* ((object (om::object view)))
        (om::om-with-focused-view view)
    )
)

; this function creates the elements for the main panel
(defun make-main-view (editor)
    ; background colour
    (om::om-set-bg-color editor om::*om-light-gray-color*)
)

; To access the melodizer object, (om::object self)
(defmethod initialize-instance ((self params-editor) &rest args)
    ;;; do what needs to be done by default
    (call-next-method) ; start the search by default?
    (make-my-interface self)
)

; function to create the tool's interface
(defmethod make-my-interface ((self params-editor))
    (print "Creating interface...")
    ; create the main view of the object
    (make-main-view self)

    (let*
        (
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;; setting the different regions of the tool ;;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (melodic-params-panel (om::om-make-view 'om::om-view
            :size (om::om-make-point 400 450)
            :position (om::om-make-point 5 5)
            :bg-color om::*azulito*)
        )
        (general-params-panel (om::om-make-view 'om::om-view
            :size (om::om-make-point 400 450)
            :position (om::om-make-point 410 5)
            :bg-color om::*azulote*)
        )
        (species-params-panel (om::om-make-view 'om::om-view
            :size (om::om-make-point 400 450)
            :position (om::om-make-point 815 5)
            :bg-color (om::make-color-255 230 190 165))
        )
        (search-params-panel (om::om-make-view 'om::om-view
            :size (om::om-make-point 400 450)
            :position (om::om-make-point 1220 5)
            :bg-color om::*maq-color*)
        )
        (search-buttons (om::om-make-view 'om::om-view
            :size (om::om-make-point 390 120)
            :position (om::om-make-point 1225 330)
            :bg-color om::*workspace-color*)
        )
        )

        (make-general-params-panel self general-params-panel)
        (make-melodic-params-panel self melodic-params-panel)
        (make-species-params-panel self species-params-panel)
        (make-search-params-panel self search-params-panel)
        (make-search-buttons self search-buttons)
        ; ; add the subviews for the different parts into the main view
        (om::om-add-subviews
            self
            search-buttons
            search-params-panel
            species-params-panel
            general-params-panel
            melodic-params-panel
        )
    )
    ; return the editor
    self
)

(defun make-melodic-params-panel (editor melodic-params-panel)
    (om::om-add-subviews
        melodic-params-panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 90 2)
        (om::om-make-point 220 20)
        "Preferences for Melodic Intervals of..."
        :font om::*om-default-font2b*
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 50)
        (om::om-make-point 150 20)
        "Step"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 50)
        (om::om-make-point 200 20)
        "Step"
        :range (costs-list t)
        :value (m-step-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-step-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 100)
        (om::om-make-point 150 20)
        "Third"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 100)
        (om::om-make-point 200 20)
        "Third"
        :range (costs-list)
        :value (m-third-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-third-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 150)
        (om::om-make-point 150 20)
        "Fourth"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 150)
        (om::om-make-point 200 20)
        "Fourth"
        :range (costs-list)
        :value (m-fourth-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-fourth-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 200)
        (om::om-make-point 150 20)
        "Tritone"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 200)
        (om::om-make-point 200 20)
        "Tritone"
        :range (costs-list)
        :value (m-tritone-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-tritone-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 250)
        (om::om-make-point 150 20)
        "Fifth"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 250)
        (om::om-make-point 200 20)
        "Fifth"
        :range (costs-list)
        :value (m-fifth-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-fifth-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 300)
        (om::om-make-point 150 20)
        "Sixth"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 300)
        (om::om-make-point 200 20)
        "Sixth"
        :range (costs-list)
        :value (m-sixth-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-sixth-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 350)
        (om::om-make-point 150 20)
        "Seventh"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 350)
        (om::om-make-point 200 20)
        "Seventh"
        :range (costs-list)
        :value (m-seventh-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-seventh-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 400)
        (om::om-make-point 150 20)
        "Octave"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 400)
        (om::om-make-point 200 20)
        "Octave"
        :range (costs-list)
        :value (m-octave-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (m-octave-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )
    )
)

(defun make-general-params-panel (editor general-params-panel)
    (om::om-add-subviews
        general-params-panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 140 2)
        (om::om-make-point 220 20)
        "General Preferences"
        :font om::*om-default-font2b*
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 50)
        (om::om-make-point 150 20)
        "Borrowing mode"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 50)
        (om::om-make-point 200 20)
        "Borrowing mode"
        :range (list "None" "Major" "Minor")
        :value (borrow-mode-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (borrow-mode-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 100)
        (om::om-make-point 150 20)
        "Borrowed notes"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 100)
        (om::om-make-point 200 20)
        "Borrowed notes"
        :range (costs-list t)
        :value (borrow-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (borrow-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 150)
        (om::om-make-point 150 20)
        "Fifths in down beats"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 150)
        (om::om-make-point 200 20)
        "Fifths in down beats"
        :range (costs-list t)
        :value (h-fifth-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (h-fifth-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 200)
        (om::om-make-point 150 20)
        "Octaves in down beats"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 200)
        (om::om-make-point 200 20)
        "Octaves in down beats"
        :range (costs-list t)
        :value (h-octave-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (h-octave-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 250)
        (om::om-make-point 150 20)
        "Contrary motions"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 250)
        (om::om-make-point 200 20)
        "Contrary motions"
        :range (costs-list t)
        :value (con-motion-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (con-motion-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 300)
        (om::om-make-point 150 20)
        "Oblique motions"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 300)
        (om::om-make-point 200 20)
        "Oblique motions"
        :range (costs-list)
        :value (obl-motion-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (obl-motion-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 350)
        (om::om-make-point 150 20)
        "Direct motions"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 350)
        (om::om-make-point 200 20)
        "Direct motions"
        :range (costs-list t)
        :value (dir-motion-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (dir-motion-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 400)
        (om::om-make-point 150 20)
        "Apply specific penultimate note rules"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-check-box
        (om::om-make-point 170 400)
        (om::om-make-point 20 20)
        "Apply specific penultimate note rules"
        ::checked-p (penult-rule-check-param (om::object editor))
        :di-action #'(lambda (c)
            (if (om::om-checked-p c)
                (setf (penult-rule-check-param (om::object editor)) t)
                (setf (penult-rule-check-param (om::object editor)) nil)
            )
        )
        )
    )
)

(defun make-species-params-panel (editor species-params-panel)
    (om::om-add-subviews
        species-params-panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 130 2)
        (om::om-make-point 220 20)
        "Species Specific Preferences"
        :font om::*om-default-font2b*
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 50)
        (om::om-make-point 150 20)
        "2nd: Penultimate thesis note is not a fifth"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 50)
        (om::om-make-point 200 20)
        "2nd: Penultimate thesis note is not a fifth"
        :range (costs-list t)
        :value (penult-sixth-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (penult-sixth-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 100)
        (om::om-make-point 150 20)
        "3rd: Non-cambiata notes"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 100)
        (om::om-make-point 200 20)
        "3rd: Non-cambiata notes"
        :range (costs-list t)
        :value (non-cambiata-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (non-cambiata-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 150)
        (om::om-make-point 150 20)
        "3rd: Same notes two beats apart"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 150)
        (om::om-make-point 200 20)
        "3rd: Same notes two beats apart"
        :range (costs-list)
        :value (two-beats-apart-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (two-beats-apart-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 200)
        (om::om-make-point 150 20)
        "3rd: Force joint contrary melody after skip (from Bitsch)"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-check-box
        (om::om-make-point 170 200)
        (om::om-make-point 20 20)
        "3rd: Force joint contrary melody after skip (from Bitsch)"
        ::checked-p (con-m-after-skip-check-param (om::object editor))
        :di-action #'(lambda (c)
            (if (om::om-checked-p c)
                (setf (con-m-after-skip-check-param (om::object editor)) t)
                (setf (con-m-after-skip-check-param (om::object editor)) nil)
            )
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 250)
        (om::om-make-point 150 20)
        "4th: Same syncopations two bars apart"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 250)
        (om::om-make-point 200 20)
        "4th: Same syncopations two bars apart"
        :range (costs-list)
        :value (two-bars-apart-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (two-bars-apart-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 300)
        (om::om-make-point 150 20)
        "4th: No syncopation"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 300)
        (om::om-make-point 200 20)
        "4th: No syncopation"
        :range (costs-list t)
        :value (no-syncopation-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (no-syncopation-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 350)
        (om::om-make-point 150 50)
        "5th: Preference to a lot of quarters [left] OR a lot of syncopations [right]"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-slider
        (om::om-make-point 170 350)
        (om::om-make-point 200 20)
        "5th: Preference to a lot of quarters [left] OR a lot of syncopations [right]"
        :range '(0 100)
        :increment 1
        :value (pref-species-slider-param (om::object editor))
        :di-action #'(lambda (s)
            (setf (pref-species-slider-param (om::object editor)) (om::om-slider-value s))
        )
        )
    )
)

(defun make-search-params-panel (editor search-params-panel)
    (om::om-add-subviews
        search-params-panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 140 2)
        (om::om-make-point 200 20)
        "Solver Configuration"
        :font om::*om-default-font2b*
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 50)
        (om::om-make-point 150 20)
        "Chosen species"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 50)
        (om::om-make-point 200 20)
        "Chosen species"
        :range (list "1st" "2nd" "3rd" "4th" "5th")
        :value (species-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (species-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 100)
        (om::om-make-point 150 20)
        "Voice range"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 100)
        (om::om-make-point 200 20)
        "Voice range"
        :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
        :value (voice-type-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (voice-type-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 150)
        (om::om-make-point 150 20)
        "Irreverence"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-slider
        (om::om-make-point 170 150)
        (om::om-make-point 200 20)
        "Irreverence"
        :range '(0 40)
        :increment 1
        :value (irreverence-slider-param (om::object editor))
        :di-action #'(lambda (s)
            (setf (irreverence-slider-param (om::object editor)) (om::om-slider-value s))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 200)
        (om::om-make-point 150 20)
        "Minimum % of skips"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-slider
        (om::om-make-point 170 200)
        (om::om-make-point 200 20)
        "Minimum % of skips"
        :range '(0 100)
        :increment 1
        :value (min-skips-slider-param (om::object editor))
        :di-action #'(lambda (s)
            (setf (min-skips-slider-param (om::object editor)) (om::om-slider-value s))
        )
        )
    )
)

(defun make-search-buttons (editor search-buttons)
    (om::om-add-subviews
        search-buttons
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 140 5)
        (om::om-make-point 150 20)
        "Solver Launcher"
        :font om::*om-default-font3b*
        )

        (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 10 50) ; position (horizontal, vertical)
        (om::om-make-point 120 20) ; size (horizontal, vertical)
        "Save Config"
        :di-action #'(lambda (b)
            (if (null (cf-voice (om::object editor))); if the problem is not initialized
                (error "No voice has been given to the solver. Please set a cantus firmus into the second input and try again.")
            )
            (set-global-cf-variables
                (cf-voice (om::object editor))
                (convert-to-voice-integer (voice-type-param (om::object editor)))
                (borrow-mode-param (om::object editor))
            )
            (defparameter *params* (make-hash-table))
            ;; set melodic parameters
            (setparam-cost 'm-step-cost (m-step-cost-param (om::object editor)))
            (setparam-cost 'm-third-cost (m-third-cost-param (om::object editor)))
            (setparam-cost 'm-fourth-cost (m-fourth-cost-param (om::object editor)))
            (setparam-cost 'm-tritone-cost (m-tritone-cost-param (om::object editor)))
            (setparam-cost 'm-fifth-cost (m-fifth-cost-param (om::object editor)))
            (setparam-cost 'm-sixth-cost (m-sixth-cost-param (om::object editor)))
            (setparam-cost 'm-seventh-cost (m-seventh-cost-param (om::object editor)))
            (setparam-cost 'm-octave-cost (m-octave-cost-param (om::object editor)))
            ;; set general parameters
            (setparam 'borrow-mode (borrow-mode-param (om::object editor)))
            (setparam-cost 'borrow-cost (borrow-cost-param (om::object editor)))
            (setparam-cost 'h-fifth-cost (h-fifth-cost-param (om::object editor)))
            (setparam-cost 'h-octave-cost (h-octave-cost-param (om::object editor)))
            (setparam-cost 'con-motion-cost (con-motion-cost-param (om::object editor)))
            (setparam-cost 'obl-motion-cost (obl-motion-cost-param (om::object editor)))
            (setparam-cost 'dir-motion-cost (dir-motion-cost-param (om::object editor)))
            (setparam 'penult-rule-check (penult-rule-check-param (om::object editor)))
            ;; set species specific parameters
            (setparam-cost 'penult-sixth-cost (penult-sixth-cost-param (om::object editor)))
            (setparam-cost 'non-cambiata-cost (non-cambiata-cost-param (om::object editor)))
            (setparam-cost 'two-beats-apart-cost (two-beats-apart-cost-param (om::object editor)))
            (setparam 'con-m-after-skip-check (con-m-after-skip-check-param (om::object editor)))
            (setparam-cost 'two-bars-apart-cost (two-bars-apart-cost-param (om::object editor)))
            (setparam-cost 'no-syncopation-cost (no-syncopation-cost-param (om::object editor)))
            (setparam-slider 'pref-species-slider (pref-species-slider-param (om::object editor)))
            ;; set search parameters
            (setparam-slider 'irreverence-slider (irreverence-slider-param (om::object editor)))
            (setparam-slider 'min-skips-slider (min-skips-slider-param (om::object editor)))
            (setf (current-csp (om::object editor)) (fux-cp (convert-to-species-integer (species-param (om::object editor)))))
        )
        )

        (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 135 50) ; position
        (om::om-make-point 120 20) ; size
        "Next Solution"
        :di-action #'(lambda (b)
            (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                (error "The problem has not been initialized. Please set the input and press Start.")
            )
            (print "Searching for the next solution")
            ;reset the boolean because we want to continue the search
            (setparam 'is-stopped nil)
            ;get the next solution
            (mp:process-run-function ; start a new thread for the execution of the next method
                "solver-thread" ; name of the thread, not necessary but useful for debugging
                nil ; process initialization keywords, not needed here
                (lambda () ; function to call
                    (setf
                        (result-voice (om::object editor))
                        (search-next-fux-cp (current-csp (om::object editor)))
                    )
                    (om::openeditorframe ; open a voice window displaying the solution
                        (om::omNG-make-new-instance (result-voice (om::object editor)) "Current solution")
                    )
                )
            )
        )
        )

        (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 260 50) ; position (horizontal, vertical)
        (om::om-make-point 120 20) ; size (horizontal, vertical)
        "Stop"
        :di-action #'(lambda (b)
            (setparam 'is-stopped t)
        )
        )
  )
)

; return the list of available costs for the preferences
; @is-required: if true, "Forbidden" is removed
(defun costs-list (&optional (is-required nil))
    (let (
        (costs (list "No cost" "Low cost" "Medium cost" "High cost" "Last resort" "Cost prop. to length" "Forbidden"))
    )
        (if is-required
            (butlast costs)
            costs
        )
    )
)

; set the value @v in the hash table @h with key @k
(defun seth (h k v)
    (setf (gethash k h) v)
)

; set the value @v in the parameters with key @k
(defun setparam (k v)
    (seth *params* k v)
)

; set the cost-converted value @of v in the parameters with key @k
(defun setparam-cost (k v)
    (setparam k (convert-to-cost-integer v))
)

; set the species-converted value @of v in the parameters with key @k
(defun setparam-species (k v)
    (setparam k (convert-to-species-integer v))
)

; set the slider-converted value @of v in the parameters with key @k
(defun setparam-slider (k v)
    (setparam k (convert-to-percent v))
)

; convert a cost to an integer
(defun convert-to-cost-integer (param)
    (cond
    ((equal param "No cost") 0)
    ((equal param "Low cost") 1)
    ((equal param "Medium cost") 2)
    ((equal param "High cost") 4)
    ((equal param "Last resort") 8)
    ((equal param "Cost prop. to length") (* 2 *cf-len))
    ((equal param "Forbidden") (* 64 *cf-len))
    )
)

; convert a species to an integer
(defun convert-to-species-integer (param)
    (cond
    ((equal param "1st") 1)
    ((equal param "2nd") 2)
    ((equal param "3rd") 3)
    ((equal param "4th") 4)
    ((equal param "5th") 5)
    )
)

;; convert the string for the voice type to an integer
;; belong to {"Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below"}
;; convert to {-3 -2 -1 0 1 2 3}
(defun convert-to-voice-integer (param)
    (cond
    ((equal param "Really far above") 3)
    ((equal param "Far above") 2)
    ((equal param "Above") 1)
    ((equal param "Same range") 0)
    ((equal param "Below") -1)
    ((equal param "Far below") -2)
    ((equal param "Really far below") -3)
    )
)

; convert a slider value to a percentage
(defun convert-to-percent (param)
    (float (/ param 100))
)

; convert a mode to an integer
(defun convert-to-mode-integer (param tone)
    (cond
    ((equal param "Major") (mod tone 12))
    ((equal param "Minor") (mod (+ tone 3) 12))
    ((equal param "None") nil)
    )
)

; define all the global variables
(defun set-global-cf-variables (cantus-firmus voice-type borrow-mode)
    ; Lower bound and upper bound related to the cantus firmus pitch
    (defparameter VOICE_TYPE voice-type)
    (defparameter RANGE_UB (+ 12 (* 6 VOICE_TYPE)))
    (defparameter RANGE_LB (+ -6 (* 6 VOICE_TYPE)))
    (defparameter *prev-sol-check nil)
    (defparameter rythmic+pitches nil)
    (defparameter rythmic-om nil)
    (defparameter pitches-om nil)
    ; get the tonalite of the cantus firmus
    (defparameter *tonalite-offset (get-tone-offset cantus-firmus))
    ; get the *scale of the cantus firmus
    (defparameter *scale (build-scaleset (get-scale) *tonalite-offset))
    ; *chromatic *scale
    (defparameter *chromatic-scale (build-scaleset (get-scale "chromatic") *tonalite-offset))
    ; get the first note of each chord of the cantus firmus
    (defparameter *cf (mapcar #'first (to-pitch-list (om::chords cantus-firmus))))
    ; get the tempo of the cantus firmus
    (defparameter *cf-tempo (om::tempo cantus-firmus))
    ; get the first note of the cantus firmus ;; just used for the moment
    (defparameter *tone-pitch-cf (first *cf))
    ; get the borrowed scale of the cantus firmus, i.e. some notes borrowed from the natural scale of the tone (useful for modes)
    (setq mode-param (convert-to-mode-integer borrow-mode *tone-pitch-cf))
    (if mode-param
        (defparameter *borrowed-scale (build-scaleset (get-scale "borrowed") mode-param))
        (defparameter *borrowed-scale (list))
    )
    ; get notes that are not in the natural scale of the tone
    (defparameter *off-scale (set-difference *chromatic-scale *scale))
    ; set the pitch range of the counterpoint
    (defparameter *cp-range (range (+ *tone-pitch-cf RANGE_UB) :min (+ *tone-pitch-cf RANGE_LB))) ; arbitrary range
    ; set counterpoint pitch domain
    (defparameter *cp-domain (intersection *cp-range *scale))
    ; penultimate (first *cp) note domain
    (defparameter *chromatic-cp-domain (intersection *cp-range *chromatic-scale))
    ; set counterpoint extended pitch domain
    (defparameter *extended-cp-domain (intersection *cp-range (union *scale *borrowed-scale)))
    ; set the domain of the only barrowed notes
    (defparameter *off-domain (intersection *cp-range *off-scale))
    ; length of the cantus firmus
    (defparameter *cf-len (length *cf))
    ; *cf-last-index is the number of melodic intervals in the cantus firmus
    (defparameter *cf-last-index (- *cf-len 1))
    ; *cf-penult-index is the number of larger (n -> n+2) melodic intervals in the cantus firmus
    (defparameter *cf-penult-index (- *cf-len 2))
    ; COST_UB is the upper bound of the cost function
    (defparameter COST_UB (* *cf-len 20))
)