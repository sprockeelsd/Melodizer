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
    (succ-p-cons-cost-param :accessor succ-p-cons-cost-param :initform "Medium cost" :type string :documentation "")
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
    (species-param :accessor species-param :initform (list "1st" "1st") :type string :documentation "")
    (voice-type-param :accessor voice-type-param :initform (list "Really far above" "Above") :type string :documentation "")
    (min-skips-slider-param :accessor min-skips-slider-param :initform 0 :type integer :documentation "")
    ; ---------- Output & Stop ----------
    (current-csp :accessor current-csp :initform nil :documentation "")
    (result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
    ; ---------- Cost order --------------
    (no-syncope-order-param :accessor no-syncope-order-param :initform "1" :type integer :documentation "")
    (h-triad-order-param :accessor h-triad-order-param :initform "3" :type integer :documentation "")
    (h-triad-3rd-species-order-param :accessor h-triad-3rd-species-order-param :initform "4" :type integer :documentation "")
    (fifths-order-param :accessor fifths-order-param :initform "7" :type integer :documentation "")
    (octaves-order-param :accessor octaves-order-param :initform "5" :type integer :documentation "")
    (motions-order-param :accessor motions-order-param :initform "12" :type integer :documentation "")
    (direct-move-to-p-cons-order-param :accessor direct-move-to-p-cons-order-param :initform "14" :type integer :documentation "")
    (off-key-order-param :accessor off-key-order-param :initform "8" :type integer :documentation "")
    (m-degrees-order-param :accessor m-degrees-order-param :initform "13" :type integer :documentation "")
    (not-cambiata-order-param :accessor not-cambiata-order-param :initform "11" :type integer :documentation "")
    (m2-eq-zero-order-param :accessor m2-eq-zero-order-param :initform "10" :type integer :documentation "")
    (variety-order-param :accessor variety-order-param :initform "7" :type integer :documentation "")
    (penult-fifth-order-param :accessor penult-fifth-order-param :initform "6" :type integer :documentation "")
    (succ-p-cons-order-param :accessor succ-p-cons-order-param :initform "2" :type integer :documentation "")
    (linear-combination :accessor linear-combination :initform "Linear combination" :type string :documentation "")

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
        (pref-order-panel (om::om-make-view 'om::om-view
            :size (om::om-make-point 1615 220)
            :position (om::om-make-point 5 460)
            :bg-color (om::make-color-255 255 215 180))
        )
        )

        (make-general-params-panel self general-params-panel)
        (make-melodic-params-panel self melodic-params-panel)
        (make-species-params-panel self species-params-panel)
        (make-search-params-panel self search-params-panel)
        (make-search-buttons self search-buttons)
        (make-pref-order-panel self pref-order-panel)
        ; ; add the subviews for the different parts into the main view
        (om::om-add-subviews
            self
            search-buttons
            search-params-panel
            species-params-panel
            general-params-panel
            melodic-params-panel
            pref-order-panel
        )
    )
    ; return the editor
    self
)

(defun make-pref-order-panel (editor panel)
    (om::om-add-subviews
        panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 0 0)
        (om::om-make-point 220 20)
        "Cost importance order of importance"
        :font om::*om-default-font2b*
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 80 30)
        (om::om-make-point 150 20)
        "Ligatures in 4th sp."
        :font om::*om-default-font1b*
        )
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 0 30)
        (om::om-make-point 70 20)
        "Ligatures in 4th species"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (no-syncope-order-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (no-syncope-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

         ;; Add pop-up menus for other cost parameters
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 80 70)
        (om::om-make-point 220 20)
        "Use a lot of harmonic triads"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 0 70)
        (om::om-make-point 70 20)
        "Use a lot of harmonic triads"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (h-triad-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (h-triad-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 80 110)
        (om::om-make-point 200 20)
        "Upbeat h. triad (3rd sp.)"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 0 110)
        (om::om-make-point 70 20)
        "Upbeat h. triad (3rd sp.)"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (h-triad-3rd-species-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (h-triad-3rd-species-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 80 150)
        (om::om-make-point 150 20)
        "No fifths"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 0 150)
        (om::om-make-point 70 20)
        "No fifths"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (fifths-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (fifths-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 380 0)
        (om::om-make-point 150 20)
        "No octaves"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 300 0)
        (om::om-make-point 70 20)
        "No octaves"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (octaves-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (octaves-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 380 40)
        (om::om-make-point 220 20)
        "Motions cost (as defined above)"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 300 40)
        (om::om-make-point 70 20)
        "Motions cost (as defined above)"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (motions-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (motions-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 380 80)
        (om::om-make-point 220 20)
        "No direct motion to p. cons."
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 300 80)
        (om::om-make-point 70 20)
        "No direct motion to p. cons."
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (direct-move-to-p-cons-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (direct-move-to-p-cons-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 380 120)
        (om::om-make-point 150 20)
        "No off-key notes"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 300 120)
        (om::om-make-point 70 20)
        "No off-key notes"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (off-key-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (off-key-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 380 160)
        (om::om-make-point 250 20)
        "Melodic intervals (defined above)"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 300 160)
        (om::om-make-point 70 20)
        "M Degrees cost"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (m-degrees-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (m-degrees-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 680 0)
        (om::om-make-point 150 20)
        "Many cambiatas"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 600 0)
        (om::om-make-point 70 20)
        "Many cambiatas"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (not-cambiata-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (not-cambiata-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 680 40)
        (om::om-make-point 220 20)
        "Diff. notes in down- and upbeat"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 600 40)
        (om::om-make-point 70 20)
        "Diff. notes in down- and upbeat"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (m2-eq-zero-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (m2-eq-zero-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 680 80)
        (om::om-make-point 150 20)
        "A lot of variety"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 600 80)
        (om::om-make-point 70 20)
        "A lot of variety"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (variety-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (variety-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 680 120)
        (om::om-make-point 280 20)
        "Penult. thesis should be a fifth (2nd sp.)"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 600 120)
        (om::om-make-point 70 20)
        "Penult. thesis should be a fifth (2nd sp.)"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (penult-fifth-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (penult-fifth-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 680 160)
        (om::om-make-point 280 20)
        "No successive perfect consonances"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 600 160)
        (om::om-make-point 70 20)
        "No successive perfect consonances"
        :range (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
        :value (succ-p-cons-order-param (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (succ-p-cons-order-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 1000 0)
        (om::om-make-point 400 20)
        "If two costs are ranked the same, perform between them a:"
        :font om::*om-default-font1b*)
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 1000 20)
        (om::om-make-point 280 20)
        "Linear combination"
        :range (list "Linear combination" "Maximum minimisation")
        :value (linear-combination (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (linear-combination (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )
    )
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
        (om::om-make-point 0 0)
        (om::om-make-point 220 20)
        "General Preferences"
        :font om::*om-default-font2b*
        )


        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 190 10)
        (om::om-make-point 250 20)
        "Apply specific penultimate note rules"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-check-box
        (om::om-make-point 170 10)
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
        "Successive p. cons."
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 400)
        (om::om-make-point 200 20)
        "Successive p. cons."
        :range (costs-list t)
        :value (succ-p-cons-cost-param (om::object editor))
        :di-action #'(lambda (cost)
            (setf (succ-p-cons-cost-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
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
        "First voice species"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 50)
        (om::om-make-point 200 20)
        "First voice species"
        :range (list "1st" "2nd" "3rd" "4th" "5th")
        :value (first (species-param (om::object editor)))
        :di-action #'(lambda (cost)
            (setf (first (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 100)
        (om::om-make-point 150 20)
        "First voice range"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 100)
        (om::om-make-point 200 20)
        "Voice range"
        :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
        :value (first (voice-type-param (om::object editor)))
        :di-action #'(lambda (cost)
            (setf (first (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 150)
        (om::om-make-point 150 20)
        "Second voice species"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 150)
        (om::om-make-point 200 20)
        "Second voice species"
        :range (list "None" "1st" "2nd" "3rd" "4th" "5th")
        :value (second (species-param (om::object editor)))
        :di-action #'(lambda (cost)
            (setf (second (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        )

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 200)
        (om::om-make-point 150 20)
        "Second voice range"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 170 200)
        (om::om-make-point 200 20)
        "Second voice range"
        :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
        :value (second (voice-type-param (om::object editor)))
        :di-action #'(lambda (cost)
            (setf (second (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
        )
        ) 

        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 15 250)
        (om::om-make-point 150 20)
        "Minimum % of skips"
        :font om::*om-default-font1b*
        )

        (om::om-make-dialog-item
        'om::om-slider
        (om::om-make-point 170 250)
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
        (om::om-make-point 55 30) ; position (horizontal, vertical)
        (om::om-make-point 160 20) ; size (horizontal, vertical)
        "Save Config"
        :di-action #'(lambda (b)
            (if (null (cf-voice (om::object editor))); if the problem is not initialized
                (error "No voice has been given to the solver. Please set a cantus firmus into the second input and try again.")
            )

            (set-global-cf-variables
                (cf-voice (om::object editor))
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
            (setparam-cost 'succ-p-cons-cost (succ-p-cons-cost-param (om::object editor)))
            ;; set species specific parameters
            (setparam-cost 'penult-sixth-cost (penult-sixth-cost-param (om::object editor)))
            (setparam-cost 'non-cambiata-cost (non-cambiata-cost-param (om::object editor)))
            (setparam-cost 'two-beats-apart-cost (two-beats-apart-cost-param (om::object editor)))
            (setparam 'con-m-after-skip-check (con-m-after-skip-check-param (om::object editor)))
            (setparam-cost 'two-bars-apart-cost (two-bars-apart-cost-param (om::object editor)))
            (setparam-cost 'no-syncopation-cost (no-syncopation-cost-param (om::object editor)))
            (setparam-slider 'pref-species-slider (pref-species-slider-param (om::object editor)))
            ;; set search parameters
            (setparam-slider 'min-skips-slider (min-skips-slider-param (om::object editor)))

            ;; preferences for the cost order
            (defparameter *cost-preferences* (make-hash-table))
            (setf (gethash 'no-syncope-cost *cost-preferences*)               (no-syncope-order-param (om::object editor)))
            (setf (gethash 'h-triad-cost *cost-preferences*)                (h-triad-order-param (om::object editor)))
            (setf (gethash 'h-triad-3rd-species-cost *cost-preferences*)            (h-triad-3rd-species-order-param (om::object editor)))
            (setf (gethash 'fifth-cost *cost-preferences*)                 (fifths-order-param (om::object editor)))
            (setf (gethash 'octave-cost *cost-preferences*)                (octaves-order-param (om::object editor)))
            (setf (gethash 'motions-cost *cost-preferences*)                (motions-order-param (om::object editor)))
            (setf (gethash 'direct-move-to-p-cons-cost *cost-preferences*)  (direct-move-to-p-cons-order-param (om::object editor)))
            (setf (gethash 'off-key-cost *cost-preferences*)                (off-key-order-param (om::object editor)))
            (setf (gethash 'm-degrees-cost *cost-preferences*)              (m-degrees-order-param (om::object editor)))
            (setf (gethash 'not-cambiata-cost *cost-preferences*)           (not-cambiata-order-param (om::object editor)))
            (setf (gethash 'm2-eq-zero-cost *cost-preferences*)             (m2-eq-zero-order-param (om::object editor)))
            (setf (gethash 'variety-cost *cost-preferences*)                (variety-order-param (om::object editor)))
            (setf (gethash 'penult-thesis-cost *cost-preferences*)           (penult-fifth-order-param (om::object editor)))
            (setf (gethash 'succ-p-cons-cost *cost-preferences*)           (succ-p-cons-order-param (om::object editor)))
            (if (string= "Linear combination" (linear-combination (om::object editor))) 
                (setf *linear-combination t)
                (setf *linear-combination nil)
            )

            
            (setf species-integer-list (convert-to-species-integer-list (species-param (om::object editor))))
            (setf *voices-types (convert-to-voice-integer-list (voice-type-param (om::object editor))))
            (setf (current-csp (om::object editor)) (fux-cp species-integer-list))
        )
        )

        (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 55 70) ; position
        (om::om-make-point 160 20) ; size
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
        (om::om-make-point 215 70) ; position
        (om::om-make-point 160 20) ; size
        "Best Solution"
        :di-action #'(lambda (b)
            (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                (error "The problem has not been initialized. Please set the input and press Start.")
            )
            (print "Searching for the best solution")
            ;reset the boolean because we want to continue the search
            (setparam 'is-stopped nil)
            ;get the next solution
            (mp:process-run-function ; start a new thread for the execution of the next method
                "solver-thread" ; name of the thread, not necessary but useful for debugging
                nil ; process initialization keywords, not needed here
                (lambda () ; function to call
                    (let ((check 1) (result nil))
                        (loop while check do
                            (setf result (search-next-fux-cp (current-csp (om::object editor))))
                            (if result (setf (result-voice (om::object editor)) result) (setf check nil))
                        )
                    )
                    ;(om::openeditorframe ; open a voice window displaying the solution
                    ;    (om::omNG-make-new-instance (result-voice (om::object editor)) "Current solution")
                    ;)
                )
            )
        )
        )

        (om::om-make-dialog-item
        'om::om-button
        (om::om-make-point 215 30) ; position (horizontal, vertical)
        (om::om-make-point 160 20) ; size (horizontal, vertical)
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
(defun convert-to-species-integer-list (param-list)
    (let (
        (species-list '())
        )
        (dolist (param param-list) 
        (progn 
            (cond
                ((equal param "1st") (setf species-list (append species-list '(1))))
                ((equal param "2nd") (setf species-list (append species-list '(2))))
                ((equal param "3rd") (setf species-list (append species-list '(3))))
                ((equal param "4th") (setf species-list (append species-list '(4))))
                ((equal param "5th") (setf species-list (append species-list '(5))))
                ((equal param "None") nil)
            )
        ))
        (setq *N-COUNTERPOINTS (length species-list))
        (setq *N-PARTS (+ 1 (length species-list)))
        species-list
    )
)

;; convert the string for the voice type to an integer
;; belong to {"Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below"}
;; convert to {-3 -2 -1 0 1 2 3}
(defun convert-to-voice-integer-list (params)
    (let ((integer-list (make-list *N-COUNTERPOINTS :initial-element nil))) (loop for i from 0 below *N-COUNTERPOINTS do
        (cond
            ((equal (nth i params) "Really far above") (setf (nth i integer-list) 3))
            ((equal (nth i params) "Far above") (setf (nth i integer-list) 2))
            ((equal (nth i params) "Above") (setf (nth i integer-list) 1))
            ((equal (nth i params) "Same range") (setf (nth i integer-list) 0))
            ((equal (nth i params) "Below") (setf (nth i integer-list) -1))
            ((equal (nth i params) "Far below") (setf (nth i integer-list) -2))
            ((equal (nth i params) "Really far below") (setf (nth i integer-list) -3))
        )
    )
    integer-list
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
(defun set-global-cf-variables (cantus-firmus borrow-mode)
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
    ; length of the cantus firmus
    (defparameter *cf-len (length *cf))
    ; *cf-last-index is the number of melodic intervals in the cantus firmus
    (defparameter *cf-last-index (- *cf-len 1))
    ; *cf-penult-index is the number of larger (n -> n+2) melodic intervals in the cantus firmus
    (defparameter *cf-penult-index (- *cf-len 2))
    ; COST_UB is the upper bound of the cost function
    (defparameter COST_UB (* *cf-len 20))
    ; *N-COUNTERPOINTS is the number of counterpoints in the counterpoint
    (defparameter *N-COUNTERPOINTS -1) ; will be defined when parsing the input
)