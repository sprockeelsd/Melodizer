(in-package :fuxcp)

; Author: Thibault Wafflard and Anton Lamotte. Adapted for the migration of FuxCP to C++ by Luc Cleenewerk and Diego de Patoul in August 2024.
; Date: June 3, 2023 and January 2024
; This file contains all the cp-params interface.
; That is to say the interface blocks, as well as the global variables updated via the interface.

;;;====================
;;;= cp-params OBJECT =
;;;====================

(print "Loading cp-params object...")

(defparameter DFS 0)
(defparameter BAB 1)

(om::defclass! cp-params ()
;attributes
(
    ; ---------- Input cantus firmus ----------
    (cf-voice :accessor cf-voice :initarg :cf-voice :initform nil :documentation "")
    ; ---------- Solver parameters ----------
    (species-param :accessor species-param :initform (list "1st" "1st" "1st") :type string :documentation "")
    (voice-type-param :accessor voice-type-param :initform (list "Really far above" "Above" "Below") :type string :documentation "")
    (min-skips-slider-param :accessor min-skips-slider-param :initform 0 :type integer :documentation "")
    (borrow-mode-param :accessor borrow-mode-param :initform "Major" :type string :documentation "")
    ; ---------- Output & Stop ----------
    (current-csp :accessor current-csp :initform nil :documentation "")
    (result-voice :accessor result-voice :initarg :result-voice :initform nil :documentation "")
    ; ---------- Cost order --------------
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
    (make-interface self)
)

(defun make-interface (editor)
    (let* (
        (melodic-subcosts '(
            (:name "Steps" :value "No cost" :cannot-be-forbidden t :param m-step-cost)
            (:name "Third skips" :value "Low cost"  :param m-third-cost)
            (:name "Fourth leaps" :value "Low cost" :param m-fourth-cost)
            (:name "Tritone leaps" :value "Forbidden" :param m-tritone-cost)
            (:name "Fifth leaps" :value "Medium cost" :param m-fifth-cost)
            (:name "Sixth leaps" :value "Medium cost" :param m-sixth-cost)
            (:name "Seventh leaps" :value "Medium cost" :param m-seventh-cost)
            (:name "Octave leaps" :value "Low cost" :param m-octave-cost)
        ))

        (melodic-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "Melodic Preferences" :name "Melodic cost" :display nil :importance "13" :value nil :subcosts ,melodic-subcosts :param m-degrees-cost)
            ;; Add more cost data as needed
        ))

        (motion-subcosts '(
            (:name "Direct motion" :value "Medium cost" :cannot-be-forbidden t :param dir-motion-cost)
            (:name "Oblique motion" :value "Low cost" :param obl-motion-cost)
            (:name "Contrary motion" :value "No cost" :cannot-be-forbidden t :param con-motion-cost)
        ))

        (general-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "General preferences" :name "Borrowed notes" :display nil :importance "8" :value "High cost" :param borrow-cost :cannot-be-forbidden t)            
            (:section "General preferences" :name "Harmonic fifths on the downbeat" :display nil :importance "7" :value "Low cost" :param h-fifth-cost :cannot-be-forbidden t)
            (:section "General preferences" :name "Harmonic octaves on the downbeat" :display nil :importance "5" :value "Low cost" :param h-octave-cost :cannot-be-forbidden t)
            (:section "General preferences" :name "Successive perfect consonances" :display nil :importance "2" :value "Medium cost" :param succ-p-cons-cost :cannot-be-forbidden t)
            (:section "General preferences" :name "Repeating notes" :display nil :importance "9" :value "Medium cost" :param variety-cost) 
            (:section "General preferences" :name "No harmonic triad" :display nil :importance "3" :value "Medium cost" :param h-triad-cost :cannot-be-forbidden t) 
            (:section "General preferences" :name "Direct motion to perf. consonance" :display nil :importance "14" :value "Last resort" :param direct-move-to-p-cons-cost :cannot-be-forbidden t)
            (:section "General preferences" :name "Motion cost" :display nil :importance "12" :value nil :subcosts ,motion-subcosts :param motions-cost)
            (:section "General preferences" :name "Apply specific penultimate note rules" :value "Yes" :special-range ("Yes" "No") :param penult-rule-check)
            
            ;; Add more cost data as needed
        ))

        (specific-preferences `( ; care it is a special apostrophe here (needed to evaluate every value that has a comma in this list, and not to take their symbols)
            (:section "Second species specific pref." :name "Penultimate downbeat note is a fifth" :importance "6" :value "Last resort" :param penult-sixth-cost :cannot-be-forbidden t)
            (:section "Third species specific pref." :name "No cambiatas" :importance "11" :value "High cost" :param non-cambiata-cost :cannot-be-forbidden t)
            (:section "Third species specific pref." :name "Force contrary motion after skip" :value "No" :special-range ("Yes" "No") :param con-m-after-skip-check)
            (:section "Third species specific pref." :name "No hamonic triad in 2nd/3rd beat" :display nil :importance "4" :value "Medium cost" :param h-triad-3rd-species-cost) 
            (:section "Third and fourth species specific pref." :name "Same note in downbeat and upbeat" :importance "10" :value "Low cost" :param m2-eq-zero-cost)
            (:section "Fourth species specific pref." :name "No ligatures" :importance "1" :value "Last resort" :param no-syncopation-cost :cannot-be-forbidden t)
            (:section "Fifth species specific pref." :name "Many quarters (left) or many syncopations (right)" :value 50 :make-slider t :param pref-species-slider)
            ;; Add more cost data as needed
        ))
        )   
        ;; Add the cost table to the main view
        (om::om-add-subviews editor (make-cost-panel editor general-preferences  #|x-offset:|# 0    #|y-offset:|# 0   #|size:|# 540 #|colour:|# om::*azulote*))
        (om::om-add-subviews editor (make-cost-panel editor melodic-preferences  #|x-offset:|# 526  #|y-offset:|# 0   #|size:|# 350 #|colour:|# om::*azulito*))
        (om::om-add-subviews editor (make-cost-panel editor specific-preferences #|x-offset:|# 1052 #|y-offset:|# 0   #|size:|# 500 #|colour:|# (om::make-color-255 230 190 165)))
        (om::om-add-subviews editor (make-explanation-panel editor              #|x-offset:|# 0    #|y-offset:|# 541 #|size:|# 160 #|colour:|# (om::make-color-255 255 240 120)))
        (om::om-add-subviews editor (make-search-params-panel editor      #|x-offset:|# 526  #|y-offset:|# 351 #|size:|# 350 #|colour:|# om::*maq-color*))
        (om::om-add-subviews editor (make-search-buttons      editor      #|x-offset:|# 1052 #|y-offset:|# 501 #|size:|# 200 #|colour:|# om::*workspace-color* melodic-subcosts melodic-preferences motion-subcosts general-preferences specific-preferences))

    )
    
    ;; ... (existing code)
    

    editor ; Return the editor
)

(defun make-explanation-panel (editor panel-x-offset panel-y-offset size colour)
    (let* (
        ;; Explanation text
        (explanation-text "First choose the importance of each preference (1 being the most important and 14 being the least important). The solver will give priority to the most important preferences. The cost value is taken into account if two costs have the same importance. ")

        ;; Create a view for the explanation panel
        (explanation-panel (om::om-make-view 'om::om-view
                                :size (om::om-make-point 525 size)
                                :position (om::om-make-point panel-x-offset panel-y-offset)
                                :bg-color colour))
        
        ;; Create a text element for the explanation
        (explanation-label (om::om-make-dialog-item 'om::om-static-text
                                (om::om-make-point 10 10) (om::om-make-point 500 800)
                                explanation-text
                                ))
        )

    ;; Add the text element to the explanation panel
    (om::om-add-subviews explanation-panel explanation-label)

    (om::om-add-subviews explanation-panel
        (om::om-make-dialog-item
        'om::om-static-text
        (om::om-make-point 10 100)
        (om::om-make-point 400 20)
        "If two costs are ranked the same, perform between them a:"
        )
        (om::om-make-dialog-item
        'om::pop-up-menu
        (om::om-make-point 215 120)
        (om::om-make-point 280 20)
        "Linear combination"
        :range (list "Linear combination" "Maximum minimisation")
        :value (linear-combination (om::object editor))
        :di-action #'(lambda (cost)
                        (setf (linear-combination (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
                        )
        )
    )

    ;; Add the explanation panel to the main view
    explanation-panel
    )
)

(defun make-cost-panel (editor cost-data panel-x-offset panel-y-offset y-size colour)
  (let* (      
         (cost-table (om::om-make-view 'om::om-view
                       :size (om::om-make-point 525 y-size)
                       :position (om::om-make-point panel-x-offset panel-y-offset)
                       :bg-color colour))
         
         (importance-column (om::om-make-dialog-item 'om::om-static-text
                               (om::om-make-point 275 0) (om::om-make-point 150 20) "Importance"
                               :font om::*om-default-font2b*
         ))
         
         (value-column (om::om-make-dialog-item 'om::om-static-text
                           (om::om-make-point 400 0) (om::om-make-point 150 20) "Value"
                           :font om::*om-default-font2b*
         ))
        )

    ;; Add header columns to the cost table
    (om::om-add-subviews cost-table importance-column value-column)
    
    ;; Populate the cost data dynamically
    (let (
        (current-section nil)
        (y-offset 0)
        )
        (loop for index from 0 below (length cost-data)
            do
            (let* ((cost (nth index cost-data))
                    (section (getf cost :section))
                    (y-position (+ y-offset (* 45 index)))
                    (name (if (getf cost :display) (getf cost :display) (getf cost :name)))
                    (importance (getf cost :importance))
                    (value (getf cost :value))
                    (is-new-section (not (string= current-section section)))
                    )

              ;; Add subsection header if it's a new section
                (when is-new-section
                    (let ((section-label (om::om-make-dialog-item 'om::om-static-text
                                        (om::om-make-point 15 y-position) (om::om-make-point 300 20) section
                                        :font om::*om-default-font2b*)))
                    (om::om-add-subviews cost-table section-label))
                    (setf current-section section)
                    (incf y-offset 35)
                    (incf y-position 35)
                )
                ;; Add the row to the cost table
                (let* (
                    (name-label (om::om-make-dialog-item 'om::om-static-text
                                (om::om-make-point 25 y-position) (om::om-make-point 500 20) name))
                    (importance-popup (om::om-make-dialog-item 'om::pop-up-menu
                                        (om::om-make-point 275 (- y-position 7)) (om::om-make-point 70 20)
                                        (format nil "~A" importance)
                                        :value importance
                                        :range (importance-range)
                                        :di-action #'(lambda (x)
                                            (setf (getf cost :importance) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                            (print (getf cost :importance))
                                    )
                                      )
                    )
                    (value-popup (if (getf cost :make-slider)
                        (make-slider cost y-position)
                        (om::om-make-dialog-item 'om::pop-up-menu
                                    (om::om-make-point 345 (- y-position 7)) (om::om-make-point 150 20)
                                    (format nil "~A" value)
                                    :value value
                                    :range (if (getf cost :special-range)
                                        (getf cost :special-range)
                                        (value-range (getf cost :cannot-be-forbidden))
                                    )
                                    :di-action #'(lambda (x)
                                        (setf (getf cost :value) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                        (print (getf cost :value))
                                    )
                                 )
                    ))
                )
                    (cond 
                        ((and value importance) (om::om-add-subviews cost-table name-label importance-popup value-popup))
                        (value (om::om-add-subviews cost-table name-label value-popup))
                        (importance (om::om-add-subviews cost-table name-label importance-popup))
                    )
                ) ; end of row
                (print (getf cost :subcosts))
                (print (length (getf cost :subcosts)))
                (if (getf cost :subcosts)
                    (loop for index from 0 below (length (getf cost :subcosts))
                        do
                        (let*  ((cost (nth index (getf cost :subcosts)))
                                (y-position (+ y-position (* 35 (+ 1 index))))
                                (name (concatenate 'string "|---" (getf cost :name)))
                                (value (getf cost :value))
                                )
                                (incf y-offset 35)
                            ;; Add the row to the cost table
                             (let* (
                                (name-label (om::om-make-dialog-item 'om::om-static-text
                                            (om::om-make-point 50 y-position) (om::om-make-point 250 20) name))
                                (value-popup (om::om-make-dialog-item 'om::pop-up-menu
                                                (om::om-make-point 345 (- y-position 7)) (om::om-make-point 150 20)
                                                (format nil "~A" value)
                                                :value value
                                                :range (value-range (getf cost :cannot-be-forbidden))
                                                :di-action #'(lambda (x)
                                                    (setf (getf cost :value) (nth (om::om-get-selected-item-index x) (om::om-get-item-list x)))
                                                    (print (getf cost :value))
                                                )
                                            )
                                )
                                )
                                (om::om-add-subviews cost-table name-label value-popup)
                            ) ; end of subcost row 
                        ) ; end of subcost
                    ) ; end of subcost loop
                )
            ) ; end of cost
        ) ; end of loop
    )
    cost-table
))

(defun make-search-params-panel (editor panel-x-offset panel-y-offset y-size colour)
    (let* (      
        (search-params-panel (om::om-make-view 'om::om-view
                        :size (om::om-make-point 525 y-size)
                        :position (om::om-make-point panel-x-offset panel-y-offset)
                        :bg-color colour))
    )
        (om::om-add-subviews
            search-params-panel
            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 15 0)
            (om::om-make-point 200 20)
            "Solver Configuration"
            :font om::*om-default-font2b*
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 30)
            (om::om-make-point 150 20)
            "First voice species"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 25)
            (om::om-make-point 220 20)
            "First voice species"
            :range (list "1st" "2nd" "3rd" "4th" "5th")
            :value (first (species-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (first (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 70)
            (om::om-make-point 150 20)
            "First voice range"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 65)
            (om::om-make-point 220 20)
            "Voice range"
            :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
            :value (first (voice-type-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (first (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 110)
            (om::om-make-point 150 20)
            "Second voice species"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 105)
            (om::om-make-point 220 20)
            "Second voice species"
            :range (list "None" "1st" "2nd" "3rd" "4th" "5th")
            :value (second (species-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (second (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 150)
            (om::om-make-point 150 20)
            "Second voice range"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 145)
            (om::om-make-point 220 20)
            "Second voice range"
            :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
            :value (second (voice-type-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (second (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            ) 



            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 190)
            (om::om-make-point 150 20)
            "Third voice species"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 185)
            (om::om-make-point 220 20)
            "Third voice species"
            :range (list "None" "1st" "2nd" "3rd" "4th" "5th")
            :value (third (species-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (third (species-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 230)
            (om::om-make-point 150 20)
            "Third voice range"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 225)
            (om::om-make-point 220 20)
            "Third voice range"
            :range (list "Really far above" "Far above" "Above" "Same range" "Below" "Far below" "Really far below")
            :value (third (voice-type-param (om::object editor)))
            :di-action #'(lambda (cost)
                (setf (third (voice-type-param (om::object editor))) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            ) 


            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 270)
            (om::om-make-point 150 20)
            "Borrowing mode"
            )

            (om::om-make-dialog-item
            'om::pop-up-menu
            (om::om-make-point 275 265)
            (om::om-make-point 220 20)
            "Borrowing mode"
            :range (list "None" "Major" "Minor")
            :value (borrow-mode-param (om::object editor))
            :di-action #'(lambda (cost)
                (setf (borrow-mode-param (om::object editor)) (nth (om::om-get-selected-item-index cost) (om::om-get-item-list cost)))
            )
            )

            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 25 310)
            (om::om-make-point 150 20)
            "Minimum % of skips"
            )

            (om::om-make-dialog-item
            'om::om-slider
            (om::om-make-point 275 305)
            (om::om-make-point 220 20)
            "Minimum % of skips"
            :range '(0 100)
            :increment 1
            :value (min-skips-slider-param (om::object editor))
            :di-action #'(lambda (s)
                (setf (min-skips-slider-param (om::object editor)) (om::om-slider-value s))
            )
            )
        )
        search-params-panel
    )
)

(defun make-search-buttons (editor panel-x-offset panel-y-offset y-size colour melodic-subcosts melodic-preferences motion-subcosts general-preferences specific-preferences)
    (let* (      
        (search-buttons (om::om-make-view 'om::om-view
                        :size (om::om-make-point 525 y-size)
                        :position (om::om-make-point panel-x-offset panel-y-offset)
                        :bg-color colour))
        )
        (om::om-add-subviews
            search-buttons
            (om::om-make-dialog-item
            'om::om-static-text
            (om::om-make-point 187 25)
            (om::om-make-point 150 20)
            "Solver Launcher"
            :font om::*om-default-font3b*
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 97 60) ; position (horizontal, vertical)
            (om::om-make-point 160 20) ; size (horizontal, vertical)
            "Save Config"
            :di-action #'(lambda (b)
                (if (null (cf-voice (om::object editor))); if the problem is not initialized
                    (error "No voice has been given to the solver. Please set a cantus firmus into the second input and try again.")
                )

                (if (not (null (current-csp (om::object editor)))) (delete-solver-pointer (current-csp (om::object editor)))) ; delete the pointer to the current-csp if it is already initialized


                (set-global-cf-variables
                    (cf-voice (om::object editor))
                    (borrow-mode-param (om::object editor))
                )

                (defparameter *is-stopped nil)

                (defvar melodic-params-list nil)
                (defvar melodic-values-list nil)
                (setq melodic-params-list nil)
                (setq melodic-values-list nil)

                (dolist (subcost melodic-subcosts)
                    (setq melodic-params-list (append melodic-params-list (list (getf subcost :param))))
                    (setq melodic-values-list (append melodic-values-list (list (convert-to-cost-integer (getf subcost :value))))))

                ;; (print "melodic lists params then values: ")
                ;; (print melodic-params-list)
                ;; (print melodic-values-list)
                ;; (print (length melodic-values-list))


                (defvar general-params-list nil)
                (defvar general-values-list nil)
                (setq general-params-list nil)
                (setq general-values-list nil)

                (dolist (cost general-preferences)
                    (if (equal (getf cost :param) 'motions-cost)
                        nil ; motions-cost is treated by the subcosts
                        (if (equal (getf cost :param) 'penult-rule-check)
                            (progn ; Use progn to group multiple expressions
                                (setq general-params-list (append general-params-list (list (getf cost :param))))
                                (setq general-values-list (append general-values-list (list (convertparam-yes-no (getf cost :value)))))) ; penult-rule-check is a yes no
                            (progn ; Use progn to group multiple expressions
                                (setq general-params-list (append general-params-list (list (getf cost :param))))
                                (setq general-values-list (append general-values-list (list (convert-to-cost-integer (getf cost :value))))))))) ; else

                ;; (print "general lists params then values: ")
                ;; (print general-params-list)
                ;; (print general-values-list)
                ;; (print (length general-values-list))


                (defvar motion-params-list nil)
                (defvar motion-values-list nil)
                (setq motion-params-list nil)
                (setq motion-values-list nil)

                (dolist (subcost motion-subcosts)
                    (setq motion-params-list (append motion-params-list (list (getf subcost :param))))
                    (setq motion-values-list (append motion-values-list (list (convert-to-cost-integer (getf subcost :value))))))

                ;; (print "motion lists params then values: ")
                ;; (print motion-params-list)
                ;; (print motion-values-list)
                ;; (print (length motion-values-list))


                (defvar specific-params-list nil)
                (defvar specific-values-list nil)
                (setq specific-params-list nil)
                (setq specific-values-list nil)

                (dolist (cost specific-preferences)
                    (if (equal (getf cost :param) 'pref-species-slider)
                        (progn ; Use progn to group multiple expressions
                            (setq specific-params-list (append specific-params-list (list (getf cost :param))))
                            (setq specific-values-list (append specific-values-list (list (getf cost :value))))) ; slider
                        (if (equal (getf cost :param) 'con-m-after-skip-check)
                            (progn ; Use progn to group multiple expressions
                                (setq specific-params-list (append specific-params-list (list (getf cost :param))))
                                (setq specific-values-list (append specific-values-list (list (convertparam-yes-no (getf cost :value)))))) ; con-m-after-skip-check is a yes no
                            (progn ; Use progn to group multiple expressions
                                (setq specific-params-list (append specific-params-list (list (getf cost :param))))
                                (setq specific-values-list (append specific-values-list (list (convert-to-cost-integer (getf cost :value))))))))) ; else

                ;; (print "specific lists params then values: ")
                ;; (print specific-params-list)
                ;; (print specific-values-list)
                ;; (print (length specific-values-list))



                (defvar cost-params-list nil)
                (defvar cost-values-list nil)
                (setq cost-params-list nil)
                (setq cost-values-list nil)

                ;; preferences for the cost order
                (defparameter *cost-preferences* (make-hash-table))
                (dolist (current-list (list general-preferences specific-preferences melodic-preferences))
                    (dolist (cost current-list)
                        (if (getf cost :importance)
                            (progn ; 
                                (setq cost-params-list (append cost-params-list (list (getf cost :name))))
                                (setq cost-values-list (append cost-values-list (list (parse-integer (getf cost :importance))))))
                        )   
                    )
                )

                ;; (print "cost name then values: ")
                ;; (print cost-params-list)
                ;; (print cost-values-list)
                ;; (print (length cost-values-list))


                (setf borrow-mode-int (map-mode-to-int (borrow-mode-param (om::object editor))))
                (setf min-skips-slider (min-skips-slider-param (om::object editor)))

                ;; (print (borrow-mode-param (om::object editor)))
                ;; (print (map-mode-to-int (borrow-mode-param (om::object editor))))
                ;; (print borrow-mode-int)
                ;; (print min-skips-slider)

                (setf species-integer-list (convert-to-species-integer-list (species-param (om::object editor))))
                (setf *voices-types (convert-to-voice-integer-list (voice-type-param (om::object editor))))
                
                ;; (print species-integer-list)
                ;; (print *voices-types)

                (defparameter *species-list species-integer-list)

                ;; (print *scale)

                (print "calling new problem")

                (defparameter problem-pointer (new-ctp-problem *cf species-integer-list *voices-types borrow-mode-int min-skips-slider general-values-list motion-values-list melodic-values-list specific-values-list cost-values-list *tonalite-offset *scale *chromatic-scale *borrowed-scale))
                ;; (print problem-pointer)

                (setf (current-csp (om::object editor)) (create-solver problem-pointer BAB))
                ;; (print (current-csp (om::object editor)))
                
                (print "Base<Problem>* (search engine) stored in current-csp object variable")

                (delete-pointer problem-pointer)

                (print "problem-pointer deleted")

                ;; (setf (current-csp (om::object editor)) (fux-cp species-integer-list))   ; TODO : REPLACE BY CALL TO GECODE
            )
            )

            (om::om-make-dialog-item
            'om::om-button
            (om::om-make-point 97 100) ; position
            (om::om-make-point 160 20) ; size
            "Next Solution"
            :di-action #'(lambda (b)
                (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                    (error "The problem has not been initialized. Please set the input and press Start.")
                )
                (print "Searching for the next solution")
                ;reset the boolean because we want to continue the search
                (setq *is-stopped nil)
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
            (om::om-make-point 262 100) ; position
            (om::om-make-point 160 20) ; size
            "Best Solution"
            :di-action #'(lambda (b)
                (if (typep (current-csp (om::object editor)) 'null); if the problem is not initialized
                    (error "The problem has not been initialized. Please set the input and press Start.")
                )
                (print "Searching for the best solution")
                ;reset the boolean because we want to continue the search
                (setq *is-stopped nil)
                ;get the next solution
                (mp:process-run-function ; start a new thread for the execution of the next method
                    "solver-thread" ; name of the thread, not necessary but useful for debugging
                    nil ; process initialization keywords, not needed here
                    (lambda () ; function to call
                        (let ((check 1) (result nil))
                            (loop while check do
                                (setf result (search-next-fux-cp (current-csp (om::object editor))))
                                (if result (setf (result-voice (om::object editor)) result) (setf check nil))
                                (if *is-stopped (setf check nil))
                            )
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
            (om::om-make-point 262 60) ; position (horizontal, vertical)
            (om::om-make-point 160 20) ; size (horizontal, vertical)
            "Stop"
            :di-action #'(lambda (b)
                ;; (setq *is-stopped t)
                (setf *is-stopped t)
                (print "search stopped. *is-stopped parameter :" )
                (print *is-stopped)
            )
            )
        )
        search-buttons
    )
)




(defun search-next-fux-cp (se)
    (print "Searching next solution...")
    ;; (print l)
    ;; (#<|gil|::bab-engine 40100D21DB> (#<|gil|::int-var 40D038C6BB> #<|gil|::int-var 40D038C9CB> #<|gil|::int-var 40D038CD93> #<|gil|::int-var 40D038D233> #<|gil|::int-var 40D038D71B> #<|gil|::int-var 40D038DC03> #<|gil|::int-var 40D038E0EB> #<|gil|::int-var 40D038E5D3> #<|gil|::int-var 40D038E993> #<|gil|::int-var 40D038C59B> #<|gil|::int-var 40D038CB53> #<|gil|::int-var 40D038C81B> #<|gil|::int-var 40D038CF63> #<|gil|::int-var 40D038CB3B> #<|gil|::int-var 40D038D44B> #<|gil|::int-var 40D038CF4B> #<|gil|::int-var 40D038D933> #<|gil|::int-var 40D038D433> #<|gil|::int-var 40D038DE1B> #<|gil|::int-var 40D038D91B> #<|gil|::int-var 40D038E303> #<|gil|::int-var 40D038DE03> #<|gil|::int-var 40D038E753> #<|gil|::int-var 40D038E2EB> #<|gil|::int-var 40D038EAB3> #<|gil|::int-var 40D03974D3>) #<|gil|::time-stop 40100D16A3> #<|gil|::search-options 40100D1BC3> (1 2))
    (let (
        (check t)
        sol sol-pitches sol-species
        )

        (time (om::while check :do
            (print "search next loop")
            ;; (print *is-stopped)
            ;; (print (getparam 'is-stopped))
            ;; ; reset the tstop timer before launching the search
            ;; (gil::time-stop-reset tstop)                             ; Done on Gecode side
            ; try to find a solution
            (time (setq sol (try-find-solution se)))    ; "sol" will contain a void* cast of a Problem* pointer (the solution space)
            (if (null sol)
                ; then check if there are solutions left and if the user wishes to continue searching
                ;; (stopped-or-ended (search-stopped se) (getparam 'is-stopped))   ; TODO check git damien
                (stopped-or-ended (search-stopped se) *is-stopped)   ; TODO check git damien
                ; else we have found a solution so break fthe loop
                (setf check nil)
            )
        ))

        (print sol)
        
                
        (print "The solution can now be retrieved by evaluating the third output of cp-params.")
        ;; (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (setq sol-pitches (solution-to-int-array sol)) ; store the values of the solution
        ;; (print "sol-pitches :")
        ;; (print sol-pitches)
        (let (
            (basic-rythmics (get-basic-rythmics *species-list *cf-len sol-pitches sol))
            (sol-voices (make-list *N-COUNTERPOINTS :initial-element nil))
            )

            (loop for i from 0 below *N-COUNTERPOINTS do (progn
                (setq rythmic+pitches (nth i basic-rythmics)) ; get the rythmic correpsonding to the species
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
            )

                (setf (nth i sol-voices) (make-instance 'voice :chords (to-midicent pitches-om) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo))
            )
            (make-instance 'poly :voices sol-voices)
        )
    )
)

; try to find a solution, catch errors from Gecode
(defun try-find-solution (se)
    (print "try-find-solution")
    (handler-case
        (return-next-solution-space se) ; search the next solution, sol is the space of the solution
        (error (c)
            (error "Error on Gecode side when calling return-next-solution-space. Wait a few seconds, then press stop, then next solution again. If that doesn't work, press save config and restart the search.")
            ;(try-find-solution se)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user)
    (print (list "stopped-se" stopped-se "stop-user" stop-user))
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "The search was stopped because no more solution was found. Either the best solution was found or none exist.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search was stopped. Press next to continue the search.")
    )
)


; get the basic rythmic pattern and the corresponding notes the given species
; - species-list: the species [1 2 3 4]
; - len: the length of the cantus-firmus
; - sol-pitches: the whole solution array
; - sol: the solutino space (only useful for the 5th species)
; return format = '('(rythmics-1 pitches-1) '(rythmics-2 pitches-2) ... '(rythmics-n pitches-n))
; examples:
; ((1) 5) -> (((1 1 1 1 1) (60 62 64 65 60)))
; ((1 2) 5) -> (((1 1 1 1 1) (60 62 64 65 60)) ((1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1) (60 62 64 65 64 62 60 62 60)))
; ((2) 5) -> ((1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1 (pitches))
; ((3) 5) -> ((1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1 (pitches))
; ((4) 5) -> ~((-1/2 1 1 1 1/2 1/2 1 (pitches)) depending on the counterpoint

(defun get-basic-rythmics (species-list len sol-pitches sol)
    ;; (print "get-basic-rythmics function entered")
    (setq len-1 (- len 1))
    (setq len-2 (- len 2))
    (let (
        (rythmic+pitches (make-list *N-COUNTERPOINTS :initial-element nil))
        )
        (loop for i from 0 below *N-COUNTERPOINTS do (progn
            (case (nth i species-list)
                (1 (progn 
                    (setf (nth i rythmic+pitches) (list
                        ; rythm
                        (make-list len :initial-element 1)
                        ; pitches
                        (subseq sol-pitches 0 len)
                        
                    ))
                    (setf sol-pitches (subseq sol-pitches len))
                ))
                (2 (let (
                        (rythmic (append (make-list (* 2 len-1) :initial-element 1/2) '(1)))
                        (pitches (subseq sol-pitches 0 (- (* 2 len) 1)))
                        )
                        (if (eq (car (last pitches 4)) (car (last pitches 3))) (progn ; if the first note in the penult bar is the same as the last in the 2nd-to last
                            ; then ligature them 
                            (setf rythmic (append (butlast rythmic 4) '(1) (last rythmic 2)))
                            (loop
                                for i from (- (length pitches) 4) below (- (length pitches) 1)
                                do (setf (nth i pitches) (nth (+ i 1) pitches))
                            )
                        ))
                        (if (eq (car (last pitches 3)) (car (last pitches 2))) (progn ; same but for 3rd-to-last and 2nd-to-last
                            (setf rythmic (append (butlast rythmic 3) '(1) (last rythmic 1)))
                            (loop
                                for i from (- (length pitches) 3) below (- (length pitches) 1)
                                do (setf (nth i pitches) (nth (+ i 1) pitches))
                            )
                        ))
                        (setf (nth i rythmic+pitches) (list
                            rythmic
                            pitches
                        ))
                        ; remove all the notes we've just considered from sol-pitches
                        (setf sol-pitches (subseq sol-pitches (length pitches)))
                    )                    
                )
                (3 (progn
                    (setf (nth i rythmic+pitches) (list 
                        ; rhythm
                        (append (make-list (* 4 len-1) :initial-element 1/4) '(1))
                        ; pitches
                        (subseq sol-pitches 0 (- (* 4 len) 3))
                    ))
                    ; remove all the notes we've just considered from sol-pitches
                    (setf sol-pitches (subseq sol-pitches (- (* 4 len) 3)))
                ))
                (4 ;(error "The fourth species is not implemented yet in get-basic-rythmics function.")
                (progn 
                    (setf (nth i rythmic+pitches) (build-rythmic-pattern
                        (get-4th-species-array len-2)
                        (get-4th-notes-array (subseq sol-pitches 0 (* 2 len-1)) (+ (* 4 len-1) 1))
                    ))
                    ;; (setf j 0)
                    ;; (dotimes (k *cf-penult-index)
                    ;;     (setf j (+ j 2))
                    ;;     ; if we have a note that creates a hidden fifth (direct motion to a fifth), then remove the note
                    ;;     (if (and
                    ;;         (eq 7 (nth (+ 1 j) (gil::g-values sol (first (h-intervals (nth i counterpoints))))))
                    ;;         (eq DIRECT (nth j (gil::g-values sol (first (motions (nth i counterpoints))))))
                    ;;         )
                    ;;         (setf (nth j (first (nth i rythmic+pitches))) -1/2)
                    ;;     )
                    ;; )
                    (setf sol-pitches (subseq sol-pitches (* 2 len-1)))
                )
                )
                (5 ;; (error "The fifth species is not implemented yet in get-basic-rythmics function.")
                (let (
                        ;; (sol-species (gil::g-values sol (species-arr (nth i counterpoints)))) ;; store the values of the solution
                        (sol-species (species-array-to-int-array sol i len)) ;; get the solution array from Gecode
                        (ext-cp-dom (ext-domain-to-int-array sol i)) ;; get the extended cp domain from Gecode
                    )                    
                    (setf (nth i rythmic+pitches) 
                        (parse-species-to-om-rythmic sol-species sol-pitches ext-cp-dom)
                    )
                    ;; (setf sol-pitches (subseq sol-pitches (solution-len (nth i counterpoints))))
                    (setf sol-pitches (subseq sol-pitches (- (* len 4) 3)))
                )
                )
            )
        ))
        (assert (eql sol-pitches nil) (sol-pitches) "Assertion failed: sol-pitches should be nil at the end of function get-basic-rythmics.")
        rythmic+pitches
    )
)


(defun make-slider (cost y-position)
    (om::om-make-dialog-item
    'om::om-slider
    (om::om-make-point 350 (- y-position 3))
    (om::om-make-point 150 20)
    "5th: Preference to a lot of quarters [left] OR a lot of syncopations [right]"
    :range '(0 100)
    :increment 1
    :value (getf cost :value)
    :di-action #'(lambda (s)
        (setf (getf cost :value) (om::om-slider-value s))
                                        (print (getf cost :value))
    )
    )
)

; return the list of available costs for the preferences
; @is-required: if true, "Forbidden" is removed
(defun value-range (&optional (is-required nil))
    (let (
        (costs (list "No cost" "Low cost" "Medium cost" "High cost" "Last resort" "Cost prop. to length" "Forbidden"))
    )
        (if is-required
            (butlast costs)
            costs
        )
    )
)

(defun importance-range ()
    (mapcar #'(lambda (x) (format nil "~A" x)) (loop for i from 1 to 14 collect i))
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
    ;; (print param-list)
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

; return the tone offset of the voice
; => [0, ...,  11]
; 0 = C, 1 = C#, 2 = D, 3 = D#, 4 = E, 5 = F, 6 = F#, 7 = G, 8 = G#, 9 = A, 10 = A#, 11 = B
(defun get-tone-offset (voice)
    (let (
        (tone (om::tonalite voice))
    )
        (if (eq tone nil)
            ; then default to C major
            0
            ; else check if the mode is major or minor
            (let (
                (mode (om::mode tone))
            )
                (if (eq (third mode) 300)
                    (midicent-to-midi-offset (+ (om::tonmidi tone) 300))
                    (midicent-to-midi-offset (om::tonmidi tone))
                )
            )
        )
    )
)


; build the list of acceptable pitch based on the scale and a key offset
(defun build-scaleset (scale offset)
    (let ((major-modified (adapt-scale scale))
          (scaleset (list)))
        (loop :for octave :from -1 :below 11 :by 1 append
              (setq scaleset (nconc scaleset (mapcar (lambda (n) (+ (+ n (* octave 12)) offset)) major-modified)))
        )
        (setq scaleset (remove-if 'minusp scaleset))
        ;; tibo: remove notes higher than 127
        (setq scaleset (remove 127 scaleset :test #'<))
    )
)

; reformat a scale to be a canvas of pitch and not intervals
(defun adapt-scale (scale)
    (let ((major-modified (list (first scale))))
         (loop :for i :from 1 :below (length scale) :by 1 :do
            (setq major-modified (nconc major-modified (list (+ (nth i scale) (nth (- i 1) major-modified)))))
         )
    (return-from adapt-scale major-modified)
    )
)

; returns the list of intervals defining a given mode
(defun get-scale (&optional (mode "ionian (major)"))
    (cond
        ((string-equal mode "ionian (major)")
            (list 2 2 1 2 2 2 1)
        )
        ((string-equal mode "dorian")
            (list 2 1 2 2 2 1 2)
        )
        ((string-equal mode "phrygian")
            (list 1 2 2 2 1 2 2)
        )
        ((string-equal mode "lydian")
            (list 2 2 2 1 2 2 1)
        )
        ((string-equal mode "mixolydian")
            (list 2 2 1 2 2 1 2)
        )
        ((string-equal mode "aeolian (natural minor)")
            (list 2 1 2 2 1 2 2)
        )
        ((string-equal mode "locrian")
            (list 1 2 2 1 2 2 2)
        )
        ((string-equal mode "harmonic minor")
            (list 2 1 2 2 1 3 1)
        )
        ((string-equal mode "pentatonic")
            (list 2 2 3 2 3)
        )
        ((string-equal mode "chromatic")
            (list 1 1 1 1 1 1 1 1 1 1 1 1)
        )
        ((string-equal mode "borrowed")
            (list 5 4 2 1)
        )
    )
)

; <chords> a list of chord object
; Return the list of pitch contained in chords in midi format
(defun to-pitch-list (chords)
     (loop :for n :from 0 :below (length chords) :by 1 collect (to-midi (om::lmidic (nth n chords))))
)

; convert from MIDIcent to MIDI
(defun to-midi (l)
    (if (null l)
        nil
        (cons (/ (first l) 100) (to-midi (rest l)))
    )
)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
    (if (null l)
        nil
        (cons (* 100 (first l)) (to-midicent (rest l)))
    )
)


(defun map-mode-to-int (mode)
  (cond
    ((equal mode "None") 0)
    ((equal mode "Major") 1)
    ((equal mode "Minor") 2)
    )
)


; TODO : check
(defun convertparam-yes-no (v)
    (cond
    ((equal v "Yes") 1)
    ((equal v "No")  0)
    )
)





; 4SP AND 5SP SPECIFIC 

; build the rythmic pattern for open music from the species array
; - species-arr: array of integer for species
; - cp-arr: array of integer for counterpoint notes
; - rythmic-arr: array of integer for the rythmic (supposed to be nil and then filled by the recursive function)
; - notes-arr: array of interger for notes (supposed to be nil and then filled by the recursive function)
; - b-debug: boolean to print debug info
; note: this function has noting to do with GECODE
(defun build-rythmic-pattern (species-arr cp-arr &optional (rythmic-arr nil) (notes-arr nil) (extended-cp-domain nil) (b-debug nil))
    ; print debug info
    (if b-debug
        (progn
        (print "Current species and notes:")
        (print species-arr)
        (print cp-arr)
        (print "Current answer:")
        (print rythmic-arr)
        (print notes-arr)
        )
    )
    ; base case
    (if (null species-arr)
        ; then return the rythmic pattern
        (list rythmic-arr notes-arr)
    )

    (let (
        (sn (first species-arr)) ; current species
        (sn+1 (second species-arr)) ; next species
        (sn+2 (third species-arr)) ; next next species
        (sn+3 (fourth species-arr)) ; next next next species
        (cn (first cp-arr)) ; current counterpoint note
        (cn+1 (second cp-arr)) ; next counterpoint note
        (cn+2 (third cp-arr)) ; next next counterpoint note
        (cn+3 (fourth cp-arr)) ; next next next counterpoint note
    )
        ; replace all nil by -1 for the species
        (if (null sn) (setf sn -1))
        (if (null sn+1) (setf sn+1 -1))
        (if (null sn+2) (setf sn+2 -1))
        (if (null sn+3) (setf sn+3 -1))
        ; replace all nil by -1 for the counterpoint
        (if (null cn) (setf cn -1))
        (if (null cn+1) (setf cn+1 -1))
        (if (null cn+2) (setf cn+2 -1))
        (if (null cn+3) (setf cn+3 -1))

        (if b-debug
            (progn
            (print (format nil "sn: ~a, sn+1: ~a, sn+2: ~a, sn+3: ~a" sn sn+1 sn+2 sn+3))
            (print (format nil "cn: ~a, cn+1: ~a, cn+2: ~a, cn+3: ~a" cn cn+1 cn+2 cn+3))
            )
        )

        (cond
            ; 1 if it is the last note [1 -1 ...]
            ((and (eq sn 1) (eq sn+1 -1))
                (list (append rythmic-arr (list 1)) (append notes-arr (list cn)))
            )

            ; if [4 0 4 ...] -> which syncope ?
            ((and (eq sn 4) (eq sn+1 0) (eq sn+2 4))
            (if (/= cn cn+2) ; syncopation but different notes ?
                ; then same as half note
                (if (eq sn+3 3)
                    ; then 1/2 + 1/4 if [4 0 4 3] (syncopation catch up by a quarter note)
                    (build-rythmic-pattern
                        (nthcdr 3 species-arr)
                        (nthcdr 3 cp-arr)
                        (append rythmic-arr (list 1/2 1/4))
                        (append notes-arr (list cn cn+2))
                        extended-cp-domain
                    )
                    ; else 1/2 + 1/2 if [4 0 4 0] (basic syncopation)
                    (build-rythmic-pattern
                        (nthcdr 4 species-arr)
                        (nthcdr 4 cp-arr)
                        (append rythmic-arr (list 1/2 1/2))
                        (append notes-arr (list cn cn+2))
                        extended-cp-domain
                    )
                )
                ; else same as full note syncopated
                (if (eq sn+3 3)
                    ; then 3/4 if [4 0 4 3] (syncopation catch up by a quarter note)
                    (build-rythmic-pattern
                        (nthcdr 3 species-arr)
                        (nthcdr 3 cp-arr)
                        (append rythmic-arr (list 3/4))
                        (append notes-arr (list cn))
                        extended-cp-domain
                    )
                    ; else 1 if [4 0 4 0] (basic syncopation)
                    (build-rythmic-pattern
                        (nthcdr 4 species-arr)
                        (nthcdr 4 cp-arr)
                        (append rythmic-arr (list 1))
                        (append notes-arr (list cn))
                        extended-cp-domain
                    )
                )
            ))

            ; 1/8 note (croche) if cn == cn+1 AND [!0 (3 or 4) ...]
            ((and (eq cn cn+1) (/= sn 0) (or (eq sn+1 3) (eq sn+1 4)))
                (if (>= (lastone notes-arr) cn)
                    ; then eighth note with the next lower note
                    (build-rythmic-pattern
                        (nthcdr 1 species-arr)
                        (nthcdr 1 cp-arr)
                        (append rythmic-arr (list 1/8 1/8))
                        (append notes-arr (list cn (find-next-note cn 'lower extended-cp-domain)))
                        extended-cp-domain
                    )
                    ; else eighth note with the next higher note
                    (build-rythmic-pattern
                        (nthcdr 1 species-arr)
                        (nthcdr 1 cp-arr)
                        (append rythmic-arr (list 1/8 1/8))
                        (append notes-arr (list cn (find-next-note cn 'higher extended-cp-domain)))
                        extended-cp-domain
                    )
                )
            )

            ; silence if [0 0 ...]
            ((and (eq sn 0) (eq sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list -1/2))
                    notes-arr
                    extended-cp-domain
                )
            )

            ; 1 if [1 0 0 0] (full note)
            ((and (eq sn 1) (eq sn+1 0) (eq sn+2 0) (eq sn+3 0))
                (build-rythmic-pattern
                    (nthcdr 4 species-arr)
                    (nthcdr 4 cp-arr)
                    (append rythmic-arr (list 1))
                    (append notes-arr (list cn))
                    extended-cp-domain
                )
            )
            
            ; 1/2 if [2 0 ...] (half note)
            ((and (eq sn 2) (eq sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list 1/2))
                    (append notes-arr (list cn))
                    extended-cp-domain
                )
            )

            ; 1/4 if [3 ...] (quarter note)
            ((eq sn 3)
                (build-rythmic-pattern
                    (nthcdr 1 species-arr)
                    (nthcdr 1 cp-arr)
                    (append rythmic-arr (list 1/4))
                    (append notes-arr (list cn))
                    extended-cp-domain
                )
            )

            ; 1/2 if [4 0 1 ...] (penultimate note for the 4th species)
            ((and (eq sn 4) (eq sn+1 0) (eq sn+2 1))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list 1/2))
                    (append notes-arr (list cn))
                    extended-cp-domain
                )
            )
        )
    )
)


; return a species array for a 4th species counterpoint
; - len-2: the length of the counterpoint - 2
(defun get-4th-species-array (len-2)
    (append (list 0 0) (get-n-4040 len-2) (list 4 0 1))
)

; return a note array for a 4th species counterpoint
; - len: the length of the cantus firmus
(defun get-4th-notes-array (cp len)
    (let* (
        (notes (make-list len :initial-element 0)) ; notes that we don't care about can be 0
    )
        (loop
        for n from 2 below len by 2 ; we move from 4 to 4 (4 0 4 ...) after the silence (0 0) at the start
        for p in cp
        do
            (setf (nth n notes) p)
        )
        notes
    )
)

; return a list with n * (4 0 4 0), used to build the rythmic pattern for the 4th species
; - n: the number of times the pattern is repeated
(defun get-n-4040 (n)
    (if (eq n 0)
        nil
        (append (list 4 0 4 0) (get-n-4040 (- n 1)))
    )
)



; parse the species array to get the corresponding rythmic pattern for open music
; - species-arr: array of integer for species (returned by the next-solution algorithm)
; - cp-arr: array of integer for counterpoint notes (returned by the next-solution algorithm)
; note: this function has noting to do with GECODE
(defun parse-species-to-om-rythmic (species-arr cp-arr extended-cp-domain)
    ; replace the last element of the species array by 1
    (setf (first (last species-arr)) 1)
    (build-rythmic-pattern species-arr cp-arr nil nil extended-cp-domain)
)




; return the last element of a list
(defun lastone (l)
    (first (last l))
)

; return the rest of a list without its last element
(defun restbutlast (l)
    (butlast (rest l))
)

; return the penultimate element of a list
(defun penult (l)
    (nth (- (length l) 2) l)
)


; find the next @type note in the borrowed scale,
; if there is no note in the range then return the note of the other @type
; - note: integer for the current note
; - type: atom [lower | higher] for the type of note to find
; note: this function has noting to do with GECODE
(defun find-next-note (note type extended-cp-domain)
    (let (
        ; first sort the scale corresponding to the type
        (sorted-scale (if (eq type 'lower)
            (sort extended-cp-domain #'>)
            (sort extended-cp-domain #'<)
        ))
        )
        (if (eq type 'lower)
            ; then we search the first note in the sorted scale that is lower than the current note
            (progn
                (loop for n in sorted-scale do
                    (if (< n note) (return-from find-next-note n))
                )
                ; no note so we return the penultimate element of the sorted scale
                (penult sorted-scale)
            )
            ; else we search the first note in the sorted scale that is higher than the current note
            (progn
                (loop for n in sorted-scale do
                    (if (> n note) (return-from find-next-note n))
                )
                ; no note so we return the penultimate element of the sorted scale
                (penult sorted-scale)
            )
        )
    )
)
