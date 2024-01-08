(in-package :fuxcp)

; Author: Anton Lamotte
; Date: January 2024
; This file contains the function that dispatches the counterpoints to thei respective functions and adds all the necessary constraints for having 3 voices species.

;;===================================#
;; Three voices counterpoint handler #
;;===================================#
(defun fux-cp-3v (species-list parts)
    (print "######### 3 VOICES ##########")

    (setf cantus-firmus (first parts))
    (setf counterpoint-1 (second parts))
    (setf counterpoint-2 (third parts))

    ;================================================================================;
    ;                   APPLYING THE PART-SPECIFIC RULES                             ;
    ;================================================================================;
    ; for each part
    (dotimes (i *N-PARTS)
        (case (species (nth i parts))
            (0 (fux-cp-cf  (nth i parts))) ; dispatch to the cantus firmus function
            (1 (fux-cp-1st (nth i parts) 3v-1sp)) ; dispatch to the first species function
            (2 (fux-cp-2nd (nth i parts) 3v-2sp)) ; dispatch to the second species function
            (3 (fux-cp-3rd (nth i parts) 3v-3sp)) ; dispatch to the third species function
            (4 (fux-cp-4th (nth i parts) 3v-4sp)) ; dispatch to the fourth species function
            (5 (fux-cp-5th (nth i parts) 3v-5sp)) ; dispatch to the fifth species function
            (otherwise (error "Unexpected value in the species list, when calling fux-cp-3v."))
        )
    )
    

    ;================================================================================;
    ;                 CREATING SOME ADDITIONAL VARIABLES                             ;
    ;================================================================================;
    (setf solution-array (append (solution-array counterpoint-1) (solution-array counterpoint-2))) ; the final array with both counterpoints

    (dotimes (i *N-COUNTERPOINTS)
        (create-h-intervals (first (notes (nth i *upper))) (first (notes *lowest)) (first (h-intervals (nth i *upper))))
        (setf (h-intervals-abs (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (setf (h-intervals-brut (nth i *upper)) (gil::add-int-var-array *sp* *cf-len -127 127))
        (create-intervals (first (notes *lowest)) (first (notes (nth i *upper))) (h-intervals-abs (nth i *upper)) (h-intervals-brut (nth i *upper)))
    )

    ;================================================================================;
    ;                                CONSTRAINTS                                     ;
    ;================================================================================;
    (loop 
        ; for each possible pair or parts
        ; for example if we have (cf, cp1 and c2), take (cf and cp1), (cf and cp2) and (cp1 and cp2)
        for v1 in parts 
        for i from 0 
        do (loop for v2 in (nthcdr (1+ i) parts) 
        do (progn 
            ; no unison between the voices
            (print "No unison between the voices")
            (dotimes (i 4) (if (eq i 0)
                ; first beat can be the same on first and last measure
                (add-no-unison-cst (nth i (notes v1)) (nth i (notes v2)))
                ; other beats must always be different
                (add-no-unison-at-all-cst (nth i (notes v1)) (nth i (notes v2)))
            ))
        ))
    )

    ; it is not allowed to have two direct motions
    (print "No together move")
    (add-no-together-move-cst (list (first (motions counterpoint-1)) (first (motions counterpoint-2)) (first (motions cantus-firmus))))

    (print "Last chord cannot be minor")
    (dotimes (i *N-COUNTERPOINTS)
        (add-no-minor-third-cst (lastone (first (h-intervals (nth i *upper)))))
    )
    
    (print "Last chord cannot include a tenth")
    (dotimes (i *N-COUNTERPOINTS)
        (add-no-tenth-in-last-chord-cst (first (h-intervals (nth i *upper))) (h-intervals-brut (nth i *upper)))
    )

    (print "Last chord must be a harmonic triad") 
    (add-last-chord-h-triad-cst (first (h-intervals (first *upper))) (first (h-intervals (second *upper))))

    (print "The last lowest note must be the same as the root note of the key")
    (last-lowest-note-same-as-root-note-cst)

    ; two fifth species counterpoints only
    (if (equal species-list '(5 5)) (progn
        (print "The rhythms of the two fifth-species counterpoints must be as different as possible")
        (add-make-fifth-species-different-cst parts)
    ))

    ;================================================================================;
    ;                                    COSTS                                       ;
    ;================================================================================;
    ; Cost #1 : no successive perfect consonances
    (setf succ-p-cons-cost (gil::add-int-var-array-dom *sp* (* 3 *cf-last-index) (append '(0) (getparam-val 'succ-p-cons-cost))))
    (setf succ-p-cons-cost-index 0)
    (loop 
        ; for each possible pair or parts
        ; for example if we have (cf, cp1 and c2), take (cf and cp1), (cf and cp2) and (cp1 and cp2)
        for v1 in parts 
        for i from 0 
        do (loop for v2 in (nthcdr (1+ i) parts) 
        do (progn 
            (print "As few successive perfect consonances as possible")
            (let (
                (h-intervals-1-2 (gil::add-int-var-array *sp* *cf-len 0 11)) ; the h-intervals between p1 and p2
                (is-p-cons-arr-1-2 (gil::add-bool-var-array *sp* *cf-len 0 1)) ; the is h-intervals-1-2 a perfect consonance
                (current-cost (subseq succ-p-cons-cost succ-p-cons-cost-index)) ; succ-p-cons-cost is a long array of size 3m, each slice of m being dedicated for the costs between a pair of parts
                )
                (incf succ-p-cons-cost-index *cf-last-index) ; set the index to the next slice

                (if (member 4 (list (species v1) (species v2)))
                    (progn ; first case, we have a fourth species counterpoint in the composition
                        (if (eq (species v1) 4)
                            (if (eq (species v2) 4) 
                                ; both are of fourth species, compute using the third beat for each
                                (create-h-intervals (third (notes v1)) (third (notes v2)) h-intervals-1-2)
                                ; only the first is of fourth species, compute using the third beat for it
                                (create-h-intervals (third (notes v1)) (first (notes v2)) h-intervals-1-2)
                            )
                            ; only the second is of fourth species, compute using the third beat for it
                            (create-h-intervals (first (notes v1)) (third (notes v2)) h-intervals-1-2)
                        )
                        ; if one voice is of the fourth species the last chord was not created yet, due to the delaying of the fourth species
                        (create-h-intervals (last (first (notes v1))) (last (first (notes v2))) (last h-intervals-1-2)) 
                    )
                    (progn ; "normal" case: compute using the first beat for all the parts
                        (create-h-intervals (first (notes v1)) (first (notes v2)) h-intervals-1-2)
                    )
                ) 

                (create-is-p-cons-arr h-intervals-1-2 is-p-cons-arr-1-2)
                (cond 
                    ((and (/= 2 (species v1)) (/= 2 (species v2)) (/= 4 (species v1)) (/= 4 (species v2))) ; if both voices are not from the 2nd nor from the 4th species
                        (add-no-successive-p-cons-cst is-p-cons-arr-1-2 current-cost) ; for all species except the fourth and the second, successive perfect consonances are avoided
                    )
                    ((= 2 (species v1))
                        (add-no-successive-p-cons-2nd-species-cst is-p-cons-arr-1-2 h-intervals-1-2 (first (m-succ-intervals v1)) current-cost) ; for the second species, successive fifths are allowed if there is a third in between
                    )
                    ((= 2 (species v2))
                        (add-no-successive-p-cons-2nd-species-cst is-p-cons-arr-1-2 h-intervals-1-2 (first (m-succ-intervals v2)) current-cost) ; for the second species, successive fifths are allowed if there is a third in between
                    )
                    ((or (eq 4 (species v1)) (eq 4 (species v2)))
                        (add-no-successive-p-cons-4th-species-cst is-p-cons-arr-1-2 h-intervals-1-2 current-cost) ; for the fourth species, successive fifths are allowed, but no other successive perfect consonances
                    ) 
                )
            )
        )
    ))
    (add-cost-to-factors succ-p-cons-cost 'succ-p-cons-cost)

    (dolist (part parts) (progn
        (print "As few direct motion to reach a perfect consonance as possible")
        ; Cost #2: as few direct motion to reach a perfect consonance as possible
        (if (eq (species part) 4)
            nil ; if species=4 then pass, this cost doesn't apply to 4th species
            ; else apply the cost:
            (let ((direct-move-to-p-cons-cost (gil::add-int-var-array-dom *sp* *cf-last-index (append '(0) (getparam-val 'direct-move-to-p-cons-cost)))))
                (case (species part)
                    ; the direct motion must be computed from the last note in the measure
                    ((0 1) (compute-no-direct-move-to-p-cons-costs-cst (first (motions part)) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (2 (compute-no-direct-move-to-p-cons-costs-cst (real-motions part) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (3 (compute-no-direct-move-to-p-cons-costs-cst (fourth (motions part)) direct-move-to-p-cons-cost (is-p-cons-arr part)))
                    (5 (compute-no-direct-move-to-p-cons-costs-cst 
                        (fourth (motions part)) direct-move-to-p-cons-cost (collect-bot-array (is-p-cons-arr part) (fourth (is-3rd-species-arr part))) nil
                    ))
                )
                (add-cost-to-factors direct-move-to-p-cons-cost 'direct-move-to-p-cons-cost)
            )
        )
        
        ; Cost #3: as many different notes as possible
        (print "As many different notes as possible")
        (if (eq (species part) 0)
            nil ; this cost has no sense for the cantus firmus as its notes are already fixed
            (let ( ; for all the counterpoints 
                (variety-cost (gil::add-int-var-array-dom *sp* (* 3 (- (length (first (notes part))) 2)) (append '(0)(getparam-val 'variety-cost))))
                )
                (compute-variety-cost (first (notes part)) variety-cost)
                (add-cost-to-factors variety-cost 'variety-cost)
            )
        )
    ))

    ; Cost #4
    (print "Prefer the use of the harmonic triad") 
    (if (member 4 species-list)
        (progn ; first case, we have a fourth species counterpoint in the composition
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-last-index (append '(0) (getparam-val 'h-triad-cost)))) ; length of the cost is m-1 because there is m-1 notes on the third beat
            (if (eq (species counterpoint-1) 4)
                (if (eq (species counterpoint-2) 4) 
                    ; both are of fourth species, compute using the third beat for each
                    (compute-h-triad-cost (third (h-intervals counterpoint-1)) (third (h-intervals counterpoint-2)) h-triad-cost)
                    ; only the first is of fourth species, compute using the third beat for it
                    (compute-h-triad-cost (third (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) h-triad-cost)
                )
                ; only the second is of fourth species, compute using the third beat for it
                (compute-h-triad-cost (first (h-intervals counterpoint-1)) (third (h-intervals counterpoint-2)) h-triad-cost)
            )
        )
        (progn ; "normal" case: compute using the first beat for all the parts
            (setq h-triad-cost (gil::add-int-var-array-dom *sp* *cf-len (append '(0) (getparam-val 'h-triad-cost)))) ; length is m as usual
            (compute-h-triad-cost (first (h-intervals counterpoint-1)) (first (h-intervals counterpoint-2)) h-triad-cost)
        )
    )
    (add-cost-to-factors h-triad-cost 'h-triad-cost)

    (dotimes (i *N-PARTS)
        ; Cost #5, only for 3rd species: if harmonic triad isn't achieved on the downbeat, it shall be on the second or third one
        (if (or (eq (species (nth i parts)) 3) (eq (species (nth i parts)) 5))  (let
            (
                (h-triad-3rd-species-cost (gil::add-int-var-array-dom *sp* (* *cf-last-index 2) (append '(0) (getparam-val 'h-triad-3rd-species-cost))))
            )
            (dotimes (j 2) (progn 
               (compute-h-triad-3rd-species-cost
                    (nth (+ j 1) (h-intervals (nth i parts))) ; 2nd or 3rd beat (=j)
                    (subseq h-triad-3rd-species-cost (* j *cf-last-index) (* (+ j 1) *cf-last-index))) ; these are the costs corresponding to the 2nd or 3rd beat (=j)
            ))
            (add-cost-to-factors h-triad-3rd-species-cost 'h-triad-3rd-species-cost)
        ))
    )    

    ;================================================================================;
    ;                                    RETURN                                      ;
    ;================================================================================;
    (append (fux-search-engine solution-array species-list) (list species-list))
)