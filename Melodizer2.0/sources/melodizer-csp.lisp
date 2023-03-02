(in-package :mldz)

;;;;;;;;;;;;;;;;;
; NEW-MELODIZER ;
;;;;;;;;;;;;;;;;;

; <block-csp> list of the child block objects
; <percent-diff> percentage of difference wanted for the solutions
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine.
(defmethod new-melodizer (block-csp percent-diff branching)
    (let ((sp (gil::new-space)); create the space;
        push pull playing pushMap pullMap dfs tstop sopts scaleset pitch temp push-card q-push
        pos

        (max-pitch 127)
        (bars (bar-length block-csp))
        (quant 192)
        (min-length 1) ;minimum length of a note with associated constraint
        (chord-rhythm 2) ;a chord is played every [chord-rhythm] quant
        (chord-min-length 2)) ; minimum length of a chord with associated constraint

        (print block-csp)

        (setq push-list (list))
        (setq pull-list (list))
        (setq playing-list (list))
        (setq debug (list))
        (setq debug2 (list))

        ;Setting constraint for this block and child blocks
        (setq temp (get-sub-block-values sp block-csp))
        (setq push (nth 0 temp))
        (setq pull (nth 1 temp))
        (setq playing (nth 2 temp))
        (setq notes (nth 3 temp))
        (setq added-notes (nth 4 temp))
        (setq push-card (nth 5 temp))
        (setq q-push (nth 6 temp))

        (gil::g-specify-sol-variables sp q-push)
        (gil::g-specify-percent-diff sp percent-diff)

        (cond
            ((string-equal branching "Top down")
                (loop :for i :from (- (length push-list) 1) :downto 0 :do
                    (gil::g-branch sp (append (nth i push-list) (nth i pull-list)) gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
                )
            )
            ((string-equal branching "Full")
                (progn
                    (setq branch-push (list))
                    (setq branch-pull (list))
                    (loop :for l :in push-list :do
                        (setq branch-push (append branch-push l))
                    )
                    (loop :for l :in pull-list :do
                        (setq branch-pull (append branch-pull l))
                    )
                    (gil::g-branch sp (append branch-push branch-pull) gil::SET_VAR_SIZE_MIN gil::SET_VAL_RND_INC)
                )
            )
            ((string-equal branching "Top down random")
                (loop :for i :from (- (length push-list) 1) :downto 0 :do
                    (gil::g-branch sp (append (nth i push-list) (nth i pull-list)) gil::SET_VAR_RND gil::SET_VAL_RND_INC)
                )
            )
        )

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::BAB))

        (print "new-melodizer CSP constructed")
        ; return
        (list se push pull tstop sopts bars quant push-list pull-list playing-list debug debug2)
    )
)

;recursive function to set the constraint on all the blocks in the tree structure
(defun get-sub-block-values (sp block-csp)
    ; for block child of block-csp
    ; (pull supersets de get-sub-block-values(block) )
    ; constraints
    ; return pull push playing
    (let (pull push notes playing pushMap pushMap-card pullMap block-list positions max-notes sub-push sub-pull
          push-card added-push added-notes added-push-card q-push q-push-card
         (bars (bar-length block-csp))
         (quant 192)
         (prevNotes (list))
         (major-natural (list 2 2 1 2 2 2 1))
         (max-pitch 127))

         (setq max-notes (* 127 (+ (* bars quant) 1)))

        ;initialize the variables

        (setq push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq playing (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))

        (setq push-list (nconc push-list (list push)))
        (setq pull-list (nconc pull-list (list pull)))
        (setq playing-list (nconc playing-list (list playing)))

        ;channeling array with time as index to array with pitch as index
        (setq pushMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (+ (* bars quant) 1) 0 (+ (* bars quant) 1)))
        (setq pullMap (gil::add-set-var-array sp (+ max-pitch 1) 0 (+ (* bars quant) 1) 0 (+ (* bars quant) 1)))
        (gil::g-channel sp push pushMap)
        (gil::g-channel sp pull pullMap)

        (setq pushMap-card (gil::add-int-var-array sp 128 0 (+ (* bars quant) 1)))
        (loop :for i :from 0 :below (length pushMap) :by 1 :do
            (gil::g-card-var sp (nth i pushMap) (nth i pushMap-card))
        )

        (setq block-list (block-list block-csp))
        (if (not (typep block-list 'list))
            (setq block-list (list block-list))
        )
        (setq positions (position-list block-csp))

        ;initial constraint on pull, push, playing and durations
        (gil::g-empty sp (first pull)) ; pull[0] == empty
        (gil::g-empty sp (car (last push)))  ; push[bars*quant] == empty
        (gil::g-empty sp (car (last playing)))  ; playing[bars*quant] == empty
        (gil::g-rel sp (first push) gil::SRT_EQ (first playing)) ; push[0] == playing [0]

        ;compute notes
        (setq notes (gil::add-int-var sp 0 max-notes))
        (setq push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))

        (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
            (gil::g-card-var sp (nth i push) (nth i push-card))
        )
        (gil::g-sum sp notes push-card)


        ;compute added notes
        (setq added-push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq sub-push (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq sub-pull (gil::add-set-var-array sp (+ (* bars quant) 1) 0 max-pitch 0 max-pitch))
        (setq added-notes (gil::add-int-var sp 0 127))
        (setq added-push-card (gil::add-int-var-array sp (+ (* bars quant) 1) 0 127))
        (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
            (gil::g-card-var sp (nth i added-push) (nth i added-push-card))
        )
        (gil::g-sum sp added-notes added-push-card)

        ;compute q-push
        (setq q-push (gil::add-set-var-array sp (* bars (get-quant (quantification block-csp))) 0 max-pitch 0 max-pitch))
        (loop :for i :from 0 :below (length q-push) :by 1 :do
            (gil::g-rel sp (nth i q-push) gil::SRT_EQ (nth (* i (get-length (quantification block-csp))) push))
        )
        (setq q-push-card (gil::add-int-var-array sp (length q-push) 0 127))
        (loop :for i :from 0 :below (length q-push) :by 1 :do
            (gil::g-card-var sp (nth i q-push) (nth i q-push-card))
        )


        ;connect push, pull and playing
        (loop :for j :from 1 :below (+ (* bars quant) 1) :do ;for each interval
            (let (temp z c)
                (setq temp (gil::add-set-var sp 0 max-pitch 0 max-pitch)); temporary variables
                (gil::g-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) temp); temp[0] = playing[j-1] - pull[j]
                (gil::g-op sp temp gil::SOT_UNION (nth j push) (nth j playing)); playing[j] == playing[j-1] - pull[j] + push[j] Playing note
                (gil::g-rel sp (nth j pull) gil::SRT_SUB (nth (- j 1) playing)) ; pull[j] <= playing[j-1] cannot pull a note not playing
                (gil::g-set-op sp (nth (- j 1) playing) gil::SOT_MINUS (nth j pull) gil::SRT_DISJ (nth j push)); push[j] || playing[j-1] - pull[j] Cannot push a note still playing

            )
        )

        (if (melody-source block-csp)
            (let (melody-temp melody-push melody-pull melody-playing)
                (setq melody-temp (create-push-pull (melody-source block-csp) quant))
                (setq melody-push (gil::add-set-var-array sp (length (first melody-temp)) 0 max-pitch 0 max-pitch))
                (setq melody-pull (gil::add-set-var-array sp (length (second melody-temp)) 0 max-pitch 0 max-pitch))
                (setq melody-playing (gil::add-set-var-array sp (length (third melody-temp)) 0 max-pitch 0 max-pitch))
                (loop :for i :from 0 :below (length (first melody-temp)) :by 1 :do
                    (if (or (typep (nth i (first melody-temp)) 'list) (/= (nth i (first melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-push) gil::SRT_EQ (nth i (first melody-temp)))
                        (gil::g-empty sp (nth i push))
                    )
                )
                (loop :for i :from 0 :below (length (second melody-temp)) :by 1 :do
                    (if (or (typep (nth i (second melody-temp)) 'list) (/= (nth i (second melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-pull) gil::SRT_EQ (nth i (second melody-temp)))
                        (gil::g-empty sp (nth i pull))
                    )
                )
                (loop :for i :from 0 :below (length (third melody-temp)) :by 1 :do
                    (if (or (typep (nth i (third melody-temp)) 'list) (/= (nth i (third melody-temp)) -1))
                        (gil::g-rel sp (nth i melody-playing) gil::SRT_EQ (nth i (third melody-temp)))
                        (gil::g-empty sp (nth i melody-playing))
                    )
                )
                (loop :for j :from 0 :below (length melody-push) :by 1 :do
                        (gil::g-rel sp (nth j melody-push) gil::SRT_SUB (nth j push))
                        (gil::g-rel sp (nth j melody-pull) gil::SRT_SUB (nth j pull))
                )
            )
        )


        (if (not (endp block-list))
            ; make the push and pull array supersets of the corresponding array of the child blocks
            (let ((sub-push-list (list)) (sub-pull-list (list)))

                (loop :for i :from 0 :below (+ (* bars quant) 1) :by 1 :do
                    (setq temp1 (gil::add-set-var-array sp (length block-list) 0 max-pitch 0 max-pitch))
                    (setq temp2 (gil::add-set-var-array sp (length block-list) 0 max-pitch 0 max-pitch))
                    (gil::g-setunion sp (nth i sub-push) temp1)
                    (setq sub-push-list (nconc sub-push-list (list temp1)))
                    (gil::g-setunion sp (nth i sub-pull) temp2)
                    (setq sub-pull-list (nconc sub-pull-list (list temp2)))
                    (gil::g-op sp (nth i push) gil::SOT_MINUS (nth i sub-push) (nth i added-push))
                )
                (loop :for i :from 0 :below (length block-list) :by 1 :do
                      (let (tempPush tempPull tempPlaying tempList (start (* (nth i positions) quant)))
                           (setq tempList (get-sub-block-values sp (nth i block-list)))
                           (setq tempPush (first tempList))
                           (setq tempPull (second tempList))
                           (setq tempPlaying (third tempList))
                           (setq prevNotes (nth 7 tempList))

                           (loop :for j :from start :below (+ start (length tempPlaying)) :by 1 :do
                                (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_SUB (nth j push))
                                (gil::g-rel sp (nth (- j start) tempPull) gil::SRT_SUB (nth j pull))
                                (gil::g-rel sp (nth (- j start) tempPlaying) gil::SRT_SUB (nth j playing))
                           )

                           (loop :for j :from 0 :below (length push) :by 1 :do
                                (if (and (>= j start) (< j (+ start (length tempPlaying))))
                                    (gil::g-rel sp (nth (- j start) tempPush) gil::SRT_EQ (nth i (nth j sub-push-list)))
                                    (gil::g-empty sp (nth i (nth j sub-push-list)))
                                )
                           )

                           (loop :for j :from 0 :below (length pull) :by 1 :do
                                (if (and (>= j start) (< j (+ start (length tempPlaying))))
                                    (gil::g-rel sp (nth (- j start) tempPull) gil::SRT_EQ (nth i (nth j sub-pull-list)))
                                    (gil::g-empty sp (nth i (nth j sub-pull-list)))
                                )
                           )
                      )
                )
            )
            ; if no block-list
            (progn
                (gil::g-rel sp added-notes gil::SRT_EQ notes)
                (loop :for p :in sub-push :do (gil::g-empty sp p))
                (loop :for p :in sub-pull :do (gil::g-empty sp p))
            )


        )

        ;constraints
        (post-optional-constraints sp block-csp push pull playing pushMap pushMap-card notes added-notes push-card sub-push sub-pull q-push q-push-card)
        (pitch-range sp push (min-pitch block-csp) (max-pitch block-csp))
        (list push pull playing notes added-notes push-card q-push)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (sp block push pull playing pushMap pushMap-card notes added-notes push-card sub-push sub-pull q-push q-push-card)

    ; Block constraints
    (if (voices block)
        (gil::g-card sp playing 0 (voices block))
    )

    (if (min-pushed-notes block)
        (loop :for i :from 0 :below (length push-card) :by 1 :do
            (setq b1 (gil::add-bool-var sp 0 1))
            (gil::g-rel-reify sp (nth i push-card) gil::IRT_EQ 0 b1)
            (setq b2 (gil::add-bool-var sp 0 1))
            (gil::g-rel-reify sp (nth i push-card) gil::IRT_GQ (min-pushed-notes block) b2)
            (gil::g-rel sp b1 gil::BOT_OR b2)
        )
    )

    (if (max-pushed-notes block)
        (gil::g-card sp push 0 (max-pushed-notes block))
    )

    (if (min-notes block)
        (progn
            (gil::g-rel sp notes gil::IRT_GQ (min-notes block))
        )
    )

    (if (max-notes block)
        (gil::g-rel sp notes gil::IRT_LQ (max-notes block))
    )

    (if (min-added-notes block)
        (gil::g-rel sp added-notes gil::IRT_GQ (min-added-notes block))
    )

    (if (max-added-notes block)
        (if (= 0 (max-added-notes block))
            (progn
                (loop :for i :from 0 :below (length push) :by 1 :do
                    (gil::g-rel sp (nth i push) gil::SRT_EQ (nth i sub-push))
                )
            )
            (gil::g-rel sp added-notes gil::IRT_LQ (max-added-notes block))
        )

    )

    ; Time constraints
    (if (min-note-length-flag block)
        (note-min-length sp push pull (min-note-length block))
    )

    (if (max-note-length-flag block)
        (note-max-length sp push pull (max-note-length block))
    )

    (if (quantification block)
        (set-quantification sp push pull (quantification block))
    )

    (if (rhythm-repetition block)
        (set-rhythm-repetition sp push-card (get-length (rhythm-repetition block)))
    )

    (if (pause-quantity-flag block)
        (set-pause-quantity sp q-push-card (pause-quantity block) (bar-length block) (get-quant (quantification block)))
    )

    (if (pause-repartition-flag block)
        (set-pause-repartition sp q-push-card (pause-repartition block))
    )

    ; Pitch constraints
    ; following a scale
    (if (key-selection block)
        (if (mode-selection block)
            (let (scaleset
                  (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale (mode-selection block)))  ;if - mode selectionné
                  (offset (- (name-to-note-value (key-selection block)) 60)))
                 (setq scaleset (build-scaleset scale offset))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (scale-follow-reify sp push scaleset bool))
            (let (scaleset
                  (bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                  (scale (get-scale "ionian (major)"))  ;else - pas de mode selectionné => major natural
                  (offset (- (name-to-note-value (key-selection block)) 60)))
                 (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                 (setq scaleset (build-scaleset scale offset))
                 (scale-follow-reify sp push scaleset bool))
        )
        (if (mode-selection block)
            (let ((bool-array (gil::add-bool-var-array sp 12 0 1))) ; créer le booleen pour la reify
                (loop :for key :from 0 :below 12 :by 1 :do
                    (setq scale (get-scale (mode-selection block)))
                    (setq scaleset (build-scaleset scale key))
                    (scale-follow-reify sp push scaleset (nth key bool-array))
                )
                (gil::g-rel sp gil::BOT_OR bool-array 1)
            )
        )
    )

    (if (chord-key block)
        (if (chord-quality block)
            (if (all-chord-notes block)
                (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                      (bool2 (gil::add-bool-var sp 0 1))
                      (chord (get-chord (chord-quality block)))  ;if - mode selectionné
                      (offset (- (name-to-note-value (chord-key block)) 60))
                      (all-notes (gil::add-set-var sp 0 127 0 127))
                      chordset notesets bool-array)
                     (setq chordset (build-scaleset chord offset))
                     (scale-follow-reify sp push chordset bool)
                     (setq notesets (build-notesets chord offset))
                     (setq bool-array (gil::add-bool-var-array sp (length notesets) 0 1))
                     (loop :for i :from 0 :below (length notesets) :do
                          (let ((push-bool-array (gil::add-bool-var-array sp (length push) 0 1)))
                              (loop :for j :from 0 :below (length push) :do
                                  (gil::g-rel-reify sp (nth j push) gil::SRT_DISJ (nth i notesets) (nth j push-bool-array))
                              )
                              (gil::g-rel sp gil::BOT_AND push-bool-array (nth i bool-array))
                          )
                     )
                     (setq debug (nconc debug (list bool-array)))
                     (setq debug2 (nconc debug2 (list bool2)))

                     (gil::g-rel sp gil::BOT_OR bool-array bool2)
                     (gil::g-rel sp bool gil::SRT_EQ 1)
                     )

                (let ((bool (gil::add-bool-var sp 0 1)) ; créer le booleen pour la reify
                      (chord (get-chord (chord-quality block)))  ;if - mode selectionné
                      (offset (- (name-to-note-value (chord-key block)) 60))
                      (all-notes (gil::add-set-var sp 0 127 0 127))
                      chordset)
                     (gil::g-setunion sp all-notes push)
                     (setq chordset (build-scaleset chord offset))
                     (gil::g-rel sp bool gil::SRT_EQ 1) ;forcer le reify a true dans ce cas
                     (scale-follow-reify sp push chordset bool))
            )
        )
        (if (chord-quality block)
            (if (all-chord-notes block)
                (let (chord chordset notesets
                      (bool-array (gil::add-bool-var-array sp 12 0 1)); créer le booleen pour la reify
                      (all-notes (gil::add-set-var sp 0 127 01 127)))
                    (gil::g-setunion sp all-notes push)
                    (loop :for key :from 0 :below 12 :by 1 :do
                        (let ((bool1 (gil::add-bool-var sp 0 1))
                              (bool2 (gil::add-bool-var sp 0 1))
                              (bool-array-note (gil::add-bool-var-array sp (length notesets) 0 1))
                              chordset notesets)
                            (setq chord (get-chord (chord-quality block)))
                            (setq chordset (build-scaleset chord key))
                            (setq notesets (build-notesets chord key))

                            (loop :for i :from 0 :below (length notesets) :do
                                 (gil::g-rel-reify sp all-notes gil::SRT_DISJ (nth i notesets) (nth i bool-array-note))
                            )
                            (gil::g-rel sp gil::BOT_AND bool-array-note bool1)
                            (scale-follow-reify sp push chordset bool2)
                            (gil::g-op sp (nth key bool-array) gil::BOT_AND bool 0))
                    )
                    (gil::g-rel sp gil::BOT_OR bool-array 1)
                )
                (let (chord chordset
                      (bool-array (gil::add-bool-var-array sp 12 0 1)))
                    (loop :for key :from 0 :below 12 :by 1 :do
                        (setq chord (get-chord (chord-quality block)))
                        (setq chordset (build-scaleset chord key))
                        (scale-follow-reify sp push chordset (nth key bool-array))
                    )
                    (gil::g-rel sp gil::BOT_OR bool-array 1)
                )
            )

        )
    )


    (if (pitch-direction block)
        (let ((allPlayed (gil::add-set-var sp 0 (+ (length push) 1) 0 (+ (length push) 1)))
              (isPlayed (gil::add-bool-var-array sp (+ (length push) 1) 0 1)))
             (gil::g-arr-op sp gil::SOT_UNION pushMap allPlayed)
             (gil::g-channel sp isPlayed allPlayed)

            (cond
                ((string= (pitch-direction block) "Increasing")           (increasing-pitch sp push isPlayed))
                ((string= (pitch-direction block) "Strictly increasing")  (strictly-increasing-pitch sp push isPlayed))
                ((string= (pitch-direction block) "Decreasing")           (decreasing-pitch sp push isPlayed))
                ((string= (pitch-direction block) "Strictly decreasing")  (strictly-decreasing-pitch sp push isPlayed))
            )
        )
    )

    (if (/= (golomb-ruler-size block) 0)
         (golomb-rule sp (golomb-ruler-size block) push (/ 192 (get-quant (quantification block))))
    )



    (if (note-repetition-flag block)
        (cond
          ((string-equal (note-repetition-type block) "Random")
            (random-repeat-note sp push (note-repetition block) (get-length (quantification block))))
          ((string-equal (note-repetition-type block) "Soft")
            (soft-repeat-note sp (note-repetition block) pushMap-card))
          ((string-equal (note-repetition-type block) "Hard")
            (hard-repeat-note sp (note-repetition block) pushMap-card (length q-push)))
        )
    )
)

;;;;;;;;;;;;;;;
; SEARCH-NEXT ;
;;;;;;;;;;;;;;;

; <l> is a list containing the search engine for the problem and the variables
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod new-search-next (l melodizer-object)
    (let ((se (first l))
         (push (second l))
         (pull (third l))
         (tstop (fourth l))
         (sopts (fifth l))
         (bars (sixth l))
         (quant (seventh l))
         (push-list (eighth l))
         (pull-list (ninth l))
         (playing-list (nth 9 l))
         (debug (nth 10 l))
         (debug2 (nth 11 l))
         (check t); for the while loop
         sol score)

         (print "in search")

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol)
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        ;SOME CODE PIECES FOR DEBUGGING
         
        ;(print "PUSH")
        ;(loop :for p :in push-list :do
        ;    (let (l (list))
        ;        (print (gil::vid p))
        ;        (setq l (nconc l (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) p)))
        ;        (print l)
        ;    )
        ;)

        ;(print "PULL")
        ;(loop :for p :in pull-list :do
        ;    (let (l (list))
        ;        (setq l (nconc l (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) p)))
        ;        (print l)
        ;    )
        ;)

        ;(print "PLAYING")
        ;(loop :for p :in playing-list :do
        ;    (let (l (list))
        ;        (setq l (nconc l (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) p)))
        ;        (print l)
        ;    )
        ;)

        ;(print "DEBUG")
        ;(print debug)
        ;(loop :for p :in debug :do
        ;  (let (l (list))
        ;      (setq l (nconc l (mapcar (lambda (n) (gil::g-values sol n)) p)))
        ;      (print l)
        ;  )
        ;)

        ;(print "DEBUG")
        ;(loop :for p :in debug2 :do
        ;  (print (gil::g-values sol p))
        ;)


         ;créer score qui retourne la liste de pitch et la rhythm tree
        (setq score-chord-seq (build-chord-seq sol push pull bars quant (tempo melodizer-object)))

        (make-instance 'chord-seq
            :LMidic (first score-chord-seq)
            :LOnset (second score-chord-seq)
            :Ldur (third score-chord-seq)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user tstop)
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)
