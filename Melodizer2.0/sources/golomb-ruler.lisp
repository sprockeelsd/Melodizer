(in-package :mldz)

(defun golomb-ruler (size)
    (let ((sp (gil::new-space))
          m d se tstop sopts size-d k)
         ; initializing the variable with arbitrary high max value
         (print "size")
         (print size)
         (setq m (gil::add-int-var-array sp size 0 (- (expt 2 (- size 1)) 1)))

         ; Assuming first mark to be zero
         (gil::g-rel sp (nth 0 m) gil::IRT_EQ 0)

         ; Order marks
         (gil::g-rel sp m gil::IRT_LE nil)

         (setf size-d (/ (- (* size size) size) 2))

         ; array of differences
         (setq d (gil::add-int-var-array sp size-d 0 (- (expt 2 (- size 1)) 1)))

         (setf k 0)
         (loop :for i :from 0 :below (- size 1) :by 1 :do
            (loop :for j :from (+ i 1) :below size :by 1 :do
                (progn
                    (gil::g-linear sp '(1 -1) (list (nth j m) (nth i m)) gil::IRT_EQ (nth k d))
                    (gil::g-rel sp (nth k d) gil::IRT_GQ (* (- j i) (/ ( + (- j i) 1) 2)))
                    (setf k (+ k 1))
                )
            )
         )

         (gil::g-distinct sp d)

         (if (> size 2)
            (gil::g-rel sp (nth 0 d) gil::IRT_LE (nth (- size-d 1) d))
         )

         (gil::g-branch sp m gil::INT_VAR_NONE gil::INT_VAL_MIN)

         (setq sopts (gil::search-opts))
         (gil::init-search-opts sopts)

         (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))
         (list se m sopts)
    )
)

(defun search-next-golomb-ruler (l)
    (let ((se (first l)) (mark* (second l)) (sopts (third l)) sol marks)
         (setq sol (gil::search-next se))
         (if (null sol)
             (error "No more solution")
         )
         (setq marks (gil::g-values sol mark*))
         (print marks)
    )
)
