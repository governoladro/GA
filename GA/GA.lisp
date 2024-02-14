;============================================================================
; OM-GA
; genetic algorithG library for OM
;============================================================================
;
;   This program is free software. 
;
;============================================================================





(in-package :om)

(mapc #'(lambda (file) 
          (compile&load (om-make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name file))) 
      '(
        "crom-generation"
        "gen-operators"
        "fitness-functions"
        "utilities"
        "selection"
        ))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(om::fill-library '( ("crom-generation" nil nil (binary-creation phrase-generator) nil)
                    ("gen-operators" nil nil (binary-mutation numbers-mutation pitch-mutation rhythmic-mutation rhythmic-mutation-add retroGa inverted re-ro transposition crossover) nil)
                    ("fitness-functions" nil nil (differences distances repeated pitch-variety pitch-range contour-direction contour-stability movement-by-step no-returns climax-strength syncopation sound-silence-ratio rhythmic-variety rhythmic-range duration-control random-fit) nil)
                    ("utilities" nil nil (dur-not-union separator scale1 summ vertical-control binToDec) nil)
                    ("selection" nil nil (evolution evolution-stationary evolution-turneament))
         ))

(set-lib-release 1.3)

;;;;;;;;;;;;;;;;;;;;;
