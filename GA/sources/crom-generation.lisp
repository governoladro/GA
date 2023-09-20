
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																		;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;							UTILITA'									;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																		;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																		;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																		;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun accoppia (lista)
      (labels ((costruisce (lista coppia)
                 (let ((resto (nthcdr 2 lista)))
                   (if (consp resto)
                     (costruisce resto (push (subseq lista 0 2) coppia))
                     (nreverse (push lista coppia))))))
        (if lista (costruisce lista nil) nil)))      
        

          


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																	;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;					GENERAZIONE										;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;				BINARIA (binary-creation)							;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;			FRASI MUSICALI (phrase-generator)						;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;																	;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 (om::defmethod! binary-creation ( (dimension integer) (cromLength integer) (max integer) )
    :initvals '(30 8 0)
    :indoc '("how many cromosomes" "cromosome length" "maximum lenght")
    :icon 240
    :doc "Inputs (left to right)
    quanti. Number of cromosomes to generate
    lunghi. How many long they are
    max. (optional) Use it if you need different length cromosomes.
    In this case <max> is maximum length of cromosome (and <cromLength> is minimum length), if <max> is 0 (default) or
    is less then <lunghi>, it is not considerated.
    Output a list of lists.    
   	Genera una lista di (dimension) cromosomi binari di lunghezza (cromLength).
    Se si desiderano cromosomi di lunghezza veriabile, Il valore opzionale <max>
    indica la lunghezza massima, (e <cromLength> quella minima). Se <max> è 0 o è inferiore a <cromLength> non considerato."
    (defun crom (lunghezza maxlung)
      (let ( (lista nil) (numAlleli (if (> maxlung lunghezza)
                                      (+ (random (- maxlung lunghezza)) lunghezza)
                                      lunghezza)) )
        (dotimes (i numAlleli lista)
          (push (random 2) lista))))
    (let ((popolo nil))
      (dotimes (i2 dimension popolo)
        (push (crom cromLength max) popolo)))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; generazione lista durate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; funzioni preliminari ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; restituisce un numero casuale tra -durMax e - durMin ;;;;;;;;;;;;;
;;;;;; oppure tra +durMin e + durMax, multiplo di passo ;;;;;;;;;;;;;;;;;
(defun durata-casuale (durMin durMax passo prob)
      (let ((min (divisione-modulo durMin passo))
            (max (divisione-modulo durMax passo)))
        (* (casuale-tra min max)
           (casuale+- prob)
           passo)))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; se sommatoria == limite restituisce ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; lista con l'aggiunta di val altrimenti ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; toglie dal valore assoluto di val l'eccedenza ;;;;;;;;;;;;;;
;;;;;;;;;;;; tra sommatoria e limite (- sommatoria limite) ;;;;;;;;;;;;;;
;;;;;;;;;;;; e mette in lista il val così diminuito ;;;;;;;;;;;;;;;;;;;;;
(defun toglie-eccedenza (val lista sommatoria limite)
      (if (= sommatoria limite)
        (push val lista)
        (cond ((< val 0) (push (+ val (- sommatoria limite)) lista))
              (t (push (- val (- sommatoria limite)) lista)))))
        

;;;;;;;;;;;;;;;;;; principale ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lista-durate (valMin valMax passo limite perc)
      (let ((val (durata-casuale valMin valMax passo perc)) (acc nil))
        (do (( somma (abs val) (+ (abs val) somma)))
            ((>= somma limite) (toglie-eccedenza val acc somma limite))
          (push val acc)
          (setf val (durata-casuale valMin valMax passo perc)))))

;;;;;;;;;;;;;;;;;; come sopra ma non toglie l'eccedenza ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; ed il valore assoluto di limite ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; (limite è negativo) indica la durata massima ;;;;;;;;
(defun lista-durate2 (valMin valMax passo limite perc)
      (let ((val (durata-casuale valMin valMax passo perc)) (acc nil) (tot (+ passo (abs limite))))
        (do (( somma (abs val) (+ (abs val) somma)))
            ((>= somma tot) acc)
          (setf val (durata-casuale valMin valMax passo perc))
          (push val acc))))
          
;;;;;;;;; generazione lista note ::;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nota-casuale (bassa alta passo)
      (let ((min (divisione-modulo bassa passo))
            (max (divisione-modulo alta passo)))
        (* (casuale-tra min max) passo)))
        
(defun lista-note (inferiore superiore passo durate)
      (let ((lista nil) (quantit (conta-i-positivi durate)))
        (do ((i 0 (+ 1 i)))
            ((= i quantit) lista)
          (push (nota-casuale inferiore superiore passo) lista))))
 
 
;;;;;;;;;; generazione frase (2 valori: note e durate) ;;;;;;;;;;;;;;;;;;;;;
 (defun genera-frase2 (nota-bassa nota-alta definizione durMin durMax scarto durTot percnot)
    (let ((durate (if (> durTot 0)
                    (lista-durate durMin durMax scarto durTot percnot)
                    (lista-durate2 durMin durMax scarto durTot percnot))))
      (values (lista-note nota-bassa nota-alta definizione durate) durate)))
      
;;;;;;;;;;;; come genera-frase ma unisce i valori ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
(defun genera-frase (nota-bassa nota-alta definizione durMin durMax scarto durTot percnot)
    (let ((durate (if (> durTot 0)
                    (lista-durate durMin durMax scarto durTot percnot)
                    (lista-durate2 durMin durMax scarto durTot percnot))))
      (unisci durate (lista-note nota-bassa nota-alta definizione durate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unisci (l1 l2)
(let ((ris nil) (ind 0))
                (dolist (elemento l1 ris)
                  (cond ((< elemento 0) (push (list elemento) ris))
                        (t (push (list elemento (nth ind l2)) ris)
                           (setf ind (+ 1 ind))))
                  ris)
                (nreverse ris)))

(defmethod! phrase-generator ((num-phrases integer) (low-note integer) (high-note integer) (cent-definition integer)
                                (min-duration rational) (max-duration rational) (dur-definition rational) (total-duration rational) (perc-note float))
   :initvals '(50 -12 12 1 1/16 2/4 1/16 8/4 0.85)
   :indoc '("how many phrases" "lowest note" "highest note" "cent division" "dur1" "dur2" "dur def" "total duration" "note vs pause")
   :numouts 1
   :icon 242
   :doc "Generate a <num-phrases> number of musical phrases of pitch between <low-note> and <high-note> and length <total-duration>
   Inputs (left to right)
   num-phrases. number of cromosomes (musical phrases) to generate
   low-note. lowest note
   high-note. higher note
   cent-definition. definition step between notes
   min-duration. minimum duration of notes (or pauses) in ratio or in millisecond
   max-duration. maximum duration
   dur-definition. allowed durations setp.
   total-duration. If positive, duration of cromosome (sum of notes and pauses) exactly. If negative, its absolute value is the limit (maximum) duration of cromosome.
   perc-note. notes/pauses ratio; 1 = only notes, 0 = only pauses, 0.5 (default) normal distribution
   Outputs (left to right)
   phrases. list of lists of pitches and dur"
  (let ((frasi nil))
    (dotimes (i num-phrases frasi)
      (push (genera-frase low-note high-note cent-definition min-duration max-duration dur-definition total-duration perc-note) frasi))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

