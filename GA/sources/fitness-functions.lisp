;;;;;;;;;; UTILITA' COMUNI
;;;;;; divcon0
;;;;;; compresop
;;;;;; riscala
 

;;;;;;;;;;;;; riscala i valori di una lista. In-su-in-giù decide se i valori vanno riscalati fra 1 e 0 ;;;;;;;;;
;;;;;;;;;;;;; con il valore zero effettivamente = a 0, oppure fra 0 e 1. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; nel primo caso (in-su-in-giu = 0) il valore zero indicherebbe la minima idoneità ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; nel secondo caso (in-su-in-giu = 1) il valore zero indicherebbe la massima idoneità ;;;;;;;;;;;;;;
(defun riscala-lista (lista zero in-su-in-giu)
    (if (= 0 in-su-in-giu)
      (mapcar #'(lambda (x) (riscala x zero)) lista)
      (mapcar #'(lambda (x) (- 1 (riscala x zero))) lista)))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;																 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;				 Calcolo delle Fintness 						 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;  Ogni funzione restituisce i valori di fitness scalati fra 0 e 1  ;;;;;;;;;;;;;;;;;;;;;;;;;;															 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;      	   I valori migliori sono i più bassi                       ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;*********************************************************************************************************************************;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; fitness generici ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*********************************************************************************************************************************;

;;;;;;;;;;;;;;;;;;;; *********************************************************** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! differences ((pitches list) (allowed-elements list) (option symbol) (ideal-ratio float))
   :initvals '(() () 'normal 0.01)
   :menuins '((2 (("Normal" 'normal) ("Octave" 'octave) ("Absolute" 'absolute))))
   :indoc '("list of pitches" "permitted distance from root" "option" "ideal ratio")
   :numouts 1
   :icon 242
   :doc "giudica una lista di valori <intervals>, calcolando quanti di essi non sono
   presenti in <allowed-elements>. Se option è 'octave' calcola prima il mod 12 degli elementi, se è 'absolute' il valore assoluto. 
   Inputs (left to right)
   intervals. list of list of intervals
   allowed-intervals. list of permitted intervals
   octave. if 1 octave differences don't mind.
   ratio. ideal value"
  (if (null pitches)
  	   nil
  	   (let ((elemento (car pitches)))
  	   	  (if (null elemento)
  	   	  	 (cons 0.0 (differences (cdr pitches) allowed-elements option ideal-ratio))
  	   		 (let* ((note (calcola-valori elemento option))
  	   		  	    (valore (estranei note allowed-elements)))
  	   		 	(cons (riscala (divcon0 valore (length elemento)) ideal-ratio)
  	   		 		  (differences (cdr pitches) allowed-elements option ideal-ratio)))))))
  	   		 		  

;;;;UTILITA'
;;;;;;; conta gli intervalli non presenti;;;;;;;
(defun estranei (intervalli classi)
                   (length (set-difference intervalli classi)))
                   
                   
(defun calcola-valori (elementi condizione)
    (cond ((equal condizione 'octave)
           (mapcar #'(lambda (x) (mod x 12)) elementi))
          ((equal condizione 'absolute)
           (mapcar #'abs elementi))
          (t elementi)))
          

;;;;;;;;;;;;;;;;;;;; **********CONTROLLARE************************************ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! distances ((pitches list) (allowed-elements list) (option symbol) (type symbol))
   :initvals '(() () 1 'normal)
   :menuins '((2 (("Normal" 'normal) ("Octave" 'octave) ("Absolute" 'absolute))) (3 (("Normal" 'normal) ("Position" 'position))))
   :indoc '("list of pitches" "permitted elements" "option" "position or not")
   :numouts 1
   :icon 242
   :doc "Giudica gli intervalli in base alla distanza da una lista di <allowed-elements> (somma le distanze di ogni
   		intervallo dal più vicino degli intervalli permessi). Il punteggio è fra 0 e 1 con 1 corrispondente a 0 errori.
   		se option is 'octave' prima calcola il mod 12, se 'absolute calcola il valore assoluto. Se <type> è su 'position
   		allora ogni valore di errore viene moltiplicato per l'indice che segna la sua posizione nel cromosoma.
   Inputs (left to right)
   intervals. list of list of intervals
   allowed-elements. list of permitted intervals
   octave. if 1 octave differences allowed.
   position. If position error is dipendent by index position"
  (if (null pitches)
  	nil
  	(let ((elemento (car pitches)))
  	   	  (if (null elemento)
  	   	  	  (cons 0.0 (distances (cdr pitches) allowed-elements option type))
  	   	  	  (let ((valore (case type 
  	   	  	  					(normal (lontananze allowed-elements (calcola-valori elemento option)))
  	   	  	  					(otherwise (lontananze2 allowed-elements (calcola-valori elemento option) 1)))))
  	   	  	  	(cons (- 1 (/ 1 (+ 1 valore)))
  	   		 		  (distances (cdr pitches) allowed-elements option type)))))))
  	   		 		  
;;;;;;; UTILITA'  	
;;;;;;;;;;;;;;; calcola la somma di quanto ogni intervallo nella lista è lontano ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; dal più vicino fra gli intervalli permessi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lontananze (intervalli-ammessi intervalli)
              (if (null intervalli)
                  0
                (+ (distanza-minima (car intervalli) intervalli-ammessi)
                   (lontananze intervalli-ammessi (cdr intervalli)))))
                   
(defun lontananze2 (intervalli-ammessi intervalli ind)
              (if (null intervalli)
                  0
                (+ (* ind (distanza-minima (car intervalli) intervalli-ammessi))
                   (lontananze2 intervalli-ammessi (cdr intervalli) (1+ ind)))))
                   
                   
 (defun distanza (n lista)
              (mapcar #'(lambda (x) (abs (- n x))) lista))
              
              
 (defun distanza-minima (n lista)
 	(if (member n lista)
 		0
        (reduce #'min (distanza n lista)))) 
;;;;;;;;;;;;;;;;;;;; *********************************************************** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;; rapporto fra valori ripetuti  ed il numero di valori ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! repeated ((cromosomes list) (ideal-ratio float))
   :initvals '(() 0.4)
   :indoc '("list of events" "ratio diatonic intervals")
   :numouts 1
   :icon 242
   :doc "calcola la proporzione delle ripetutizioni.
   Inputs (left to right)
   intervals. list of list of events
   ideal-ratio. Ideal ratio value (default 0.4)
   output. list of fitness values"
	(if (null cromosomes)
		nil
		(let ((elemento (car cromosomes)))
			(if (null elemento)
				(cons 1.0 (repeated (cdr cromosomes) ideal-ratio))
			   	(let ((valore (divcon0 (ripetizioni (car elemento) (cdr elemento))
			   						   (1- (length elemento)))))
					(cons (riscala valore ideal-ratio) (repeated (cdr cromosomes) ideal-ratio)))))))


;;;; conta le ripetizioni nella lista fatta da primo e resto					
(defun ripetizioni (primo resto)
    (if (null resto)
      0
      (let ((secondo (car resto)))
        (if (equal primo secondo)
          (+ 1 (ripetizioni secondo (cdr resto)))
          (ripetizioni secondo (cdr resto))))))
;*********************************************************************************************************************************;




;*********************************************************************************************************************************;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; funzioni per giudicare le altezze ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*********************************************************************************************************************************;

;*********************************************************************************************************************************;      
;;;;;;;;;;;;;  calcola il rapporto fra il numero di note diverse e il numero di note totali ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; riscala poi questo rapporto in modo che valore ideale sia il valore massimo ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; valore-ideale dovrebbe essere circa 0.24 nella musica tonale ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; IN ENTRATA: lista di lista di note (sole altezze) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! pitch-variety ((pitches list) (ideal-ratio float))
   :initvals '(() 0.7)
   :indoc '("list of list of notes" "ratio different total notes")
   :numouts 1
   :icon 242
   :doc "calcola il rapporto fra il numero di note diverse e il numero di note totali
   L'idoneità aumenta con l'avvicinarsi ad un valore ideale per questo rapporto
   Inputs (left to right)
   pitches. list of list of notes
   ideal-ratio. Ideal ratio value (default 0.7)
   output. list of fitness values"
   (if (null pitches)
		nil
      	(let ((elemento (car pitches)))
      		(if (null elemento)
      			(cons 1.0 (pitch-variety (cdr pitches) ideal-ratio))
      			(let ((num-note (length elemento))
            		  (n-note-diverse (length (remove-duplicates elemento))))
       				(cons (riscala (divcon0 n-note-diverse num-note) ideal-ratio)
       					  (pitch-variety (cdr pitches) ideal-ratio)))))))
;*********************************************************************************************************************************;


;*********************************************************************************************************************************;
;;;;;;;;;;;; calcola il rapporto fra la massima distanza fra le altezze  di una lista ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; e il massimo salto accettato (max-pitch-range) solitamente 15 (poco più di un ottava) ;;;;;;;;;;;;;
(defmethod! pitch-range ((pitches list) (max-pitch-range integer))
   :initvals '(() 15)
   :indoc '("list of list of notes" "maximum pitch range")
   :numouts 1
   :icon 242
   :doc "Giudica quanto l'estensione degli individui da valutare è diversa da quella indicata in 'max-pitch-range' solitamente 15 (poco più di un ottava) 
   Inputs (left to right)
   pitches. list of list of notes
   max-pitch-range. maximum pitch range between pitches
   output. list of fitness values"
	(if (null pitches)
		nil
		(let ((val (car pitches)))
		  (if (null val) 
		  	 (cons 1.0 (pitch-range (cdr pitches) max-pitch-range))
		  	 (cons (ampiezza-profilo val max-pitch-range)
		  	 	   (pitch-range (cdr pitches) max-pitch-range))))))
		  		
	   
                                          
;;;;;;;;;; UTILITA'
(defun ampiezza-profilo (altezze massima-ampiezza)
	(let ((salto (- (apply #'max altezze) (apply #'min altezze))))
		(if (<= salto massima-ampiezza)
			0.0
			(- 1.0 (/ 1 (- salto massima-ampiezza))))))
			  
;*********************************************************************************************************************************;              

                        
;*********************************************************************************************************************************;
;;;;;;;;;;;;;;;;; rapporto fra gli intervalli dissonanti e il numero totale di intervalli ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; agli intervalli è associato un punteggio (int 10 = 0.5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; int 6, 11, >12 = 1 gli altri = 0)                           
(defmethod! dissonant-intervals ((intervals list) (ideal-ratio float))
   :initvals '(() 0.01)
   :indoc '("list of intervals" "ratio dissonant total intervals")
   :numouts 1
   :icon 242
   :doc "calcola il rapporto fra gli intervalli dissonanti e il numero totale di intervalli
   agli intervalli è associato un punteggio (int 10 = 0.5 int 6, 11, >12 = 1 gli altri = 0) 
   Inputs (left to right)
   intervals. list of list of intervals
   ideal-ratio. Ideal ratio value (default 0.01)
   output. list of fitness values"
	(if (null intervals)
		nil
		(let ((elemento (car intervals)))
			(if (null elemento)
				(cons 1.0 (dissonant-intervals (cdr intervals) ideal-ratio))
				(let ((valore (divcon0 (dissonanze elemento) (length elemento))))
				  (cons (riscala valore ideal-ratio) (dissonant-intervals (cdr intervals) ideal-ratio)))))))
                                       
;;;;;;;;;; UTILITA'                   
(defun dissonanze (l-intervalli)
                   (let ((intervallo (car l-intervalli)))
                     (cond ((null intervallo) 0)
                           ((= intervallo 10) (+ 0.5 (dissonanze (cdr l-intervalli))))
                           ((or (= intervallo 6)
                                (= intervallo 11)
                                (> intervallo 12)) (+ 1.0 (dissonanze (cdr l-intervalli))))
                           (t (dissonanze (cdr l-intervalli))))))
;*********************************************************************************************************************************;
                                               

;*********************************************************************************************************************************;
;;;;;;;;;;;;;;   rapporto fra intervalli ascendenti sommati fra loro e ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   la somma di tutti gli intervalli ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! contour-direction ((direct-intervals list) (ideal-ratio float))
   :initvals '(() 0.5)
   :indoc '("list of intervals" "ratio ascending melody")
   :numouts 1
   :icon 242
   :doc "calcola il rapporto fra intervalli ascendenti sommati fra loro e
   la somma di tutti gli intervalli. 'E un indicatore della tendenza della melodia a selire o scendere
   una melodia che inizia e finisce con la stessa nota segnerà 0.5 
   Inputs (left to right)
   intervals. list of list of intervals
   ideal-ratio. Ideal ratio value (default 0.5)
   output. list of fitness values"
	(if (null direct-intervals)
		nil
		(let ((elemento (car direct-intervals)))
			(if (null elemento)
				(cons 1.0 (contour-direction (cdr direct-intervals) ideal-ratio))
				(let ((valore (divcon0 (somma-chi-sale elemento) (somma-assoluti elemento))))
					(cons (riscala valore ideal-ratio) (contour-direction (cdr direct-intervals) ideal-ratio)))))))
		
;;;;;;;;;; UTILITA'
(defun somma-chi-sale (l-intervalli)
                   (if (null l-intervalli)
                   	   0
                   	   (let ((intervallo (car l-intervalli)))
                   		 (if (> intervallo 0)
                   		 	 (+ intervallo (somma-chi-sale (cdr l-intervalli)))
                   		 	 (somma-chi-sale (cdr l-intervalli))))))
                   		 	 
(defun somma-assoluti (lista)
	(reduce #'+ (mapcar #'abs lista)))
;*********************************************************************************************************************************;



;*********************************************************************************************************************************;	
;;;;;;;;;;;;;; rapporto fra intervalli per i quali l'intervallo seguente va nella stessa direzione ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;  e il numero di intervalli meno 1. 3 intervalli uguali sono considerati nella stessa direzione ;;;;;;;;
;;;;;;;;;;;;;; le lista di altezze devono contenere almeno 3 note ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! contour-stability ((direct-intervals list) (ideal-ratio float))
   :initvals '(() 0.4)
   :indoc '("list of intervals" "ratio same direction")
   :numouts 1
   :icon 242
   :doc "calcola la proporzione di intervalli per i quali l'intervallo seguente va nella stessa direzione
   Inputs (left to right)
   intervals. list of list of intervals
   ideal-ratio. Ideal ratio value (default 0.4)
   output. list of fitness values"
	(if (null direct-intervals)
		nil
		(let ((elemento (car direct-intervals)))
			(if (null elemento)
				(cons 1.0 (contour-stability (cdr direct-intervals) ideal-ratio))
				(let ((valore (divcon0 (stessa-direzione elemento) (- (length elemento) 1))))
					(cons (riscala valore ideal-ratio) (contour-stability (cdr direct-intervals) ideal-ratio)))))))
			
			   
;;;;;;;;;; UTILITA'
(defun stessa-direzione (lista)
	(let ((direzioni (mapcar #'segnop lista (cdr lista))))
	 (count t direzioni)))
	
(defun segnop (a b)
    (cond ((= a 0) (= b 0))
          ((> a 0) (> b 0))
          (t (< b 0))))        
;*********************************************************************************************************************************;

            

;*********************************************************************************************************************************;                          
;;;;;;;;;;;;;; rapporto fra intervalli diatonici ed il numero di intervalli ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! movement-by-step ((direct-intervals list) (ideal-ratio float))
   :initvals '(() 0.62)
   :indoc '("list of intervals" "ratio diatonic intervals")
   :numouts 1
   :icon 242
   :doc "calcola la proporzione di intervalli diatonici.
   Inputs (left to right)
   intervals. list of list of intervals
   ideal-ratio. Ideal ratio value (default 0.62)
   output. list of fitness values"
	(if (null direct-intervals)
		nil
		(let ((elemento (car direct-intervals)))
			(if (null elemento)
				(cons 1.0 (movement-by-step (cdr direct-intervals) ideal-ratio))
				(let ((valore (divcon0 (conta-diatonici elemento) (length elemento))))
					(cons (riscala valore ideal-ratio) (movement-by-step (cdr direct-intervals) ideal-ratio)))))))
			
;;;;;;;;;; UTILITA'			
(defun conta-diatonici (lista)
	(if (null lista)
		0
		(let ((ele (abs (car lista))))
			(cond ((= ele 1) (+ 1 (conta-diatonici (cdr lista))))
				  ((= ele 2) (+ 1 (conta-diatonici (cdr lista))))
				  (t (conta-diatonici (cdr lista)))))))
;*********************************************************************************************************************************;

				  
				  
;*********************************************************************************************************************************;				  
;;;;;;;;;;;;;; rapporto fra intervalli grandi non seguiti da un intervallo di recupero ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;  e la somma di tutti gli intervalli larghi. Un intervallo largo è >= a 8 (6° minore) ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! no-returns ((direct-intervals list) (ideal-ratio float) (large-interval-def integer))
   :initvals '(() 0.2 7)
   :indoc '("list of intervals" "ratio" "large interval")
   :numouts 1
   :icon 242
   :doc "calcola la proporzione di intervalli > di large-interval-def non seguiti da un intervallo di recupero.
   Inputs (left to right)
   intervals. list of list of intervals
   ideal-ratio. Ideal ratio value (default 0.2)
   large-interval-def. number indicating the large interval - 1 (def 7)
   output. list of fitness values"
	(if (null direct-intervals)
		nil
		(let ((elemento (car direct-intervals)))
			(if (null elemento)
				(cons 1.0 (no-returns (cdr direct-intervals) ideal-ratio large-interval-def))
				(let ((valore (rapporto-recuperi elemento large-interval-def)))
					(cons (riscala valore ideal-ratio) (no-returns (cdr direct-intervals) ideal-ratio large-interval-def)))))))
			
;;;;;;;;;; UTILITA'
(defun rapporto-recuperi (lista int-largo)
  (setf recuperati 0 salti-larghi 0)
	(labels ((conta-recuperi-e-larghi (intervalli massimo)
				(let ((salto (car intervalli)))
					(cond ((null salto) (values recuperati salti-larghi))
		 	  		      ((> (abs salto) massimo) (cond ((recuperop salto (cdr intervalli)) (incf recuperati)
                                                                        			   (incf salti-larghi)
                                                                        			   (conta-recuperi-e-larghi (cdr intervalli) massimo))
                                                   (t (incf salti-larghi)
		 	  			  		        			  (conta-recuperi-e-larghi (cdr intervalli) massimo))))
                          (t (conta-recuperi-e-larghi (cdr intervalli) massimo))))))
          (conta-recuperi-e-larghi lista int-largo))
    (divcon0 recuperati salti-larghi))
    
(defun recuperop (a lista)
	(let ((b (car lista)))
	  (if b (compresop b a 0))))
;*********************************************************************************************************************************;	  
	  
	  
	  
;*********************************************************************************************************************************;	  
;;;;;;;;;;;;;; l'inverso del numero di ripetizioni della nota più alta  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! climax-strength ((pitches list) (ideal-ratio float))
   :initvals '(() 0.61)
   :indoc '("list of intervals" "ratio")
   :numouts 1
   :icon 242
   :doc "calcola l'inverso del numero di ripetizioni della nota più alta. Il valore 1 indica una sola ripetizione
   Inputs (left to right)
   intervals. list of list of pitches
   ideal-ratio. Ideal ratio value (default 0.61)
   output. list of fitness values"
	(if (null pitches)
		nil
		(let ((elemento (car pitches)))
			(if (null elemento)
				(cons 1.0 (climax-strength (cdr pitches) ideal-ratio))
				(let ((valore (/ 1 (ripetizioni-climax elemento))))
					(cons (riscala valore ideal-ratio) (climax-strength (cdr pitches) ideal-ratio)))))))

;;;;;;;;;; UTILITA'			
(defun ripetizioni-climax (lista)
	(count (reduce #'max lista) lista))
;*********************************************************************************************************************************;

	


;*********************************************************************************************************************************;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; funzioni per giudicare i ritmi ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;*********************************************************************************************************************************;


                    
                                      
                               
;;;;;;;;;;;;;;;;;; conta le sincopi in un ritmo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! syncopation ((dur-values list) (tactus rational) (ideal-ratio float))
   :initvals '(() 1/4 0.1)
   :indoc '("list of rhithmic values" "tactus" "ideal ratio")
   :numouts 1
   :icon 242
   :doc "calcola il numero di sincopi presenti
   Inputs (left to right)
   dur-values. list of rhythmic durations
   tactus. default 1/4
   ratio. ideal value
   output. list of fitness values for rhythm in cromosomes"
   (if (null dur-values)
   		nil
   		(let* ((elemento (car dur-values))
   			   (valore (/ (sincope (unisci-pause elemento) tactus) (length elemento))))
   			(cons (riscala valore ideal-ratio) (syncopation (cdr dur-values) tactus ideal-ratio)))))
 
 ;;;;;;;;UTILITA'  			  	  
 (defun toglie-ecc-al-primo (lista tact)
              (let ((ridotto (- (car lista) tact)))
                (labels ((riduci (a)
                           (if (> a tact)
                               (riduci (- a tact))
                             (setf ridotto a))))
                  (riduci ridotto))
                (setf lista (cons ridotto (cdr lista)))))
                
(defun sincope (tempi tactus)
              (if (null tempi)
                  0
                (let ((nota1 (car tempi)))
                  (cond ((= nota1 tactus) (sincope (cdr tempi) tactus))     
                        ((> nota1 tactus) (sincope (toglie-ecc-al-primo tempi tactus) tactus))
                        (t (labels ((accumula (nota valori limite)
                                      (If (null valori)
                                          0
                                        (let ((somma (+ nota (car valori))))
                                          (cond ((multiplop somma limite) (sincope (cdr valori) limite))
                                                ((< somma limite) (accumula somma (cdr valori) limite))
                                                (t (+ 1 (accumula somma (cdr valori) limite))))))))
                             (accumula nota1 (cdr tempi) tactus)))))))
                             
 (defun unisci-pause (lista)
              (if (null lista)
                  nil
                (let ((primo (car lista)))
                  (if (> primo 0)
                      (cons primo (unisci-pause (cdr lista)))
                    (labels ((somma (pausa resto)
                               (if (null resto)
                                   (cons pausa (unisci-pause resto))
                                 (let ((secondo (car resto)))
                                   (if (> secondo 0)
                                       (cons pausa (cons secondo (unisci-pause (cdr resto))))
                                     (somma (+ pausa secondo) (cdr resto)))))))
                      (somma primo (cdr lista)))))))
;;;;;;;;;;;;;;;;;;;; *********************************************************** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                      
;*********************************************************************************************************************************;
;;;;;;;;;;;; calcola la proporzione di suono-silenzio ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! sound-silence-ratio ((durations list) (ideal-ratio float))
   :initvals '(() 0.85)
   :indoc '("people" "ratio")
   :numouts 1
   :icon 242
   :doc "calcola la proporzione di tempi di suono sulla durata totale
   Inputs (left to right)
   cromosomes. list of durations
   ideal-ratio. Ideal ratio value (default 0.85)
   output. list of fitness values"
                  (if (null durations)
                      nil
                    (let* ((elemento (car durations)) 
                    	   (suoni (conta-note elemento))
                    	   (totale (reduce #'+ elemento :key 'abs)))
                    	 (cons (riscala (/ suoni totale) ideal-ratio)
                    	 	   (sound-silence-ratio (cdr durations) ideal-ratio)))))
                    	
;;;;;;;;;; UTILITA' ;;;;;;;;;;;;;;;;;;;;;
(defun conta-note (lista-durate)
              (if (null lista-durate)
                  0
                (let ((elemento (car lista-durate)))
                  (if (> elemento 0)
                      (+ elemento (conta-note (cdr lista-durate)))
                    (conta-note (cdr lista-durate))))))	
;*********************************************************************************************************************************;


;********CONTROLLARE***********************************************************************************************************;
;;;;;;;;;;;; giudica la quantità di diversi valori di durata usati ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! rhythmic-variety ((durations list) (variety real))
   :initvals '(() 4)
   :indoc '("list of list of rhythmic values" "number of different rhythm values")
   :numouts 1
   :icon 242
   :doc "calcola il rapporto fra i diversi valori ritmici usati ed il numero dei valori ritmici voluti
   Inputs (left to right)
   durations. list of list of regular values 
   variety. best number of different rhythmic values
   output. list of fitness values"
	(if (null durations)
		nil
		(let ((durate-attuali (car durations)))
			(if (null durate-attuali)
				(cons 1.0 (rhythmic-variety (cdr durations) variety))
				(cons (riscala1 (quanti-diversi-assoluti durate-attuali) variety) (rhythmic-variety (cdr durations) variety))))))
			
;;;;;;;;;; UTILITA'
(defun riscala1 (valore giusto)
                   (if (> valore giusto)
                       (- 1.0 (/ giusto valore))
                     (- 1.0 (/ valore giusto))))
                     
                     
;;;;; assoluti non fa distinzione fra note e pause ;;;;;     
(defun quanti-diversi-assoluti (lista)
      (length (remove-duplicates (mapcar #'abs lista))))
      
(defun quanti-diversi (lista)
	(if (null lista)
		0
      (length (remove-duplicates lista))))
;*********************************************************************************************************************************;	   




;**********CONTROLLARE*******************************************************************************************************;
;;;;;;;;;;;; calcola la gamma dei valori di durata usati. 4 significa che ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  il valore ritmico più grande è 4 volte il più piccolo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! rhythmic-range ((durations list) (range real))
   :initvals '(() 4)
   :indoc '("list of list of rhythmic values" "best rhythmic escursion")
   :numouts 1
   :icon 242
   :doc "calcola e giudica la gamma ritmica
   Inputs (left to right)
   durations. list of list of regular values
   range.
   output. list of fitness values"
	(if (null durations)
		nil
		(let ((durate-attuali (car durations)))
			(if (null durate-attuali)
				(cons 1.0 (rhythmic-range (cdr durations) range))
				(cons (riscala1 (escursione-ritmica-assoluti durate-attuali) range) (rhythmic-range (cdr durations) range))))))
			
;;;;;;;;;; UTILITA'
;;;;; assoluti non fa distinzione fra note e pause ;;;;;     
(defun escursione-ritmica-assoluti (lista)
	(let ((tutti-diversi (remove-duplicates (mapcar #'abs lista))))
		(/ (apply #'max tutti-diversi) (apply #'min tutti-diversi))))
		
(defun escursione-ritmica (lista)
	(if (null lista)
	0
	(let ((tutti-diversi (remove-duplicates lista)))
		(/ (apply #'max tutti-diversi) (apply #'min tutti-diversi)))))
;*********************************************************************************************************************************;


 
;;;********************* CONTROLLA LA DURATA DELLE FRASI ***************************; 
(defmethod! duration-control ((cromosomes list) (ideal-dur rational))
	:initvals '(() 4/4)
   	:indoc '("list of durations" "ideal-duration")
   	:numouts 1
   	:icon 242
   	:doc "calcola la durata.
   	Inputs (left to right)
   	cromosomes. list of list of events
   	ideal-dur. duration needed
   	output. list of fitness values"
			(cond ((null cromosomes)
					nil)
				  ((atom (caar cromosomes))
				   (mapcar #'(lambda (x)
				  	  				(riscala-errore (abs (- ideal-dur 
				  	  										(durata-durate x)))))
				  	  		  cromosomes))
				  (t (mapcar #'(lambda (x)
				  	  				(riscala-errore (abs (- ideal-dur
				  	  										(durata-cromosomi x)))))
				  	  		  cromosomes))))
				  	
(defun durata-cromosomi (cromosoma)
	(reduce #'+ (mapcar #'car cromosoma) :key #'abs))
                 	   
(defun durata-durate (l-durate)
	(reduce #'+ l-durate :key 'abs))
	
(defun riscala-errore (elemento)
	(- 1 (/ 1.0 (+ 1 elemento))))
                 
;*********************************************************************************************************************************;
;*********************************************************************************************************************************;                          
              
  	
  	
  	



              
  	