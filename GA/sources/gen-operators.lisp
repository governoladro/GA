
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;				BINARY OPERATORS					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(om::defmethod! binary-mutation ( (binary-chromosomes list) (prob float))
    :initvals '( () 0.01)
    :indoc '("chromosomes" "mutation probability")
    :icon 250
    :doc " 'binary-chromosomes' mutation"
    (defun muta-valore (n) (if (= n 1) 0 1))
    (defun mutazione (lista prob)
      (mapcar (lambda (x)
                (if (> prob (r1))
                  (muta-valore x)
                  x)) lista))
    (mapcar (lambda (x) (mutazione x prob)) binary-chromosomes))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;				REAL VALUES OPERATORS				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mutazione2 (intervallo salto passo)
      (let ((trasposizione (* (- (random (* 2 salto)) salto) passo)))
        (+ intervallo trasposizione)))
    
(defun muta-valore2 (l-intervalli probabilit salto-massimo passo)
	(mapcar #'(lambda (x) (if (< (r1) probabilit)
                            (mutazione2 x salto-massimo passo)
                            x)) l-intervalli))


(om::defmethod! numbers-mutation ( (l-of-values list) (prob float) (max-int integer) (step real) )
    :initvals '( '(0) 0.01 3 100)
    :indoc '("chromosomes" "mutation probability" "maximum range of intervals" "intervals range multiplicator")
    :icon 250
    :doc "change values adding k * step, where k is  a random number between -max-int and +max-int"                       
    (mapcar #'(lambda (x) (muta-valore2 x prob max-int step)) l-of-values))

                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;********************************************************************************************;  
;; muta l'altezza aggiungendo o togliendo 1 o una quinta                             


(om::defmethod! pitch-mutation ( (chromosomes list) (prob float) (mutation-type symbol))
    :initvals '( () 0.01 'normal-add)
    :indoc '("list" "mutation probability" "mutation-type")
    :menuins '((2 (("normal" 'normal-add) ("fifth" 'fifth-add))))
    :icon 250
    :doc "pitch mutation adding +/- 1 or +/- 7 or +/- 2"                       
	(if (null chromosomes)
		nil
		(let ((elemento (car chromosomes)))
			(cond ((null elemento) (cons nil (pitch-mutation (cdr chromosomes) prob mutation-type)))
				  (t (cons (mutazione elemento prob mutation-type) (pitch-mutation (cdr chromosomes) prob mutation-type)))))))

;;;;;;;;;; Utilità
;;r1

(defun mutazione (individuo perc tipo)
	(cond ((null individuo) nil)
		  ((> perc (r1)) (cons (muta-altezza (car individuo) tipo) (mutazione (cdr individuo) perc tipo)))
		  (t (cons (car individuo) (mutazione (cdr individuo) perc tipo)))))
		  
(defun muta-altezza (elemento tipologia)
                   (if (consp elemento)
                       (let ((durata (car elemento)))
                         (if (< durata 0)
                             (list (* durata -1) 0)
                           (list durata (+ (funcall tipologia) (second elemento)))))
                     (+ elemento (funcall tipologia))))

;;;;;;;;;;;;;; mutazione a quinte altezze ;;;;;;;;;;;;;;;;;;
(defun fifth-add ()
              (let ((quantit (random 4)))
                (case quantit
                  (0 7)
                  (1 -7)
                  (2 2)
                  (3 -2))))
                  
(defun normal-add ()
    (let ((x (random 2)))
       (if (> x 0)
           1
          -1)))
;********************************************************************************************; 





;;;;;;;;;;;;;;;;;; muta i valori di durata ;;;;;;;;;;;;;;;;;;;;;;;;;;                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;********************************************************************************************;
(om::defmethod! rhythmic-mutation ( (chromosomes list) (prob float) )
    :initvals '( () 0.01)
    :indoc '("list" "mutation probability")
    :icon 250
    :doc "rhytmic mutation operator."                                
    (cond ((null chromosomes) nil)
          (t (cons (mutazione-ritmica (car chromosomes) prob)
                   (rhythmic-mutation (cdr chromosomes) prob)))))

(defun mutazione-ritmica (individuo probabilit)
                   (cond ((null individuo) nil)
                         ((> probabilit (r1)) (cons (chiama-mutazione-ritmo (car individuo))
                                                     (mutazione-ritmica (cdr individuo) probabilit)))
                         (t (cons (car individuo)
                                  (mutazione-ritmica (cdr individuo) probabilit)))))

(defun chiama-mutazione-ritmo (elemento)
                   (if (atom elemento)
                       (muta-durata elemento)
                     (let ((durata (car elemento)))
                       (if (> durata 0)
                           (list (muta-durata durata) (second elemento))
                         (list (muta-durata durata))))))
                         
(defun muta-durata (a)
               (if (= (random 2) 1)
                  (* a 2)
                (/ a 2)))
;********************************************************************************************;      



(om::defmethod! rhythmic-mutation-add ( (chromosomes list) (prob float) (value rational))
    :initvals '( () 0.01 1/16)
    :indoc '("list" "mutation probability" "min add/sub value")
    :icon 250
    :doc "adds or subtracts a "value" from durations in input by a probability "prob"."
    (cond ((null chromosomes) nil)
          (t (cons (aggiunta-ritmica (car chromosomes) prob value)
                   (rhythmic-mutation-add (cdr chromosomes) prob value)))))

(defun aggiunta-ritmica (individuo probabilit valore)
                   (cond ((null individuo) nil)
                         ((> probabilit (r1)) (cons (chiama-aggiunta-ritmo (car individuo) valore)
                                                     (aggiunta-ritmica (cdr individuo) probabilit valore)))
                         (t (cons (car individuo)
                                  (aggiunta-ritmica (cdr individuo) probabilit valore)))))

(defun chiama-aggiunta-ritmo (elemento x)
                   (if (atom elemento)
                       (aggiunta-durata elemento x)
                     (let ((durata (car elemento)))
                       (if (> durata 0)
                           (list (aggiunta-durata durata x) (second elemento))
                         (list (aggiunta-durata durata x))))))
                         
(defun aggiunta-durata (a min)
              (if (= (random 2) 1)
                  (let ((pippo (- a min)))
                    (if (= 0 pippo)
                        a
                      pippo))
                (let ((pippo (+ a min)))
                  (if (= 0 pippo)
                      a
                    pippo))))
                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                    
(defun suddividi (nota divisore)
                   (let ((lista ())
                         (1tempo (/ (car nota) divisore))
                         (altezza (cdr nota)))
                      (dotimes (i divisore lista)
                         (push (cons 1tempo altezza) lista))))
                           
                    
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;***************** RETROGRADO ************************;
(om::defmethod! retroGa ((chromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 nil)
    :menuins '((2 (("Yes" 'yes) ())))
    :indoc '("list" "mutation probability")
    :icon 250
    :doc "returns the retrograde of the input, with probability 'prob' " 
               (cond ((null chromosomes) nil)
                     ((> (r1) prob) (cons (car chromosomes)
                                          (retroGa (cdr chromosomes) prob section)))
                     ((null section) (cons (reverse (car chromosomes))
                                         (retroGA (cdr chromosomes) prob section)))
                     (t (cons (retrogrado-da-a (car chromosomes))
                              (retroGa (cdr chromosomes) prob section)))))
                              
;UTILITA'
;i-primi
;r1
                              
(defun retrogrado-da-a (elemento)
			  (let ((lunghezza (length elemento)))
			  	(if (> lunghezza 2)
              		(let ((partenza (random (1- lunghezza))))
                	   (append (i-primi partenza elemento)
                        	   (reverse (nthcdr partenza elemento))))
                    (reverse elemento))))
;******************************************************************************;;                        


;************* INVERSIONE *****************************************************;;
(om::defmethod! inverted ((chromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 () )
    :menuins '((2 (("Yes" 'yes) ())))
    :indoc '("list" "mutation probability")
    :icon 250
    :doc "gives the inversion of the chromosome" 
               (if (null chromosomes)
               		nil
               		(let ((elemento (car chromosomes)))
               		  (cond ((> (r1) prob) (cons elemento
                                           		 (inverted (cdr chromosomes) prob section)))
                     		((null section) (cons (inversione elemento (delta (note elemento)))
                                         		(inverted (cdr chromosomes) prob section)))
                    	    (t (cons (inversione-da-a elemento)
                              		 (inverted (cdr chromosomes) prob section)))))))

                            
;UTILITA'
;i-primi
;r1
                              
(defun inversione-da-a (elemento)
			  (let ((lunghezza (length elemento)))
			  	(if (> lunghezza 1)
              		(let* ((partenza (random (1- lunghezza)))
              			   (sezione (nthcdr partenza elemento)))
                	   (append (i-primi partenza elemento)
                        	   (inversione sezione (delta (note sezione)))))
                    elemento)))
                    
(defun inversione (cromosoma l-delta)
    (cond ((null cromosoma) nil)
          ((< (caar cromosoma) 0)
           (cons (car cromosoma) (inversione (cdr cromosoma) l-delta)))
          (t (cons (car cromosoma) (inverte (cadar cromosoma) l-delta (cdr cromosoma))))))
          
(defun inverte (altezza1 delta crom)
    (if (null delta)
      nil
      (let ((durata (caar crom)))
        (if (< durata 0)
          (cons (list durata) (inverte altezza1 delta (cdr crom)))
          (let ((nuova-altezza (+ altezza1 (car delta))))
            (cons (list durata nuova-altezza)
                  (inverte nuova-altezza (cdr delta) (cdr crom))))))))
                  
                            
;;;;;;restituisce le altezze eliminando le pause                        
(defun note (lista)
               (remove nil (mapcar #'second lista)))
 
;;;; calcola gli "intervalli" fra le altezze 
(defun delta (lista)
                   (mapcar #'- lista (cdr lista)))               


;************** RETROGRADO DELL'INVERSO ************************************;
(om::defmethod! re-ro ((chromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 nil)
    :menuins '((2 (("Yes" 'yes) ())))
    :indoc '("list" "mutation probability")
    :icon 250
    :doc "returns the retrograde of the inversion of the input, with probability "prob"" 
	(if (null chromosomes)
		nil
		(let ((elemento (car chromosomes)))
			(cond ((> (r1) prob) (cons elemento
                                       (re-ro (cdr chromosomes) prob section)))
                  ((null section) (cons (reverse (inversione elemento (delta (note elemento))))
                                      (re-ro (cdr chromosomes) prob section)))
                  (t (cons (re-ro-da-a elemento)
                           (re-ro (cdr chromosomes) prob section)))))))
                           
                         
(defun re-ro-da-a (elemento)
	(let ((lunghezza (length elemento)))
			  	(if (> lunghezza 2)
              		(let* ((partenza (random (1- lunghezza)))
              			   (sezione (nthcdr partenza elemento)))
                	   (append (i-primi partenza elemento)
                        	   (reverse (inversione sezione (delta (note sezione))))))
                    (reverse (inversione elemento (delta (note elemento)))))))
                    
                    
;***************TRASPOSIZIONE*******************************************************;


(om::defmethod! transposition ((chromosomes list) (prob float) (mutation-type symbol) &optional section)
	:initvals '( () 0.05 'fifth-add nil)
  	:indoc '("parents" "mutation probability" "mutation-type of mutation")
    :menuins '((2 (("normal" 'normal-add) ("fifth" 'fifth-add))) (3 (("Yes" 'yes) ())))
    :icon 250
    :doc "transpose the the input, with probability "prob""
              (cond ((null chromosomes) nil)
                    ((> (r1) prob)
                     (cons (car chromosomes) (transposition (cdr chromosomes) prob mutation-type section)))
                    ((null section)
                     (cons (trasponi (car chromosomes) mutation-type)
                           (transposition (cdr chromosomes) prob mutation-type section)))
                    (t (cons (trasponi-da-a (car chromosomes) mutation-type)
                             (transposition (cdr chromosomes) prob mutation-type section)))))

;;;;;UTILITA'
;r1
;fifth-add
;normal-add
                             
(defun trasponi (elemento tipo)
              (if (null elemento)
                  nil
                (let ((dur (caar elemento)))
                (if (< dur 0)
                    (cons (list dur)
                          (trasponi (cdr elemento) tipo))
                  (cons (list dur (+ (funcall tipo) (cadar elemento)))
                        (trasponi (cdr elemento) tipo))))))
                        
(defun trasponi-da-a (elemento tipo-trasp)
			  (let* ((partenza (random (length elemento)))
              		 (sezione (nthcdr partenza elemento)))
                	   (append (i-primi partenza elemento)
                        	   (trasponi sezione tipo-trasp))))
     

;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      



;************************** INCROCIO **********************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;*********************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
(om::defmethod! crossover ( (chromosomes list) (perc float) (cross-point integer) )
	:initvals '( () 0.65 2)
    :indoc '("parents" "cross probability" "number of cross points")
    :icon 250
    :doc "classic crossover operator"
   (cond ((null chromosomes) nil)
       	 ((< (r1) perc) (append (incrocio (car chromosomes) (second chromosomes) cross-point)
                            	(crossover (nthcdr 2 chromosomes) perc cross-point)))
         (t (cons (car chromosomes) (crossover (cdr chromosomes) perc cross-point)))))

(defun incrocio (a b punti)
                   (if (null b)
                       (list a)
                     (let* ((massimo (min (length a) (length b)))
                            (ris (incrocia a b massimo)))
                       (dotimes (i (- punti 1) ris)
                         (setf ris (incrocia (car ris) (second ris) massimo))))))
                       
(defun incrocia (a b limite)
               (let ((puntox (random limite)))
                 (list (append (i-primi puntox a)
                                 (nthcdr puntox b))
                         (append (i-primi puntox b)
                                 (nthcdr puntox a)))))
;;;;;;;;;;;;;;;;;;;;;;;*********************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


