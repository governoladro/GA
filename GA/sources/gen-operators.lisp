
(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;				BINARY OPERATORS					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(om::defmethod! binary-mutation ( (binary-cromosomes list) (prob float))
    :initvals '( '(0) 0.01)
    :indoc '("cromosomes" "mutation probability")
    :icon 230
    :doc "Muta gli alleli dei cromosomi presenti in <binary-cromosomes> con probabilit <mutation probability>"
    (defun muta-valore (n) (if (= n 1) 0 1))
    (defun mutazione (lista prob)
      (mapcar (lambda (x)
                (if (> prob (r1))
                  (muta-valore x)
                  x)) lista))
    (mapcar (lambda (x) (mutazione x prob)) binary-cromosomes))


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
    :indoc '("cromosomes" "mutation probability" "maximum range of intervals")
    :icon 230
    :doc "cambia i valori in una lista con probabilit pari a <mutation probability>
    <max-int  value massimo, step  il moltiplicatore. defoult 100"                       
    (mapcar #'(lambda (x) (muta-valore2 x prob max-int step)) l-of-values))

                            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;********************************************************************************************;  
;; muta l'altezza aggiungendo o togliendo 1 o una quinta                             


(om::defmethod! pitch-mutation ( (cromosomes list) (prob float) (mutation-type symbol))
    :initvals '( '(0) 0.01 'normal-add)
    :indoc '("list" "mutation probability" "mutation-type")
    :menuins '((2 (("normal" 'normal-add) ("fifth" 'fifth-add))))
    :icon 230
    :doc "muta le altezze in crom secondo la probabilit prob. Se mutation-type  normal
    aggiunge o toglie 1, altrimenti aggiunge o toglie 7 o 2. Si sposta cio di
    una o due quinte sopra o sotto, ma restando nell'ottava"                       
	(if (null cromosomes)
		nil
		(let ((elemento (car cromosomes)))
			(cond ((null elemento) (cons nil (pitch-mutation (cdr cromosomes) prob mutation-type)))
				  (t (cons (mutazione elemento prob mutation-type) (pitch-mutation (cdr cromosomes) prob mutation-type)))))))

;;;;;;;;;; Utilit
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
(om::defmethod! rhythmic-mutation ( (cromosomes list) (prob float) )
    :initvals '( '(0) 0.01)
    :indoc '("list" "mutation probability")
    :icon 230
    :doc "muta le durate in crom secondo la probabilit prob."                                
    (cond ((null cromosomes) nil)
          (t (cons (mutazione-ritmica (car cromosomes) prob)
                   (rhythmic-mutation (cdr cromosomes) prob)))))

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


;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;***************************************************;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;***************** RETROGRADO ************************;
(om::defmethod retro ((cromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 nil)
    :indoc '("list" "mutation probability")
    :icon 230
    :doc "restitusce il retrogrado dei cromosomi con probabilit prob.
    Col valore opzionale <section> restituir il retrogrado di una porzione del cromosoma a partire da un punto
    casuale fra <section> e la fine del cromosoma" 
               (cond ((null cromosomes) nil)
                     ((> (r1) prob) (cons (car cromosomes)
                                          (retro (cdr cromosomes) prob section)))
                     ((null section) (cons (reverse (car cromosomes))
                                         (retro (cdr cromosomes) prob section)))
                     (t (cons (retrogrado-da-a (car cromosomes))
                              (retro (cdr cromosomes) prob section)))))
                              
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
(om::defmethod! inverted ((cromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 nil)
    :indoc '("list" "mutation probability")
    :icon 230
    :doc "restitusce l'inverso dei cromosomi con probabilit prob.
    Col valore opzionale <section> restituir l'inverso di una porzione del cromosoma a partire da un punto
    casuale fra <section> e la fine del cromosoma" 
               (if (null cromosomes)
               		nil
               		(let ((elemento (car cromosomes)))
               		  (cond ((> (r1) prob) (cons elemento
                                           		 (inverted (cdr cromosomes) prob section)))
                     		((null section) (cons (inversione elemento (delta (note elemento)))
                                         		(inverted (cdr cromosomes) prob section)))
                    	    (t (cons (inversione-da-a elemento)
                              		 (inverted (cdr cromosomes) prob section)))))))

                            
;UTILITA'
;i-primi
;r1
                              
(defun inversione-da-a (elemento)
			  (let ((lunghezza (length elemento)))
			  	(if (> lunghezza 2)
              		(let* ((partenza (random (1- lunghezza)))
              			   (sezione (nthcdr partenza elemento)))
                	   (append (i-primi partenza elemento)
                        	   (inversione sezione (delta (note sezione)))))
                    (inversione elemento))))
                    
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
                  
                            
;;;;;;restituisce le altezze                        
(defun note (lista)
               (remove nil (mapcar #'second lista)))
               
(defun delta (lista)
                   (mapcar #'- lista (cdr lista)))               


;************** RETROGRADO DELL'INVERSO ************************************;
(om::defmethod! re-ro ((cromosomes list) (prob float) &optional section)
	:initvals '( () 0.05 nil)
    :indoc '("list" "mutation probability")
    :icon 230
    :doc "restitusce il retrogrado dell'inverso dei cromosomi con probabilit prob.
    Col valore opzionale <section> restituir il re-ro di una porzione del cromosoma a partire da un punto
    casuale fra <section> e la fine del cromosoma" 
	(if (null cromosomes)
		nil
		(let ((elemento (car cromosomes)))
			(cond ((> (r1) prob) (cons elemento
                                       (re-ro (cdr cromosomes) prob section)))
                  ((null section) (cons (reverse (inversione elemento (delta (note elemento))))
                                      (re-ro (cdr cromosomes) prob section)))
                  (t (cons (re-ro-da-a elemento)
                           (re-ro (cdr cromosomes) prob section)))))))
                           
                         
(defun re-ro-da-a (elemento)
	(let ((lunghezza (length elemento)))
			  	(if (> lunghezza 2)
              		(let* ((partenza (random (1- lunghezza)))
              			   (sezione (nthcdr partenza elemento)))
                	   (append (i-primi partenza elemento)
                        	   (reverse (inversione sezione (delta (note sezione))))))
                    (reverse (inversione elemento (delta (note elemento)))))))
                    
                    
;***************TRASPOSIZIONE*******************************************************;

(om::defmethod! transposition ((cromosomes list) (prob float) (mutation-type symbol) &optional section)
	:initvals '( '() 0.05 'fifth-add nil)
  	:indoc '("parents" "mutation probability" "mutation-type of mutation")
    :menuins '((2 (("normal" 'normal-add) ("fifth" 'fifth-add))))
    :icon 230
    :doc "Incrocia una lista di genitori <parents> con una probabilit pari a <cross probability>. 
    <number of cross points> stabilisce quanti punti di incrocio devono esserci, solitamente 2"
              (cond ((null cromosomes) nil)
                    ((> (r1) prob)
                     (cons (car cromosomes) (transposition (cdr cromosomes) prob mutation-type section)))
                    ((null section)
                     (cons (trasponi (car cromosomes) mutation-type)
                           (transposition (cdr cromosomes) prob mutation-type section)))
                    (t (cons (trasponi-da-a (car cromosomes) mutation-type)
                             (transposition (cdr cromosomes) prob mutation-type section)))))

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
(om::defmethod! classic-cross ( (cromosomes list) (perc float) (cross-point integer) )
	:initvals '( '() 0.65 2)
    :indoc '("parents" "cross probability" "number of cross points")
    :icon 230
    :doc "Incrocia una lista di genitori <parents> con una probabilit pari a <cross probability>. 
    <number of cross points> stabilisce quanti punti di incrocio devono esserci, solitamente 2"
   (cond ((null cromosomes) nil)
       	 ((< (r1) perc) (append (incrocio (car cromosomes) (second cromosomes) cross-point)
                            	(classic-cross (nthcdr 2 cromosomes) perc cross-point)))
         (t (cons (car cromosomes) (classic-cross (cdr cromosomes) perc cross-point)))))

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


