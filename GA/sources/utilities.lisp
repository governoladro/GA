;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Funzioni utili ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(inline r1 casuale+- casuale-tra divisione-modulo conta-i-positivi abs1 non-presente
			ordina-indici i-primi divcon0 log0 multiplop compresop riscala))

;;;;;;;;;;;;; da un numero casuale fra 0 e 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun r1 ()
    (/ (random 1000) 1000.0))
    
;;;;;;;;;;;;; restituisce a caso il numero 1 oppure -1 ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; prop indica la percentuale di 1 contro -1 ;;;;;;;;;;;;;;;;;
(defun casuale+- (prob)
    (let ((num (r1)))
      (if (> prob num)
        1
        -1)))
      
;;;;;;;;;;;;; restituisce un numero casuale fra min e max ;;;;;;;;;;;;;;;
(defun casuale-tra (min max)
              (let ( (range (- max min)) )
                (if (<= range 0)
                    (setf num 0)
                  (setf num (random (+ 1 range))))
                (+ num min)))
      
;;;;;;;;;;;;; restituisce l'intero pi vicino ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; della divisione di num / modulo ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun divisione-modulo (num modulo)
      (first (list (round num modulo))))

;;;;;;;;;;;;;; conta i numeri positivi di una lista ;;;;;;;;;;;;;;;;;;;;;
(defun conta-i-positivi (lista)
    (if (null lista)
      0
      (if (> (car lista) 0)
        (+ 1 (conta-i-positivi (cdr lista)))
        (conta-i-positivi (cdr lista)))))         
               
;;;;; valore assoluto solo quando c' ;;;;;;;;;;;;;;;;;;;            
(defun abs1 (a)
              (if a
                  (abs a)))               
               
;;;;;;;;; da vero se l'elemento non c' in lista, falso altrimenti ;;;;;;;;;;;;;;;;;;;;;;;;
(defun non-presente (ele lista)
              (cond ((null lista) t)
                    ((member ele lista) nil)
                    (t nil)))
                    

;;;;;;;; restituisce la lista degli indici degli elementi di lista dal pi grande al pi piccolo                    
(defun ordina-indici (lista)
       (let ((limite (length lista)))
         (labels ((ordina (l conto)
                    (let ((maggiore (position (reduce #'max l) l)))
                      (if (> conto limite)
                        nil
                        (cons maggiore (ordina (substitute -1 maggiore l :count 1) (+ 1 conto)))))))
           (ordina lista 1))))

;;;;;;;;;;;;;;;;;;; restituisce la lista dei primi n elementi di lista ;;;;;;;;;;;;;;;;;;;           
(defun i-primi (n lista)
              (if (= 0 n)
                  nil
                (cons (car lista) (i-primi (- n 1) (cdr lista)))))

;;;;;;;;; divisione protetta (permette la divisione per 0) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun divcon0 (a b)
              (if (= 0 b)
                  0
                (/ a b)))

;;;;;;;;;; logaritmo di 0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun log0 (n base)
      (if (= 0 n)
        0
        (log n base)))                     
                    
;;;;;;;;;;;;;;;;;;;;;;;;; restituisce t se a  multiplo di b, nil altrimenti ;;;;;;;;;;;;;;;;;;;;;;;;
(defun multiplop (a b)
                   (integerp (/ a b)))   
                                   
;;;;;;;;;;;;;;;;;;;;;;;; restituisce t se tra  compreso fra val1 e val2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun compresop (tra val1 val2)
	(if (> val1 val2)
		(and (> tra val2) (< tra val1))
		(and (< tra val2) (> tra val1))))
		                     
;;;;;;;;;;;;;; riscala i valori di lista fra 0 e 1. zero  il valore ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; che nella lista riscalata deve essere uguale a 0 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; il valore pi lontano possibile da questo zero sarˆ 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Es.: se zero = 0.6 0.7 diventa 0.5, 0.6 diventa 0 e 0 diventa 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun riscala (valore zero)
    (let ((nuovo-valore (abs (- zero valore))))
      (if (> zero 0.5)
        (divcon0 nuovo-valore zero)
        (divcon0 nuovo-valore (- 1 zero)))))

        

  
(defmethod! dur-not-union ((l1 list) (l2 list))
   :initvals '(() ())
   :indoc '("durations list" "notes list")
   :icon 252
   :doc "takes a list of durations and a list of pitches and returns a list of musical phrases"
    (if (null (car l1))
        nil
        (cons (unisci (car l1) (car l2))
            (dur-not-union (cdr l1) (cdr l2)))))
                

(defun unisci (lista1 lista2)
    (let ((ris nil) (ind 0))
                (dolist (elemento lista1 ris)
                  (cond ((< elemento 0) (push (list elemento) ris))
                        (t (push (pulisci (list elemento (nth ind lista2))) ris)
                           (setf ind (+ 1 ind))))
                  ris)
                (nreverse ris)))
                
(defun pulisci (coppia)
                   (if (null (car (cdr coppia)))
                       (list (* (car coppia) -1))
                     coppia))
                     
                     

(defmethod! separator ((chromosomes list))
   :initvals '(())
   :indoc '("chromosomes")
   :numouts 4
   :icon 252
   :doc "spilts each list in inlet in list of durations, list of pitches, list of positive durations, list of negative durations (rests)"                  
      (let ((tutto (divide chromosomes)))
      	(values (mapcar #'car tutto) (mapcar #'second tutto) (mapcar #'caddr tutto) (mapcar #'cadddr tutto))))
          
(defun separatore (crom)
	(let ((durate nil) (altezze nil) (note nil) (pause nil))
		(dolist (x crom (values (nreverse durate) (nreverse altezze) (nreverse note) (nreverse pause)))
			(let ((durata (car x)))
				(cond ((> durata 0) (push durata durate)
									(push (second x) altezze)
									(push x note))
					  (t (push durata durate)
					  	 (push durata pause)))))))

(defun divide (chromosomes)
    (if (null chromosomes)
      nil
      (let ((attuale (multiple-value-list (separatore (car chromosomes)))))
        (cons attuale (divide (cdr chromosomes))))))



(defmethod! scale1 (&rest fit-val)
   :initvals '(())
   :indoc '("fitness-values")
   :numouts 1
   :icon 252
   :doc "scales the values in input between 0 and 1; 1 is optimal value"
   (let ((divisore (length fit-val)))
      (mapcar #'(lambda (x) (- 1 (/ x divisore))) (apply #'somme fit-val))))
      
      
;;;;;;;; somma per ogni lista tutti i primi, poi i secondi ecc
;;;;;;;; es valori sono (1 2 3) (10 20 30) (100 200 300)
;;;;;;;; (111 222 333) cio (1 + 10 + 100) (2 + 20 + 200) (3 + 30 + 300)                 
(defun somme (&rest valori)
              (if (null (car valori))
                  nil
                (cons (reduce #'+ (mapcar #'car valori))
                      (apply #'somme (mapcar #'cdr valori)))))  
                      

         
;****************************************************************;                         

(defmethod! summ ((values list) &optional absolute?)
   :initvals '( () () )
   :indoc '("list-of-values" "abolute-value")
   :menuins '( (1 (("Abs" 'abs) ())))
   :numouts 1
   :icon 252
   :doc "summ toghethere the values in inlet"
    (if (null values)
      nil
      (let ((elemento (car values)))
        (if (consp elemento)
          (cons (summ elemento absolute?) (summ (cdr values) absolute?))
          (reduce #'+ values :key (if absolute? 'abs))))))   

;*********************************************************************************************************************************;

(defmethod! binToDec ((valori list))
   :initvals '( () )
   :numouts 1
   :icon 252
   :doc "convert a list of list of binary values to decimal values"
   (mapcar #'converti valori))
   
(defun converti (lista)
    (if (null lista)
    0
    (+ (* (car lista) (expt 2 (- (length lista) 1))) (converti (cdr lista)))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;; controllo verticale                          
(defmethod! vertical-control ((chromosomes list) (bass list))
   :initvals '(() ())
   :indoc '("list of chromosomes" "bass line")
   :numouts 1
   :icon 251
   :doc "returns the distance between the pitches of each phrase and the pitches of 'bass line' "
	(if (null chromosomes)
		nil
	    (cons (controlla bass (car chromosomes) 0 0) (vertical-control (cdr chromosomes) bass))))

;;;;UTILITA'			
(defun controlla (l-basso l-alto acc-b acc-a)
    (if (or (null l-basso)
            (null l-alto))
      nil									
      (let* ((nota-b (car l-basso))
             (nota-a (car l-alto))
             (dur-b (+ (abs (car nota-b)) acc-b))
             (dur-a (+ (abs (car nota-a)) acc-a)))
        (cond ((> dur-b dur-a) (cons (confronta nota-a nota-b)
                                     (controlla l-basso (cdr l-alto) 0 dur-a)))
              ((= dur-b dur-a) (cons (confronta nota-a nota-b)
                                     (controlla (cdr l-basso) (cdr l-alto) 0 0)))
              (t (cons (confronta nota-a nota-b)
                       (controlla (cdr l-basso) l-alto dur-b 0)))))))
                       
(defun confronta (a b)
              (let ((nota1 (second a))
                    (nota2 (second b)))
                (if (and nota1 nota2)
                    (- nota1 nota2)
                  0)))
;*********************************************************************************************************************************;


