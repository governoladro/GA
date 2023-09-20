;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	FUNZIONI PER IL CALCOLO DELLE IDONEITA'	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 1) prop		  							  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    Calcola l'idoneit proporzionale cioè   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    l'idoneit di un individuo divisa 	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  per la media delle idoneit			  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 2) sigma									  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    riscala le idoneit in scala sigma	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    (vedi Mitchell 1998)					  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	I valori in entrata devono gi essere	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  ordinati in modo che il valore più basso  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  sia il peggiore.						  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prop (valori-attesi)
    (let ((med (media valori-attesi)))
      (mapcar #'(lambda (x) (divcon0 x med)) valori-attesi)))

(defun sigma (valori-attesi)
    (let* ((med (media valori-attesi))
           (varianza (- (media (mapcar #'quadrato valori-attesi))
      	                (quadrato med)))
           (dev-standard (sqrt (abs varianza))))
      (if (> dev-standard 0)
        (mapcar #'(lambda (x) (+ 1 (divcon0 (- x med) (* 2 dev-standard)))) valori-attesi)
        (make-list (length valori-attesi) :initial-element 1))))

;;;;;;; utilit ;;;;;;;;;;;;        
(defun media (lista)
	(float (divcon0 (apply #'+ lista) (length lista))))
      
(defun quadrato (l)
	(* l l))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;											        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			 METODI DI SELEZIONE			 	 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;													;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	1) evolution: 									;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	   con campionamento stocastico universale (CSU);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     Invece di generare un nuovo numero casuale   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     ogni volta come nella selezione roulette, si ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 	   crea una lista di N puntatori equidistanti 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 	   un certo valore P; con N = numero di individui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     da scegliere, e P = T (la somma di tutti i   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	   valori attesi) fratto N. Il primo puntatore 	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     è un numero casuale scelto fra 0 e P   		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	2) A torneo										;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     sceglie a caso due individui e genera un	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     numero casuale r fra 0 e 1. Se r < di un	    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     certo valore di soglia (es. 0.75) si sceglie ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     l'individuo più idoneo, altrimenti l'altro.  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       											;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


		

(defmethod! evolution ((cromosomes list) (num-of-gen integer) (fitness-func function) (mutation-func function)
						(n-best-elements integer) (elite-perc integer) (scale symbol) (result symbol))
   :initvals '('() 100 '() '() 1 10 'prop 'last-generation)
   :indoc '("initial population" "number of generations" "fitness function" "mutation function" "number of best elements" 
   			"perc of unchange elements" "rescaling method" "best of all or last generation")
  :menuins '((6 (("Prop" 'prop) ("Sigma" 'sigma))) (7 (("Last generation" 'last-generation) ("All generations" 'all-generations))))
   :numouts 2
   :icon 242
   :doc "iteratore, da una popolazione iniziale esegue n generazioni le valuta, le muta e sceglie gli n migliori dell'ultima
   n-best-elements dice quanti dell'ultima generazione calcolata deve restituire, elite-perc definisce la percentuale di individui
   migliori che entrano di diritto nella generazione successiva senza essere mutati, number è il numero di individui presenti in ogni generazione.
   result
   Inputs (left to right)
   cromosomes. Cromosomes list to evaluate
   fitness-func. function used to judge the cromosomes. Best elements lowest score
   new-crom-number. how many coromosomes to choose for offspring
   punti. punti   
   Outputs (left to right)
   best. best member of generation
   fit-values. valori"
   (if (eq result 'last-generation)
    (let ((punteggio (funcall fitness-func cromosomes))
   		  (generazione cromosomes))
   			 (dotimes (i num-of-gen)
   				 (setf generazione (crea-nuova-generazione generazione punteggio mutation-func elite-perc scale))
   				 (setf punteggio (funcall fitness-func generazione))
   				 (if (= 1.0 (reduce #'max punteggio))
   				 	(return (values generazione punteggio))))
   			 (values (n-elementi+alti punteggio n-best-elements) (n-migliori-elementi punteggio generazione n-best-elements)))
    (let* ((punteggio (funcall fitness-func cromosomes))
   		   (generazione cromosomes)
   		   (l-migliori (list (n-migliori-elementi punteggio generazione n-best-elements)))
   		   (l-punti (list (n-elementi+alti punteggio n-best-elements))))
   			 (dotimes (i (1- num-of-gen)) 
   				 (setf generazione (crea-nuova-generazione generazione punteggio mutation-func elite-perc scale))
   				 (setf punteggio (funcall fitness-func generazione))
   				 (push (n-migliori-elementi punteggio generazione n-best-elements) l-migliori)
   				 (push (n-elementi+alti punteggio n-best-elements) l-punti)
   				 (if (= 1.0 (reduce #'max punteggio))
   				 	(return (values generazione punteggio))))
   			 (values (nreverse l-punti) (nreverse l-migliori)))))

 
;;;;;;;; UTILITA' ;;;;;

(defun crea-nuova-generazione (popolo punti fn-mutazione perc-elite tipo-scala) 
	(let ((totali (length popolo)))
		(if (= 0 (- (reduce #'max punti) (reduce #'min punti)))
			(funcall fn-mutazione (scegli-a-caso popolo totali))
			(let* ((valori-attesi (funcall tipo-scala punti))
		  	   	   (n-elite (elite totali perc-elite))
			  	   (resto (- totali n-elite)))
		   		(append (n-migliori-elementi punti popolo n-elite)
		   				(funcall fn-mutazione (scelta popolo resto valori-attesi)))))))
	 
;;;;;; se il punteggio è 0      
(defun scegli-a-caso (lista quantit)
	(let ((ris nil))
		(dotimes (i quantit ris)
			(push (nth (random quantit) lista) ris))))


;;;;;;; calcola il numero di individui che fanno parte dell'elite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun elite (totalit percentuale)
	(if (zerop percentuale)
		0
		(round (* percentuale (/ totalit 100)))))

		
;;;;;;;;;;;;sceglie gli individui della popolazione usando gli indici trovati col metofo CSU                                  
(defun scelta (popolazione quantit valori)
      (let ((indici (trova-ind-CSU valori (r1) 0 0 quantit)))  
        (mapcar #'(lambda (x) (nth x popolazione)) indici)))


;;;; trova gli indici col CSU: lista è la ista dei valori riscalati (prop o sigma) r è un casuale fra 0 e 1
;;;;; n è la quantit di valori da trovare
(defun trova-ind-CSU (lista r ind acc n)
      (if (= n 0)
        nil
        (let ((val (+ (car lista) acc)))
          (if (>= val r)
            (cons ind (trova-ind-CSU lista (1+ r) ind acc (1- n)))
            (trova-ind-CSU (cdr lista) r (1+ ind) val n)))))

                 
;;;; per restituire gli n elementi con migliore punteggio;;;
(defun n-migliori-elementi (punti elementi n)
              (if (= 0 n)
                  nil
                (let* ((maggiore (reduce #'max punti))
                       (indice (position maggiore punti)))
                  (cons (nth indice elementi) (n-migliori-elementi (substitute -1 maggiore punti :count 1) elementi (1- n))))))
                 
(defun n-migliori-elementi (punti elementi n)
              (cond ((null elementi) nil)
              		((= 0 n) nil)
                 	(t	(let* ((maggiore (reduce #'max punti))
                       		   (indice (position maggiore punti)))
                 		  (cons (nth indice elementi) (n-migliori-elementi (substitute -1 maggiore punti :count 1) elementi (1- n)))))))
                  
                    			
;;;; per restituire gli n punteggi migliori ;;;
(defun n-elementi+alti (lista quanti)
      (cond ((null lista) nil)
            ((= 0 quanti) nil)
            (t (let ((migliore (reduce #'max lista)))
                 (cons migliore (n-elementi+alti (remove migliore lista :count 1) (1- quanti)))))))
                 

;;;;;;;;;;;;;;;;;;;;; stato stazionario si sostituiscono gli n-peggiori elementi con una nuova generazione di n-migliori
;;;;;;;;;;;;;;;;;;;;; gli altri non si toccano
(defmethod! evolution-stationary ((cromosomes list) (num-of-gen integer) (fitness-func function) (mutation-func function)
						(n-best-elements integer) (elite-perc integer) (scale symbol) (result symbol))
   :initvals '('() 100 '() '() 1 10 'prop 'last-generation)
   :indoc '("initial population" "number of generations" "fitness function" "mutation function" "number of best elements" 
   			"perc of unchange elements" "rescaling method" "best of all or last generation")
   :menuins '((6 (("Prop" 'prop) ("Sigma" 'sigma))) (7 (("Last generation" 'last-generation) ("All generations" 'all-generations))))
   :numouts 2
   :icon 242
   :doc "iteratore, da una popolazione iniziale esegue n generazioni le valuta, le muta e sceglie gli n migliori dell'ultima
   n-best-elements dice quanti dell'ultima generazione calcolata deve restituire, elite-perc definisce la percentuale di individui
   migliori che sostituiscono gli individui peggiori (stato stazionario), number è il numero di individui presenti in ogni generazione.
   result
   Inputs (left to right)
   cromosomes. Cromosomes list to evaluate
   fitness-func. function used to judge the cromosomes. Best elements lowest score
   new-crom-number. how many coromosomes to choose for offspring
   punti. punti   
   Outputs (left to right)
   best. best member of generation
   fit-values. valori"
   (if (eq result 'last-generation)
    (let ((punteggio (funcall fitness-func cromosomes))
   		  (generazione cromosomes))
   			 (dotimes (i num-of-gen)
   				 (setf generazione (nuova-generazione-stato-stazionario generazione punteggio mutation-func elite-perc scale))
   				 (setf punteggio (funcall fitness-func generazione))
   				 (if (= 1.0 (reduce #'max punteggio))
   				 	(return (values generazione punteggio))))
   			 (values (n-elementi+alti punteggio n-best-elements) (n-migliori-elementi punteggio generazione n-best-elements)))
    (let* ((punteggio (funcall fitness-func cromosomes))
   		   (generazione cromosomes)
   		   (l-migliori (list (n-migliori-elementi punteggio generazione n-best-elements)))
   		   (l-punti (list (n-elementi+alti punteggio n-best-elements))))
   			 (dotimes (i (1- num-of-gen)) 
   				 (setf generazione (nuova-generazione-stato-stazionario generazione punteggio mutation-func elite-perc scale))
   				 (setf punteggio (funcall fitness-func generazione))
   				 (push (n-migliori-elementi punteggio generazione n-best-elements) l-migliori)
  				 (push (n-elementi+alti punteggio n-best-elements) l-punti)
   				 (if (= 1.0 (reduce #'max punteggio))
   				 	(return (values generazione punteggio))))
   			 (values (nreverse l-punti) (nreverse l-migliori)))))
   			 
(defun nuova-generazione-stato-stazionario (popolo punti fn-mutazione perc-tenuti-e-eliminati tipo-scala) 
	(let ((totali (length popolo)))
		(if (= 0 (- (reduce #'max punti) (reduce #'min punti)))
			(funcall fn-mutazione (scegli-a-caso popolo totali))
			(let* ((valori-attesi (funcall tipo-scala punti))
		  	   	   (n-tenuti (elite totali perc-tenuti-e-eliminati))
			  	   (n-eliminati n-tenuti))
		   		(append (funcall fn-mutazione (n-migliori-elementi punti popolo n-tenuti))
		   				(togli-peggiori valori-attesi popolo n-eliminati))))))
		   				
	 
;;;;;;; restituisce tutti senza gli n peggiori
(defun togli-peggiori (punti tutti n-togliere)
         (let ((indici-da-tenere (butlast (ordina-indici punti) n-togliere)))
         	(mapcar #'(lambda (x) (nth x tutti)) indici-da-tenere)))
         	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;; 3) metodo a torneo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod! evolution-turneament ((cromosomes list) (num-of-gen integer) (fitness-func function) (mutation-func function)
						(n-best-elements integer) (threshold float) (result symbol))
   :initvals '('() 100 '() '() 1 0.75 'last-generation)
   :indoc '("initial population" "number of generations" "fitness function" "mutation function" "number of best elements" 
   			"threshold" "best of all or last generation")
   :menuins '((6 (("Last generation" 'last-generation) ("All-generations" 'all-generations))))
   :numouts 1
   :icon 242
   :doc "iteratore, da una popolazione iniziale esegue n generazioni le valuta, le muta e sceglie gli n migliori dell'ultima
   n-best-elements dice quanti dell'ultima generazione calcolata deve restituire. La selezione è a torneo e threshold è il valore di soglia
   superato il quale si sceglie l'individuo peggiore, number è il numero di individui presenti in ogni generazione.
   result
   Inputs (left to right)
   cromosomes. Cromosomes list to evaluate
   fitness-func. function used to judge the cromosomes. Best elements lowest score
   new-crom-number. how many coromosomes to choose for offspring   
   Outputs (left to right)
   best. best member of generation
   Use the stocastic universal sampling on a normalized fitness (see Koza). low fitness values are better."
   (if (eq result 'last-generation)
   	   (solo-ultima-torneo cromosomes num-of-gen fitness-func mutation-func threshold n-best-elements)
   	   (di-tutte-torneo cromosomes num-of-gen fitness-func mutation-func threshold n-best-elements)))


;;;;;;;;;;;; utilit
;;;;;;;;;;;;;;;;;;;;;;; restituisce una lista con i migliori n individui dell'ultima generazione ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun solo-ultima-torneo (popolazione num-generazioni fn-fitness fn-mutazioni soglia n-migliori)
	(let* ((quanti-individui (length popolazione)))
		(dotimes (i num-generazioni popolazione)
			(let ((punteggio (funcall fn-fitness popolazione)))
				(if (= 1.0 (reduce #'max punteggio))
   				 	(return (values popolazione punteggio))
			 		(setf popolazione (funcall fn-mutazioni (generazione-torneo popolazione punteggio soglia quanti-individui))))))
		(n-migliori-elementi (funcall fn-fitness popolazione) popolazione n-migliori)))
		
		


(defun di-tutte-torneo (popolazione num-generazioni fn-fitness fn-mutazioni soglia n-migliori)
	(let* ((quanti-individui (length popolazione))
		   (migliori-della-generazione (if (> num-generazioni 0)
											nil
										   (list (n-migliori-elementi (funcall fn-fitness popolazione) popolazione n-migliori)))))
		(dotimes (i num-generazioni popolazione)
			(let ((punteggio (funcall fn-fitness popolazione)))
				(if (= 1.0 (reduce #'max punteggio))
   				 	(return (push (n-migliori-elementi punteggio popolazione n-migliori) migliori-della-generazione))
			  		(setf popolazione (funcall fn-mutazioni (generazione-torneo popolazione punteggio soglia quanti-individui)))) ;;;;; calcola la nuova generazione
			  	(push (n-migliori-elementi punteggio popolazione n-migliori) migliori-della-generazione))) ;;;; prende i migliori			  
		(nreverse migliori-della-generazione)))

		
;;;;;;;;;;;;;;; una nuova generazione scelta col sistema torneo 
(defun generazione-torneo (popolazione punteggio soglia numero-individui)
      (let ((generazione nil))
        (dotimes (i numero-individui generazione)
          (let* ((indice1 (random numero-individui))
                 (indice2 (casuale-tranne indice1 numero-individui))
                 (r (r1)))
            (push (scelta-torneo indice1 indice2 punteggio r popolazione soglia)
                  generazione)))))


;;;;;;;;;;;;;;;; sceglie fra due individui con metodo a torneo
(defun scelta-torneo (ind1 ind2 punteggio r popolazione soglia)
      (let ((idoneit1 (nth ind1 punteggio))
            (idoneit2 (nth ind2 punteggio)))
        (if (> soglia r)
          (if (> idoneit1 idoneit2) ;;;;; sceglie il più idoneo
          	  (nth ind1 popolazione)
          	  (nth ind2 popolazione))
          (if (> idoneit1 idoneit2) ;;;; altrimenti sceglie il meno idoneo
          	  (nth ind2 popolazione)
          	  (nth ind1 popolazione)))))
          

;;;;;;;;;;;; genera un numero casuale fra 0 e valore-massimo ma diverso da x ;;;;;;;
(defun casuale-tranne (x valore-massimo)
    (let ((r (random valore-massimo)))
      (if (= r x)
        (casuale-tranne x valore-massimo)
        r)))
                                    	   	


