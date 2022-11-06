
;; ==================================================================================== 
;;                               OMLEMPELTEXT
;; ==================================================================================== 

;;; =================================================================================== 
;;;
;;;                       © 2001  Gerard Assayag  - IRCAM
;;;                       (revision et augmentation Karim Haddad)
;;;
;;; =================================================================================== 




(in-package :om)



;;; UTILITIES

(defun rand-from-distribution  (distribution)
  "Choose an integer index given a prob. distribution -e.g. (.2 .4 .3 .1)-"
  (loop 
    with seuil = (random 1.0)
    for index from 0 to (1- (length distribution))
    for cumul = (nth 0 distribution) then (+ cumul (nth index distribution))
    while (< cumul seuil)
    finally (return index) ))


(defun inverse-distribution (distribution)
  (let* ( (inverse (mapcar #'(lambda (x) (/ 1.0 x)) distribution))
          (sum (apply #'+ inverse)))
    (mapcar #'(lambda (x) (/ x sum)) inverse)))


;;; UTILITIES



(defclass dictionary ()
  ((lzTable :initform (make-hash-table :test 'equal) :initarg :lzTable :accessor lzTable))
  )

(defmethod add ( (dict dictionary) (key t) (value t))
  ;(print key)
  (setf (gethash key (lzTable dict)) value))

(defmethod strg-member-p ((dict dictionary) ( key t ))
  (gethash key (lzTable dict)))

(defmethod get-value ((dict dictionary) ( key t ))
  (gethash key (lzTable dict)))

(defmethod pushe ( (dict dictionary) (key t) (value t))
  ;(print key)
  (push value (gethash key (lzTable dict)) ))


(defmethod print-dict ((self dictionary))
  (maphash #'(lambda (x y) (print  (list x y))) (lzTable self)))

(defmethod counter++ ((dict dictionary) ( key t ))
  (let ((value (gethash key (lzTable dict))))
    (and value
         (setf (gethash key (lzTable dict)) (incf value)))))

(defmethod mk-lz-dict ((text list) (dict dictionary))
  (loop while text
        do (loop for remaining-text on text
                 for index from 1
                 for motif  = (subseq text 0 index)
                 while (strg-member-p dict motif)
                 do (counter++ dict motif) 
                 finally 
                 (when remaining-text (add dict motif 1) )
                 (setf text (rest remaining-text))))
  dict)



(defmethod mk-continuation-dict ((lzdict dictionary))
  (let* ( (dict (make-instance 'dictionary)) )
    (maphash #'(lambda (key value)
                 (add-continuation dict key value))
             (lzTable lzdict))
    (maphash #'(lambda (key value)
                 (let ((trans (mat-trans value)))
                   (add dict key (list (first trans) (om-scale/sum (second trans) 1.0)))))
             (lzTable dict)  )
    dict))

(defmethod add-continuation ((dict dictionary) motif weight)
  (let ((prefix (or (butlast motif) T))
        (continuation (first (last motif))))
    (pushe  dict prefix (list continuation weight))))



(defmethod! string->ascii ((text t))

   :initvals '("toto")
   :indoc '("A Textfile" )
   :icon '(141) 
   :doc  "Translates strings into equivalent ascii integers"

(let ((char-text (coerce text 'list)))
  (mapcar #'(lambda (x) (char-code (character x)))
          char-text)))




(defmethod! ascii->string ((asciilist list))
   :initvals '( '(112))
   :indoc '("list of ascii numbers" )
   :icon '(141) 
   :doc  "Translates ascii integers into equivalent strings"
  (let ((aString (make-string  (length asciilist))))
    (loop for ascii in asciilist
          for index from 0 do
          (setf (elt aString index) (code-char ascii)))
    astring))






(defmethod! LZtextify ((text list) (niter integer))
   :initvals '((a b c) 1)
   :indoc '("A list of anything" "An integer")
   :icon '(230) 
   :doc  "Builds a pattern dictionary containing a statistical model'  <text>.

inputs:

text : list of anything
niter : integer >=1

output:

a <Dictionary> object.

LZtextify takes a list of anything considered as an ordered sequence.
It then builds a pattern dictionary that encodes patterns of various lengths
discovered over this sequence, as well as the conditional probabilities
that a certain pattern be followed by certain elements
If niter is greater than 1, the analysis of the sequence is iterated <niter> times,
each time skipping the next element on the left of the sequence.
niter > 1 increases the number of patterns discovered. It is equivalent to analyzing
a longer sequence, thus increasing the statistical properties (redundancy).
Empirical experience shows that niter = 4 is good value for such data as bach-like counterpoint
or jazz chorus.
"
   (let* ( (dict (make-instance 'dictionary)) )
     (loop for i from 1 to niter 
           for text2 on text
           do (mk-lz-dict text2 dict))
     (mk-continuation-dict dict)))


(defmethod! LZtextGenerate ((dict dictionary) (MaxPast integer) (maxLength integer) (mostprobable symbol))
   :initvals '(nil 5 50 T)
   :indoc '("A LZDictionary" "An integer" "An integer" "T or Nil")
   :icon '(230) 
   :doc  "Generates a new sequence using a given pattern dictionary (see LZtextify).

inputs :

dict: a <Dictionary> object generated by the LZtextify function.
maxPast : integer, >=1. size of the past window.
maxLength: integer, >=1.  length of the sequence to be generated.
mostProbable : boolean, T or Nil. generate the most probable or the least probable continuation.

output :

a list.

After building a pattern dictionary using LZtextify, LZtextGenerate may be used to
generate a new sequence that imitates the statistical behaviour encoded into the dictionary.
If a list of <something> had been analyzed by LZtextify, the result will be a new list of <something>.
At a every point of the generation, LZtextGenerate looks at the <maxPast> last elements already generated.
It looks for this subsequence into the pattern dictionary. If it finds a pattern that is 
equal to this subsequence, it checks the conditional probabilities associated with
that pattern , then generates a new element with regard to the probability. It then adds
this element to the right of the generated sequence, and iterates.
If <mostProbable> is Nil, it rather generates with egard to the inverse of the probability.
In this case, the result can be interpreted as a pattern recombination that is
very unfrequent in the original text.
"
   (loop for count from 1 to MaxLength
         for result = (list (LZtextGenerate1 dict T mostprobable))
         then (cons (loop for prefix on (reverse (first-n result MaxPast))
                          for continuation  = (LZtextGenerate1 dict prefix mostprobable)
                          while (null continuation)
                          finally (return (or continuation (LZtextGenerate1 dict T mostprobable))))
                    result)
         finally (return (nreverse result))))



#|
(defmethod LZtextGenerate1 ((dict dictionary) (prefix t) (mostprobable symbol))
;prefix contient le contexte déjà généré à prendre en compte
  (let* ((value (get-value dict prefix)))
    (and value
         (nth (rand-from-distribution (if mostprobable (second value) (inverse-distribution (second value))))
              (car value) ))))
|#


#|
(print-dict (mk-lz-dict '(a a b a b c a b c d a b d a b d e)))

(print-dict  (mk-lz-dict '(a a b a b c a b c d a b d a b d e)))
(print-dict (mk-continuation-dict (mk-lz-dict '(a a b a b c a b c d a b d a b d e))))
(print-dict (mk-continuation-dict (mk-lz-dict '(a a b a b c a b c d a b d a b d e a c e b a c a b a b a b a b a b))))
(sort 
 (LZtextGenerate (mk-continuation-dict (mk-lz-dict '(a a b a b c a b c d a b d a b d e a c e b a c a b a b a b a b a b)))
             5 1000 T)
 #'(lambda (x y) (string< (symbol-name x)(symbol-name y))))

|#


;***********************************  modifs de benoit

(defun LZtextCrossGenerate (ldict MaxPast maxLength mostprobable taux-chgt pioche)
  ;taux-chgt va de 0 à 100: 0 pour rester dans le dico de départ, et 100 pour rester dans le dico d'arrivée
  ;50 correspond à un changement égal dans l'un ou l'autre
  ;si on rentre une liste ou un bpf, on fait un matching de la liste avec la taille demandée du texte (valable pour les variables
  ;maxpast et taux-chgt).
  (let* ((taille-contexte 5)
         (fonction-chgt (cond ((and (listp taux-chgt) (cdr taux-chgt)) 
                               (bpf-sample (make-instance 'bpf :y-points taux-chgt) 0 100 maxlength))
                              ((bpf-p taux-chgt) (bpf-sample taux-chgt (car (x-points taux-chgt)) (car (last (x-points taux-chgt)))
                                                             maxlength))
                              (t nil)))
         (fonction-past (cond ((and (listp MaxPast) (cdr MaxPast)) 
                               (bpf-sample (make-instance 'bpf :y-points MaxPast) 0 100 maxlength))
                              ((bpf-p MaxPast) (bpf-sample MaxPast (car (x-points MaxPast)) (car (last (x-points MaxPast)))
                                                           maxlength))
                              (t nil))) current-dict)
    (if pioche (setq pioche (mettre-parenthese (string->ascii pioche))))
    (setq taux-chgt (if fonction-chgt fonction-chgt
                        (if taux-chgt
                          (create-list maxlength (if (listp taux-chgt) (car taux-chgt) taux-chgt))
                          (create-list maxlength 0))))
    (setq MaxPast (if fonction-past fonction-past
                      (if MaxPast
                        (create-list maxlength (if (listp MaxPast) (car MaxPast) MaxPast))
                        (create-list maxlength 0))))
    (setq current-dict (if (< (random 100) (car taux-chgt)) 1 0))
    ;(print (list taux-chgt maxpast))
    ;current-dict = dictionnaire courant
    ;taille-contexte: taille minimale du contexte commun pour pouvoir passer d'un dico à l'autre
    ;fonction-chgt: valeur de taux-chgt selon chaque numéro d'élément généré
    (loop for count from 1 to MaxLength
          for result = (list (LZtextGenerate1 (nth current-dict ldict) T mostprobable pioche current-dict))
          then (cons (loop for prefix on (reverse (first-n result (nth (- count 1) MaxPast)))
                           for continuation1  = (if (car continuation1) continuation1
                                                    (list (LZtextGenerate1 (nth 0 ldict)
                                                                           prefix mostprobable pioche 0) (length prefix)))
                           for continuation2  = (if (car continuation2) continuation2
                                                    (list (LZtextGenerate1 (nth 1 ldict)
                                                                           prefix mostprobable pioche 1) (length prefix)))
                           while (or (null (car continuation1)) (null (car continuation2)))
                           finally (return
                           (progn 
                             ;(if (or (null (car continuation1)) (null (car continuation2)))
                             ;(print (list current-dict continuation1 continuation2)))
                             (cond ((and (null (car continuation1)) (null (car continuation2)))
                                    (LZtextGenerate1 (nth current-dict ldict) T mostprobable pioche current-dict))
                                   ((null (car continuation1))
                                    (progn ;(print (list current-dict 'nil "cont1"))
                                      (if (equal current-dict 0)
                                        (progn (setq current-dict 1)
                                               (car continuation2))
                                        ;(LZtextGenerate1 (nth current-dict ldict) T mostprobable)
                                        (car continuation2))))
                                   ((null (car continuation2))
                                    (progn ;(print (list current-dict 'nil "cont2"))
                                      (if (equal current-dict 1)
                                        (progn (setq current-dict 0)
                                               (car continuation1))
                                        ;(LZtextGenerate1 (nth current-dict ldict) T mostprobable)
                                        (car continuation1))))
                                   (t
                                    (progn
                                      ;(print (list continuation1 continuation2))
                                      ;probabilité pour un changement
                                      (setq current-dict
                                            (if (equal current-dict 0)
                                              (if (< (random 100) (min (* 2 (nth (- count 1) taux-chgt)) 100))
                                                ;chgt de 0 à 100 pour taux-chgt de 0 à 50, puis stable à 100
                                                (if (or (>= (cadr continuation2) taille-contexte)
                                                        (and (>= (cadr continuation2) (max 1 (- taille-contexte 1))) 
                                                             (equal (car (last prefix)) 32)))
                                                  1 0) 0)
                                              (if (< (random 100) (min (* 2 (- 100 (nth (- count 1) taux-chgt))) 100))
                                                (if (or (>= (cadr continuation1) taille-contexte)
                                                        (and (>= (cadr continuation1) (max (- taille-contexte 1) 1)) 
                                                             (equal (car (last prefix)) 32))) 0 1) 1)))
                                      (if (equal current-dict 0) (car continuation1) (car continuation2))))))))
                     result)
          finally (return (progn (setq *mot-en-cours* nil) (nreverse result))))))

(defun mettre-parenthese (list)
  ;change les chiffres 32 en parenthèses
  (let (resul)
    (mapcar #'(lambda(a) (if (equal a 32) (setq resul (append resul (list nil)))
                             (setq resul (append (butlast resul) (list (append (car (last resul)) (list a)))))))
            list)
    (remove 'nil resul)))

(defvar *mot-en-cours* nil)
;variable contenant le mot couramment priviliégié

(defmethod LZtextGenerate1 ((dict dictionary) (prefix t) (mostprobable symbol) &optional pioche current-dict)
  ;pioche contient les mots à privilégier
  (let* ((value (get-value dict prefix))
         (value2 (if (listp prefix) (get-value dict (last-n prefix 3)) value))
         (lettre (car *mot-en-cours*)) prob-modifiees resul)
    ;value contient la liste des lettres et leur probas
    ;lettre contient la lettre couramment privilégiée
    ;prob-modifiees contient la liste des probabilités modifiée
    ;(unless (or (null *mot-en-cours*) (equal current-dict 1) (null value2)) (print (list "iter" *mot-en-cours* value2 prefix)))
    (cond ((and value2 (equal current-dict 0) lettre (member lettre (car value2)))
           (progn (setq prob-modifiees (mapcar #'(lambda(a) (if (equal lettre a) 1. 0.)) (car value2)))
                  (setq resul (nth (rand-from-distribution prob-modifiees)
                                   (car value2)))
                  (if (equal resul lettre)
                    (setq *mot-en-cours* (cdr *mot-en-cours*))
                    (setq *mot-en-cours* nil))
                  ;(print (list *mot-en-cours* resul))
                  resul))
          ((and value2 (equal current-dict 0) (listp prefix) (null *mot-en-cours*) (member (car (last prefix)) (car (mat-trans pioche))))
           ;on cherche si un mot de la pioche pourrait être privilégié à partir de la dernière lettre du préfixe
           (progn (setq *mot-en-cours* (cdar (member (car (last prefix)) pioche :test #'(lambda(a b) (equal a (car b))))))
                  ;(print (list "init" *mot-en-cours* prefix))
                  (LZtextGenerate1 dict prefix mostprobable pioche current-dict)))
          (value
           (progn            
             (if (equal current-dict 0) (setq *mot-en-cours* nil))
             (nth (rand-from-distribution (if mostprobable (second value) (inverse-distribution (second value))))
                  (car value))))
          (t nil))))


(defun position-max (liste)
  ;renvoie en random une position maximale parmi les plusieurs possibles
  (nth-random (position-max1 liste 0 '(0) 0)))

(defun position-min (liste)
  ;renvoie en random une position minimale parmi les plusieurs possibles
  (nth-random (position-min1 liste 1 '(0) 0)))

(defun position-max1 (liste resul pos pos-courante)
  (cond ((null liste) pos)
        ((> (car liste) resul)
         (position-max1 (cdr liste) (car liste) (list pos-courante) (+ 1 pos-courante)))
        ((= (car liste) resul)
         (position-max1 (cdr liste) resul (cons pos-courante pos) (+ 1 pos-courante)))
        (t (position-max1 (cdr liste) resul pos (+ 1 pos-courante)))))


(defun position-min1 (liste resul pos pos-courante)
  (cond ((null liste) pos)
        ((< (car liste) resul)
         (position-min1 (cdr liste) (car liste) (list pos-courante) (+ 1 pos-courante)))
        ((= (car liste) resul)
         (position-min1 (cdr liste) resul (cons pos-courante pos) (+ 1 pos-courante)))
        (t (position-min1 (cdr liste) resul pos (+ 1 pos-courante)))))



#|
(defmethod get-connection-lines ((self boxframe) i)
  (let* ((ctrl (nth i (inputframes self)))
         (connection (connected? (object ctrl)))
         (boxsource (first connection))
         (possource (view-position (car (frames boxsource))))
         (sizesource (view-size (car (frames boxsource))))
         (x-self (x self))
         (y-self (y self))
         (in-xs (x ctrl))
         x1 y1 xi yi midx midy)
    (setq x1 (+ (point-h possource)
                (- (* (+ (second connection) 1) (round (point-h sizesource) (+ (numouts boxsource) 1))) 2)))
    (setq y1 (- (+ (point-v possource) (point-v sizesource)) 2)) 
    (setq xi (+ x-self in-xs 4))
    (setq yi  y-self)
    (setf midx (round (abs (- xi x1)) 2))
    (setf midy (round (abs (- yi y1)) 2))
    (list (make-point x1  y1)
            (make-point xi yi))))
|#