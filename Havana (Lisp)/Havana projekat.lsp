;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIJALIZACIJA POCETNOG STANJA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;inicijalizuje prom3 na n-1
(defun promeni3 (n)
              (if (equal (setq prom3 (- n 1)) (- n 1)) t))

;;inicijalizuje prom4 na 2n-1
(defun promeni4 (n)
              (if (equal (setq prom4 (- (* 2 n) 1)) (- (* 2 n) 1)) t))

;;racuna koliko elemenata ima svaka vrsta u igrici
(defun racunaj2 (br)
               (if (and (< prom3 (- (* 2 br) 1)) (equalp prom4 (- (* 2 br) 1))) (setq prom3 (+ prom3 1)) (setq prom4 (- prom4 1))))

;;crta proizvoljni broj crtica
(defun crtaj2 (br)
               (cond ((equalp br 0) lista)
              ((listp (setq lista (cons '- lista))) (crtaj2 (- br 1)))))

;;poziva crtaj2() i nadovezuje listu crtica kao poseban element na listu2
(defun crtaj4 (br)
                (setq lista2 (list lista2 (crtaj2 br))))

;;poziva crtaj4() i ispisuje proizvoljan broj vrsta sa onoliko crtica koliko izracuna funkcija racunaj2
(defun crtaj3 (br1 br2)
               (cond ((equalp br1 0) lista1)
                     ((and (listp (setq lista1 (append lista1 (cdr (crtaj4 (racunaj2 br2)))))) (null (setq lista '()))) (crtaj3 (- br1 1) br2))))

;;poziva crtaj3() i racuna koliko vrsta treba da ima na osnovu ivice
(defun crtaj5 (br)
               (if (and (equal (setq n br) br) (null (setq lista1 '())) (null (setq lista2 '())) (null (setq lista '())) (promeni3 br) (promeni4 br)    )  (crtaj3 (- (* br 2) 1) br) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ISPIS TRENUTNOG STANJA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;inicijalizuje prom1 na n
(defun promeni1 (n)
              (if (equal (setq prom1 n) n) t))

;;inicijalizuje prom2 na 0
(defun promeni2 ()
              (if (equal (setq prom2 0) 0) t))

;;racuna koliko razmaka treba ispisati ispred svakog reda
(defun racunaj ()
               (if (and (> prom1 0) (equalp prom2 0)) (setq prom1 (- prom1 1)) (setq prom2 (+ prom2 1))))

;;crta onoliko razmaka koliko vrati funkcija racunaj(), ispred svake vrste
(defun crtaj (br)
               (cond ((equalp br 0) t)
              ((null (format t" ")) (crtaj (- br 1)))))

;;ispisuje trenutno stanje igre
(defun finish ()
               (if (and (promeni1 n) (promeni2)) (loop for cons on lista1
                  do (if (crtaj (racunaj)) (format t "~a" (car cons)))
                  when (cdr cons) do (format t "~%"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POTEZ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;prevodjenje slova u broj (a-0, b-1, c-2....)
(defun slovoubroj (slovo)
               (if t (- (char-code (char (format nil "~a" slovo) 0)) 65)))

;;prevodi prosledjene vrednosti za drugi indeks u realno mesto u nizu
(defun nadjiindex (ind1 ind2)
               ( if (<= (slovoubroj ind1) (/ (- (length lista1) 1) 2)) ind2 (- ind2 (- (slovoubroj ind1) (/ (- (length lista1) 1) 2)))))

;;proverava validnost poteza
(defun validacija (ind1 ind2)
               (if (and (<= (slovoubroj ind1) (- (length lista1) 1)) (>= (nadjiindex ind1 ind2) 0) (< (nadjiindex ind1 ind2) (length (nth (slovoubroj ind1) lista1)) ) (equalp (nth (nadjiindex ind1 ind2) (nth (slovoubroj ind1) lista1)) '-)) t '()))

;;odigravanje poteza
(defun potez (ind1 ind2 igrac)
               (if (validacija ind1 ind2) (and (setq prethodnoStanje lista1) (if (equalp (setf (nth (nadjiindex ind1 ind2) (nth (slovoubroj ind1) lista1)) igrac) igrac) t)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POCETAK IGRE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;pomocna funkcija koja vraca suprotnu oznaku od one koja treba da se odigra u narednom prolazu
(defun suprotnaOznaka ()
				(if (equal oznaka 'X) 'O 'X))

;;prevodjenje broja u slovo(0-a, 1-b...)
(defun brojuslovo (broj)
                (cond ((null broj) '())
                (t (code-char (+ broj 65)))))
				
;;prevodi realno mesto u nizu u prosledjene vrednosti za drugi indeks
(defun nadjiindexkontra (ind1 ind2)
               ( if (<= (slovoubroj ind1) (/ (- (length lista1) 1) 2)) ind2 (+ ind2 (- (slovoubroj ind1) (/ (- (length lista1) 1) 2)))))
			   
;;covekov potez
(defun covek()
               (progn 
                 (format t "~%Unesite red zeljenog poteza(A-Z): ")
                 (setq red (read)) 
                 (format t "~%Unesite kolonu zeljenog poteza: ")
                 (setq kol (read)) 
                 (if (potez red kol oznaka) (list red kol) (covek)) 
                ))

;;kompjuterski potez
(defun cpu ()
				(progn
				 (setq potezCpu (minimax (list (slovoubroj (car indeksi)) (nadjiindex (car indeksi) (cadr indeksi))) 3 't))
                 (if (potez (brojuslovo (caar potezCpu)) (nadjiindexkontra (brojuslovo (caar potezCpu)) (cadar potezCpu)) oznaka) (if (null (format t "~%Kompjuter odigrao potez~%")) (list (brojuslovo (caar potezCpu)) (nadjiindexkontra (brojuslovo (caar potezCpu)) (cadar potezCpu)))) (cpu))
		))

;;odigravanje poteza
(defun igraj () 
               (progn
                 (if auto (setq indeksi (cpu)) (setq indeksi (covek)))
                 (if (equal oznaka 'X) (setq oznaka 'O) (setq oznaka 'X))
                 (if auto (setq auto '()) (setq auto t))
                 (finish)
                 (if (or (most (slovoubroj (car indeksi)) (nadjiindex (car indeksi) (cadr indeksi)) (suprotnaOznaka)) (vila (slovoubroj (car indeksi)) (nadjiindex (car indeksi) (cadr indeksi)) (suprotnaOznaka))) 'KRAJ (igraj))
                ))

;;startovanje igre
(defun start (n)               
                (progn 
                  (setq oznaka 'X)
                  (format t "Unesite r ako racunar igra prvi(r ili c): ")
                  (setq racunar (read))
                  (if (equal racunar 'r) (setq auto t) (setq auto '()))
                  (crtaj5 n)
                  (igraj)
                 ))
				 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRAVLJENJE GRAFA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;pravi i vraca cvor od prosledjenih indeksa
(defun vratiCvor(i j)
	(if (and (> i -1) (> j -1)) (if t (list i j (nth j (nth i lista1)) ))))

;;vraca sve susedne cvorove od cvora sa prosledjenim indeksima
;;tri slucaja posoje zbog cuvanja cvorova u grafu sa realnim indeksima
(defun vratiPotege(i j)
   (if (null (setq listaPotega '()))
	(cond
		((< i (- n 1))
				(if (or (= i 0) (= j 0)) '() (if (nth (- j 1) (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) (- j 1)))))))
			
				(if (= i 0) '() (if (nth j (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) j))))))
		
				(if (= j 0) '() (if (nth (- j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (- j 1)))))))
		
				(if (nth (+ j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (+ j 1))))))
		
				(if (nth j (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) j)))))
	
				(if (nth (+ j 1) (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) (+ j 1))))))
		)
		((= i (- n 1))
				(if (or (= i 0) (= j 0)) '() (if (nth (- j 1) (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) (- j 1)))))))
			
				(if (= i 0) '() (if (nth j (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) j))))))
		
				(if (= j 0) '() (if (nth (- j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (- j 1)))))))
		
				(if (nth (+ j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (+ j 1))))))
		
				(if (= j 0) '() (if (nth (- j 1) (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) (- j 1)))))))
	
				(if (nth j (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) j)))))
		)
		((> i (- n 1))
				(if (= i 0) '() (if (nth j (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) j))))))
			
				(if (= i 0) '() (if (nth (+ j 1) (nth (- i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (- i 1) (+ j 1)))))))
		
				(if (= j 0) '() (if (nth (- j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (- j 1)))))))
		
				(if (nth (+ j 1) (nth i lista1)) (setq listaPotega (append listaPotega (list (vratiCvor i (+ j 1))))))

				(if (= j 0) '() (if (nth (- j 1) (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) (- j 1)))))))
	
				(if (nth j (nth (+ i 1) lista1)) (setq listaPotega (append listaPotega (list (vratiCvor (+ i 1) j)))))))))

;;funkcija koja pravi graf
(defun praviGraf2 (i j listaParam)
  (cond ((and (> i  (- (* 2 n) 2)) (null (nth j (nth i lista1)))) listaParam)
        ((null (nth j (nth i lista1))) (praviGraf2 (+ i 1) 0 listaParam))
        (t 
         (let ((listaVrsta (append listaParam (list (list  (list i j (nth j (nth i lista1))) (vratiPotege i j))))))
             (praviGraf2 i (+ j 1) listaVrsta)))))

;;funkcija koja olaksava poziv funkcije praviGraf2
(defun praviGraf ()
                (praviGraf2 '0 '0 '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OBILAZAK PO SIRINI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noviCvorovi (potomci cvorovi igrac)
  (cond ((null potomci) '()) 
        ((and (not (member (car potomci) cvorovi)) (equal (caddr (car potomci)) igrac))
         (cons (car potomci) (noviCvorovi (cdr potomci) cvorovi igrac)))
        (t (noviCvorovi (cdr potomci) cvorovi igrac))))

(defun dodajPotomke (graf cvor cvorovi igrac) 
  (cond ((null graf) '()) 
        ((equal (caar graf) cvor)
         (noviCvorovi (cadar graf) cvorovi igrac)) 
        (t (dodajPotomke (cdr graf) cvor cvorovi igrac))))

(defun nadjiPut (graf l cilj cvorovi igrac) 
  (cond  ((null l)  '()) 
        ((equal (car l) cilj)  (list cilj))
        (t  (let* ((cvorovi1 (append cvorovi (list (car l))))
                   (potomci1 (dodajPotomke graf (car l) (append (cdr l) cvorovi1) igrac))
                   (l1 (append (cdr l) potomci1)) 
                   (nadjeni-put (nadjiPut graf l1 cilj cvorovi1 igrac)))
              (cond ((null nadjeni-put)  '()) 
                    ((member (car nadjeni-put) potomci1)  (cons (car l) nadjeni-put)) 
                    (t  nadjeni-put))))))
					
;;(nadjiPut (praviGraf) '((0 0 X)) '(3 4 X) '() 'x)
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MOST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;provera sa koliko je odgovarajucih temena povezan cvor sa zadatim indeksima	
(defun mostProvera (i j igrac)
                (let* ((brTemena 0))               
                  (+ (if (nadjiPut (praviGraf) (list (list i j igrac)) (list 0 0 igrac) '() igrac) (+ brTemena 1) 0)
                        (if (nadjiPut (praviGraf) (list (list i j igrac)) (list 0 (- n 1) igrac) '() igrac) (+ brTemena 1) 0)
                        (if (nadjiPut (praviGraf) (list (list i j igrac)) (list (- n 1) 0 igrac) '() igrac) (+ brTemena 1) 0)
                        (if (nadjiPut (praviGraf) (list (list i j igrac)) (list (- n 1) (- (* n 2) 2) igrac) '() igrac) (+ brTemena 1) 0)
                        (if (nadjiPut (praviGraf) (list (list i j igrac)) (list (- (* n 2) 2) 0 igrac) '() igrac) (+ brTemena 1) 0)
                        (if (nadjiPut (praviGraf) (list (list i j igrac)) (list (- (* n 2) 2) (- n 1) igrac) '() igrac) (+ brTemena 1) 0))))

;;funkcija koja vraca t ako je pronadjen most
(defun most(i j igrac)
                (if (>= (mostProvera i j igrac) 2) t '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VILA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;vraca cvorove iz gornje ivice
(defun listaVilaTop (i)
                (if (< i (- n 1)) 
                    (append (append listaVila (list (list 0 i))) (listaVilaTop (+ i 1)))))

;;vraca cvorove iz gornjeg leve ivice
(defun listaVilaLeftTop (i)
                (if (< i (- n 1)) 
                    (append (append listaVila (list (list i 0))) (listaVilaLeftTop (+ i 1)))))

;;vraca cvorove iz donje leve ivice
(defun listaVilaLeftBottom (i)
                (if (< i (- (* n 2) 2)) 
                    (append (append listaVila (list (list i 0))) (listaVilaLeftBottom (+ i 1)))))

;;vraca cvorove iz donje ivice
(defun listaVilaBottom (i)
                (if (< i (- n 1)) 
                    (append (append listaVila (list (list (- (* n 2) 2) i))) (listaVilaBottom (+ i 1)))))

;;vraca cvorove iz donje desne ivice
(defun listaVilaRightBottom (i)
                (if (< i (- (* n 2) 2)) 
                    (append (append listaVila (list (list i (- (length (nth i lista1)) 1)))) (listaVilaRightBottom (+ i 1)))))

;;vraca cvorove iz gornje desne ivice
(defun listaVilaRightTop (i)
                (if (< i (- n 1)) 
                    (append (append listaVila (list (list i (- (length (nth i lista1)) 1)))) (listaVilaRightTop (+ i 1)))))

;;poziva 6 funkcija za 6 ivica i appenduje ih, cvorovi su oblika (i j), vrednost se ne pamti jer je nepotrebna ovde
(defun listaVilaAppend ()
                (if (null (setq listaVila '()))
                (append (listaVilaTop 1) (listaVilaLeftTop 1) (listaVilaLeftBottom n) (listaVilaBottom 1) (listaVilaRightBottom n) (listaVilaRightTop 1))))

;;proverava do koliko se ivica moze stici od cvora sa datim indeksima
(defun vila2 ( i j igrac listaVila)
                (cond ((null listaVila) '0)
                      ((not ( null (nadjiPut (praviGraf) (list (list i j igrac)) (list (caar listaVila) (cadar listaVila) igrac) '() igrac))) (+ 1 (vila2 i j igrac (cdr listaVila))))
                      (( null (nadjiPut (praviGraf) (list (list i j igrac)) (list (caar listaVila) (cadar listaVila) igrac) '() igrac)) (+ 0 (vila2 i j igrac (cdr listaVila))))
                      (t (vila2 i j igrac (cdr listaVila)))))

;;inicijalizuje listuVila na vrednost koju vraca listaVilaAppend i proverava da li je ispunjen uslov za vilu
(defun vila (i j igrac)
                (if (not (null (setq listaVila (listaVilaAppend)))) (if (>= (vila2 i j igrac listaVila) 3) t '())))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MINIMAX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funkcija koja pravi fiksnu heuristiku za n=3
(defun proceniStanje (indeksi)
               (case (car indeksi)
			     ((0) (case (cadr indeksi)
                        ((0) 7)
                        ((1) -3)
                        ((2) 8)
                        ((3) -5)
						((4) 1)
						((5) -4)))
                 ((1) (case (cadr indeksi)
                        ((0) -5)
                        ((1) 4)
						((2) -4)
						((3) 3)
						((4) -1)
						((5) 4)
                        ((6) -2)))
                 ((2) (case (cadr indeksi)
                        ((0) -6)
                        ((1) 3)
                        ((2) -8)
                        ((3) 5)
						((4) -1)
						((5) 4)
                        ((6) -2)
						((7) -8)))
                 ((3) (case (cadr indeksi)
                        ((0) 6)
                        ((1) -3)
                        ((2) 8)
                        ((3) 5)
                        ((4) -1)
						((5) 1)
						((6) 4)
                        ((7) -2)
						((8) 8)))
                 ((4) (case (cadr indeksi)
                        ((0) 7)
                        ((1) -3)
                        ((2) 8)
                        ((3) -5)
						((4) 5)
                        ((5) -1)
						((6) -7)
						((7) 4)
                        ((8) 2)
						((9) -1)))
				 ((5) (case (cadr indeksi)
                        ((0) 7)
                        ((1) -3)
                        ((2) 8)
                        ((3) -5)
						((4) 5)
                        ((5) -1)
						((6) 7)
						((7) 4)
                        ((8) -2)
						((9) 1)
						((10) -5)))
				 ((6) (case (cadr indeksi)
                        ((0) 6)
                        ((1) 1)
                        ((2) -5)
						((3) 3)
						((4) -1)
						((5) 4)
                        ((6) -2)
						((7) 4)
                        ((8) 2)
						((9) -1)))
				 ((7) (case (cadr indeksi)
                        ((0) 6)
                        ((1) -1)
                        ((2) 5)
						((3) 3)
						((4) -1)
						((5) 1)
						((6) 4)
                        ((7) -2)
						((8) 8)))
				 ((8) (case (cadr indeksi)
                        ((0) -6)
                        ((1) 1)
                        ((2) -5)
						((3) 3)
						((4) -1)
						((5) 4)
                        ((6) -2)
						((7) 2)))
				 ((9) (case (cadr indeksi)
                        ((0) -6)
                        ((1) 1)
                        ((2) -5)
						((3) 3)
						((4) -1)
						((5) 4)
                        ((6) -2)))
                 ((10) (case (cadr indeksi)
                        ((0) 6)
                        ((1) -1)
                        ((2) 5)
						((3) 3)
						((4) -1)
						((5) 4)))))

(defun maxStanjeI (lsv stanjeVrednost)
                (cond ((null lsv) stanjeVrednost)
                      ((> (cadar lsv) (cadr stanjeVrednost))
                       (maxStanjeI (cdr lsv) (car lsv)))
                      (t (maxStanjeI (cdr lsv) stanjeVrednost))))

(defun maxStanje (lsv)
                (maxStanjeI (cdr lsv) (car lsv)))

(defun minStanjeI (lsv stanjeVrednost)
                (cond ((null lsv) stanjeVrednost)
                      ((< (cadar lsv) (cadr stanjeVrednost))
                       (minStanjeI (cdr lsv) (car lsv)))
                      (t (minStanjeI (cdr lsv) stanjeVrednost))))

(defun minStanje (lsv)
                (minStanjeI (cdr lsv) (car lsv)))
					  
(defun minimax (stanje dubina mojPotez)
                (let ((lp (vratiPotege (car stanje) (cadr stanje)))
                      (f (if mojPotez 'maxStanje 'minStanje)))
                  (cond ((or (zerop dubina) (null lp))
                         (list stanje (proceniStanje stanje)))
                        (t (apply f (list (mapcar (lambda (x)
                                                    (minimax x (1- dubina)
                                                             (not mojPotez))) lp)))))))
															 
;;(minimax (list 0 0) 4 't)