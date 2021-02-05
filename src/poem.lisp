(in-package toki)

(defparameter *toki-words*
  (list "a" "akesi" "ala" "alasa" "ale" "ali" "anpa" "ante" "anu"
	"awen" "e" "en" "esun" "ijo" "ike" "ilo" "insa" "jaki"
	"jan" "jelo" "jo" "kala" "kalama" "kama" "kasi" "ken"
	"kepeken" "kili" "kin" "kipisi" "kiwen" "ko" "kon" "kule"
	"kulupu" "kute" "la" "lape" "laso" "lawa" "len" "lete" "li"
	"lili" "linja" "lipu" "loje" "lon" "luka" "lukin" "lupa" "ma"
	"mama" "mani" "meli" "mi" "mije" "moku" "moli" "monsi" "mu" "mun"
	"musi" "mute" "namako" "nanpa" "nasa" "nasin" "nena" "ni" "nimi"
	"noka" "o" "oko" "olin" "ona" "open" "pakala" "pali" "palisa"
	"pan" "pana" "pi" "pilin" "pimeja" "pini" "pipi" "poka" "poki"
	"pona" "pu" "sama" "seli" "selo" "seme" "sewi" "sijelo" "sike"
	"sin" "sina" "sinpin" "sitelen" "sona" "soweli" "suli" "suno"
	"supa" "suwi" "tan" "taso" "tawa" "telo" "tenpo" "toki" "tomo"
	"tu" "unpa" "uta" "utala" "walo" "wan" "waso" "wawa" "weka" "wile"))

(defun toki-word-p (word)
  (member word *toki-words* :test 'equalp))

(defun count-syllables (word)
  ;; Due to the way toki pona was designed, the number
  ;; of syllables is the number of groups of vowels.
  (length
   (cl-ppcre:all-matches-as-strings "[aeiou]+" word)))

(defun rhyme-suffix (word)
  (if (< (length word) 3)
      word
      (cl-ppcre:scan-to-strings "([aeiou]+[^aeiou]+[aeiou]+$)|([aeiou]+[^aeiou]+$)" word)))

(defun rhymes-p (w1 w2)
  (string= (rhyme-suffix w1) (rhyme-suffix w2)))

(defparameter *rhyme-map* (make-hash-table :test 'equalp))

(loop for word in *toki-words*
      for i = 0 then (1+ i)
      do (loop for cmp-word in (nthcdr (1+ i) *toki-words*)
	       do (when (rhymes-p word cmp-word)
		    (setf (gethash word *rhyme-map*)
			  (cons cmp-word (gethash word *rhyme-map*)))
		    (setf (gethash cmp-word *rhyme-map*)
			  (cons word (gethash cmp-word *rhyme-map*))))))


(defparameter *toki-chain*
  (create-chain "/home/kg/proyectos/toki-poems/corpus.txt"))

;; Remove anything that's not a word in toki pona, or a
;; special state (start & end).
(loop for word being the hash-keys of *toki-chain*
      do (flet ((keep-p (v)
		  (or (eq 'start v)
		      (eq 'end v)
		      (toki-word-p v))))
	   (if (keep-p word)
	       (setf (gethash word *toki-chain*)
		     (remove-if-not (lambda (transition)
				      (keep-p (car transition)))
				    (gethash word *toki-chain*)))
	       (remhash word *toki-chain*))))

(defun generate-toki-pona ()
  (generate *toki-chain*))

(defun generate-poem (raw-structure)
  (let* ((stanzas (parse-poem-structure raw-structure))
	 (n-stanzas (length stanzas))
	 (end-table (make-line-end-table)))
    (with-output-to-string (s)
      (loop for stanza in stanzas
	    for stanza-i = 1 then (1+ stanza-i)
	    do (let* ((lines (stanza-lines stanza))
		      (n-lines (length lines)))
		 (loop for line in lines
		       for line-i = 1 then (1+ line-i)
		       do (write-line s line end-table)
		       do (format s (if (= line-i n-lines) "." ",~%"))))
	    do (format s (if (= stanza-i n-stanzas) "" "~%~%"))))))

(defun write-line (stream line end-table)
  (let ((words (gen-words line end-table)))
    (when (not (line-end-table-contains-p end-table))
      (put-line-end-table table (line-label line) (last words)))
    (write-string (format nil "~{~a~^ ~}" words) stream)))

;; 1. recursively generate each line.
;; 2. keep adding new words until syllable
;;    count is correct for line.
;; 3. if there's already a word for this
;;    line's label, make sure we match it.
;; 4. if not, then accept the word we just
;;    landed on, as long as it has at least
;;    one match. Oh, and I guess it shouldn't
;;    match any other label.
(defun gen-words (line end-table)
  (labels ((rec (syllables label target-syllables)
	     (cond
	       ((> syllables target-syllables) nil)
	       ((= syllables target-syllables)
		;; TODO
		))))
    (rec 0 (line-label line) (line-syllables line))))

(defun make-line-end-table ()
  (make-hash-table :test 'equalp))

(defun line-end-table-contains-p (table label)
  (gethash label table))

(defun put-line-end-table (table label word)
  (setf (gethash label table) (rhyme-suffix word)))

(defun matches-other-lines (table label word)
  (let ((other-end (gethash label table)))
    (or (not other-end)
	(string= other-end (rhyme-suffix word)))))

(defun copy-list (list)
  (loop for x in list collect x))

(defun parse-poem-structure (s)
  ;; Returns a list of stanzas.
  (let ((i (skip-whitespace s 0)))
    (loop while (< i (length s))
	  collect (multiple-value-bind (stanza new-i)
		      (parse-stanza s i)
		    (setf i (skip-whitespace s new-i))
		    stanza))))

(defun parse-stanza (s i)
  (let ((lines
	  (loop while (and (< i (length s))
			   (not (char= (char s i) #\/)))
		collect (multiple-value-bind (line line-end)
			    (parse-poem-line s i)
			  (setf i (skip-whitespace s line-end))
			  line))))
    (when (and (< i (length s))
	       (char= (char s i) #\/))
      (incf i))
    (values (make-stanza lines) i)))

(defun skip-whitespace (s i)
  (loop while (and (< i (length s))
		   (is-whitespace-p (char s i)))
	do (incf i))
  i)

(defun is-whitespace-p (c)
  (member c (list #\Space #\Backspace #\Tab #\Linefeed
		  #\Page #\Return #\Rubout #\Newline)))

(defun make-stanza (lines) lines)
(defun stanza-lines (stanza) stanza)

(defun parse-poem-line (s i)
  (let* ((count-end
	   (or (position-if (lambda (c) (not (digit-char-p c)))
			    s
			    :start i)
	       (length s)))
	 (end (1+ count-end)))
    (values (make-poem-line (subseq s count-end end)
			    (parse-integer (subseq s i count-end)))
	    end)))

(defun make-poem-line (label syllables)
  (list :label label :syllables syllables))

(defun line-label (line)
  (getf line :label))

(defun line-syllables (line)
  (getf line :syllables))
