;; TODO:
;; 0. [DONE] rhyme table.
;; 1. [DONE] download corpus from reddit.
;; 2. markov chain w/ only toki pona words.
;; 3. generate poems.
;;    -> DFS through the markov chain, probabilistic shuffle.
;;    -> stop when syllables correct & it rhymes.

(ql:quickload 'cl-ppcre)

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

(defun rhyme-suffix (word)
  (if (< (length word) 3)
      word
      (cl-ppcre:scan-to-strings "([aeiou]+[^aeiou]+[aeiou]+$)|([aeiou]+[^aeiou]+$)" word)))

(defparameter *rhyme-map* (make-hash-table :test 'equalp))

(loop for word in *toki-words*
      for i = 0 then (1+ i)
      do (loop for cmp-word in (nthcdr (1+ i) *toki-words*)
	       do (when (rhymes-p word cmp-word)
		    (setf (gethash word *rhyme-map*)
			  (cons cmp-word (gethash word *rhyme-map*)))
		    (setf (gethash cmp-word *rhyme-map*)
			  (cons word (gethash cmp-word *rhyme-map*))))))

(defun rhymes-p (w1 w2)
  (string= (rhyme-suffix w1) (rhyme-suffix w2)))

(defun skip-whitespace (s i)
  (loop while (and (< i (length s))
		   (is-whitespace-p (char s i)))
	do (incf i))
  i)

(defun is-whitespace-p (c)
  (member c (list #\Space #\Backspace #\Tab #\Linefeed
		  #\Page #\Return #\Rubout #\Newline)))

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
