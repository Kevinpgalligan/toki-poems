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

(defun num-rhymes (word)
  (length (gethash word *rhyme-map*)))

(defun make-toki-chain (path)
  "Creates a Markov chain of toki pona words, based
on a text file."
  (let ((chain (create-chain path)))
    ;; Remove anything that's not a word in toki pona, or
    ;; a special state (start or end).
    (loop for word being the hash-keys of chain
	  do (flet ((keep-p (v)
		      (or (eq 'start v)
			  (eq 'end v)
			  (toki-word-p v))))
	       (if (keep-p word)
		   (setf (gethash word chain)
			 (remove-if-not (lambda (transition)
					  (keep-p (car transition)))
					(gethash word chain)))
		   (remhash word chain))))
    chain))

(defun generate-poem (chain raw-structure &key allow-repeats)
  "CHAIN is a Markov chain of toki pona words.
RAW-STRUCTURE is a string that describes the structure of the poem.
Example structure: 3A 5B 3A / 5C
That's 2 stanzas, the first has 3 lines and the second has 1.
The 1st and 3rd lines rhyme in the first stanza. The first line
has 3 syllables, the second line has 5 syllables, etc."
  (let* ((stanzas (parse-poem-structure raw-structure))
	 (label-counts (make-label-counts stanzas))
	 (n-stanzas (length stanzas))
	 (end-table (make-line-end-table)))
    (with-output-to-string (s)
      (loop for stanza in stanzas
	    for stanza-i = 1 then (1+ stanza-i)
	    do (let* ((lines (stanza-lines stanza))
		      (n-lines (length lines)))
		 (loop for line in lines
		       for line-i = 1 then (1+ line-i)
		       ;; Well ain't that a lot of arguments.
		       do (write-poem-line
			   s chain line end-table label-counts allow-repeats)
		       do (format s (if (= line-i n-lines) "." ",~%"))))
	    do (format s (if (= stanza-i n-stanzas) "" "~%~%"))))))

(defun make-label-counts (stanzas)
  (let ((counts (make-hash-table :test #'equalp)))
    (loop for stanza in stanzas
	  do (loop for line in (stanza-lines stanza)
		   do (let ((label (line-label line)))
			(setf (gethash label counts)
			      (1+ (or (gethash label counts) 0))))))
    counts))

(defun write-poem-line (stream chain line end-table label-counts allow-repeats)
  (let ((words (gen-words chain line end-table label-counts allow-repeats)))
    (when (not (line-end-table-contains-p end-table (line-label line)))
      (put-line-end-table end-table (line-label line) (car (last words))))
    (format stream "~{~a~^ ~}" words)))

(defun gen-words (chain line end-table label-counts allow-repeats)
  (let ((label (line-label line))
	(target-syllables (line-syllables line)))
    (labels ((rec (curr-word syllables)
		  (cond
		   ((> syllables target-syllables) nil)
		   ((= syllables target-syllables)
		    (values nil (suitable-end-p end-table label curr-word label-counts allow-repeats)))
		   (t
		    (loop for word in (weighted-shuffle
				       (get-transitions chain curr-word))
			  ;; Excludes the 'end state.
			  do (when (stringp word)
			       (multiple-value-bind (result success)
						    (rec word (+ syllables (count-syllables word)))
						    (when success
						      (return (values (cons word result) t))))))))))
	    (rec 'start 0))))

(defun weighted-shuffle (transitions)
  (let ((shuffled (copy-list transitions))
	(total-weight (loop for (state . count) in transitions
			    sum count)))
      (labels ((rec (remaining-weight list)
		 (when (not (null (cdr list)))
		   (let* ((i (random remaining-weight)))
		     (rotatef (car list)
			      (car (drop-until-cumulative-weight i list)))
		     ;; All this cdar-ing and rotatef-ing is kinda ugly.
		     (rec (- remaining-weight (cdar list))
			  (cdr list))))))
	(rec total-weight shuffled))
    (loop for (state . count) in shuffled
	  collect state)))

(defun drop-until-cumulative-weight (i list)
  (let ((cumulative 0))
    (loop do (incf cumulative (cdar list))
	  while (<= cumulative i)
	  do (setf list (cdr list)))
    list))

(defun make-line-end-table ()
  (make-hash-table :test 'equalp))

(defun line-end-table-contains-p (table label)
  (gethash label table))

(defun put-line-end-table (table label word)
  (setf (gethash label table) (cons word (gethash label table))))

(defun suitable-end-p (table label word label-counts repeats-allowed)
  (let ((other-ends (gethash label table)))
    (and (or (not other-ends)
	     (string= (rhyme-suffix (car other-ends)) (rhyme-suffix word)))
	 (or repeats-allowed
	     ;; If repeats aren't allowed, make sure that this word
	     ;; isn't a repeat of other words that have been used to
	     ;; end lines with this label.
	     ;; Also make sure that the word has sufficiently many rhymes
	     ;; to end all the lines with this label.
	     (and
	      (loop for end in other-ends
		    never (string= end word))
	      (>= (num-rhymes word) (gethash label label-counts)))))))

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
