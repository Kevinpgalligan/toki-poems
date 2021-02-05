(defparameter +end-states+ (list "?" "!" "." 'end))
(defparameter +punctuation-chars+
  (list #\! #\. #\" #\( #\)
        #\, #\; #\/ #\? #\:))
(defparameter +whitespace-chars+
  (list #\Space #\Backspace #\Tab
        #\Linefeed #\Page #\Return
        #\Rubout #\Newline))

(defparameter +chars-without-whitespace-before+
  (list #\! #\. #\) #\? #\, #\; #\:))

(defun create-chain (path)
  "Creates a text-generating Markov chain based on the text file at PATH."
  (let ((chain (new-chain)))
    (with-open-file (stream path)
      (loop for line = (read-line stream nil)
            while line
            do (let ((tokens (tokenise line)))
                 (when tokens
                   (loop for (a b) on tokens
                         while b ; avoids last item being paired with nil
                         do (add-transition chain a b))
                   (add-transition chain 'start (first tokens))
                   (add-transition chain (first (last tokens)) 'end)))))
    chain))

(defun tokenise (s)
  (defun stripped-tokenise (s)
    ;; This will automatically handle the empty
    ;; string case.
    (let ((ptr 0))
      (loop while (< ptr (length s))
            collect (multiple-value-bind (token new-ptr)
                        (next-token s ptr)
                      (setq ptr new-ptr)
                      token))))
  (stripped-tokenise (string-trim +whitespace-chars+ s)))

(defun next-token (s ptr)
  (let ((new-ptr
          (cond
            ;; Special case for tags at the start of a
            ;; tweet, like ".@NameHere".
            ((and (char= (char s ptr) #\.)
                  (< (1+ ptr) (length s))
                  (char= (char s (1+ ptr)) #\@))
             (skip-word-chars s (1+ ptr)))
            ((punctuation-char-p s ptr)
             ;; Clump punctuation chars together.
             ;; So "..." will be a single token.
             (skip-while-equal s ptr (char s ptr)))
            (t
             (skip-word-chars s ptr)))))
    (values (subseq s ptr new-ptr)
            ;; Gotta skip whitespace *now* so that we don't
            ;; try to read anything more from the string
            ;; when it contains only whitespace.
            (skip-whitespace s new-ptr))))

(defun punctuation-char-p (s ptr)
  (member (char s ptr) +punctuation-chars+))

(defun skip-while-equal (s ptr c)
  (skip-while s ptr (lambda (s ptr)
                      (char= c (char s ptr)))))

(defun word-char-p (s ptr)
  (not
   (or (whitespace-p s ptr)
       (punctuation-char-p s ptr))))

(defun skip-word-chars (s ptr)
  (skip-while s ptr (lambda (s ptr)
                      (or
                       (word-char-p s ptr)
                       ;; This exception allows us to consider
                       ;; abbreviations, such as U.S.A., as a
                       ;; single token. Also, money amounts, like
                       ;; $5.9M.
                       (and (char= (char s ptr) #\.)
                            (or
                             (and (< (1+ ptr) (length s))
                                  (word-char-p s (1+ ptr)))
                             (and (<= 0 (- ptr 2))
                                  (char= (char s (- ptr 2)) #\.))))))))

(defun skip-whitespace (s ptr)
  (skip-while s ptr (lambda (s ptr)
                      (whitespace-p s ptr))))

(defun skip-while (s ptr condition-fn)
  (if (and (< ptr (length s))
           (funcall condition-fn s ptr))
      (skip-while s (1+ ptr) condition-fn)
      ptr))

(defun whitespace-p (s ptr)
  (member (char s ptr) +whitespace-chars+))

(defun new-chain ()
  (let ((transitions (make-hash-table :test 'equalp)))
    ;; This is the start state, which currently doesn't
    ;; have any transitions.
    (setf (gethash 'start transitions) (list))
    transitions))

(defun add-transition (chain a b)
  ;; Even if it doesn't exist yet, we can use the
  ;; nil that is returned as an empty alist.
  (let* ((alist (gethash a chain))
         (entry (assoc b alist :test 'equalp)))
    (if entry
        (let ((old-count (cdr entry)))
          (rplacd (assoc b alist :test 'equalp) (1+ old-count)))
        (setf (gethash a chain) (acons b 1 alist)))))

(defun get-transitions (chain a)
  (gethash a chain))

(defun generate (chain)
  (format-tokens (generate-tokens chain)))

(defun generate-tokens (chain)
  ;; This list will always start with 'start.
  (loop for state = 'start then (next-state chain state)
        while (not (equalp state 'end))
        collect state))

(defun next-state (chain state)
  (let* ((transitions (get-transitions chain state))
         (total (apply #'+ (mapcar #'cdr transitions)))
         (selection (random total)))
    (loop for (state . count) in transitions
          summing count into running-count
          if (< selection running-count) do (return state))))

(defun format-tokens (tokens)
  (apply #'concatenate
         (cons 'string
               (loop for (a b) on tokens
                     while b
                     collect (format-token b a)))))

(defun format-token (token prev)
  (if (or (eq 'start prev)
          (never-preceded-by-space-p token))
      token
      (concatenate 'string " " token)))

(defun never-preceded-by-space-p (token)
  (member (char token 0)
          +chars-without-whitespace-before+))
