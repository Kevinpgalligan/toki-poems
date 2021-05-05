(defparameter dependencies
  (list 'chirp
	'toki
	'local-time))

(loop for dep in dependencies
      do (ql:quickload dep))

;;;; Make sure credentials are there and set 'em.
(setf *default-pathname-defaults* (truename (sb-posix:getcwd)))
(with-open-file (file "creds.txt" :direction :input)
  (let ((creds (read file)))
    (setf chirp:*oauth-api-key* (getf creds :api-key)
	  chirp:*oauth-api-secret* (getf creds :api-secret)
	  chirp:*oauth-access-token* (getf creds :access-token)
	  chirp:*oauth-access-secret* (getf creds :access-secret))))
(chirp:account/verify-credentials)


(in-package toki)

;;;; Utilities and constants around tweeting.
(defparameter *tweet-limit* 280)
(defparameter *user-id* "PonaBot")
(defparameter *tweet-period-seconds* (* 60 60 24))
(defparameter *cooloff-time* (* 60 20))
(defparameter *last-tweet-timestamp* nil)

(defun send-tweet (text)
  (if (> (chirp:compute-status-length text) *tweet-limit*)
      (error (format nil "Tweet ~a too long" text))
      (chirp:statuses/update text)))

(defun get-last-tweet-timestamp ()
  "Returns timestamp of the bot's last tweet, in seconds."
  (when (not *last-tweet-timestamp*)
    (setf *last-tweet-timestamp*
	  (apply #'max
		 (mapcar (lambda (status)
			   ;; Indirect dependency on local-time library, woops.
			   (local-time:timestamp-to-universal (chirp:created-at status)))
			 (chirp:statuses/user-timeline :user-id *user-id*)))))
  *last-tweet-timestamp*)

(defun get-next-tweet-timestamp ()
  (+ *tweet-period-seconds* (get-last-tweet-timestamp)))

(defun log-info (message)
  (write-log "INFO" message))

(defun log-error (message)
  (write-log "ERROR" message))

(defun write-log (type-prefix message)
  (format t "~a [~a] ~a" (local-time:format-timestring nil (local-time:now)) type-prefix message))


;;;; Set up poem generation stuff. Requires a corpus.
(defparameter *poem-structures*
  (list "5A 7B 5C" ; haiku
	"11A 11A / 6B 6B 11A" ; limerick
	"10A 10B 10A 10B / 10G 10G" ; shortened sonnet (fits within tweet)
	"5A 7B 5A / 5B" ; something I made up
	))
(defparameter *chain* (make-toki-chain "corpus.txt"))

(defun random-poem-structure ()
  (setf *random-state* (make-random-state t))
  (nth (random (length *poem-structures*)) *poem-structures*))


;;;; Finally, start the main loop.
(loop repeat
      (handler-case
	  (let ((next-tweet-timestamp (get-next-tweet-timestamp))
		(current-time (get-universal-time)))
	    (if (> current-time next-tweet-timestamp)
		(let ((text (generate-poem *chain* (random-poem-structure))))
		  (send-tweet text)
		  (setf *last-tweet-timestamp* current-time)
		  (log-info (format nil "Sent tweet: '~a'" text)))
		(progn (log-info "Not due to tweet yet, sleeping.")
		       (sleep (- next-tweet-timestamp current-time)))))
	(error (condition)
	  (log-error (format nil "Encountered an error! ~S" condition))
	  (log-info "Cooling off.")
	  (sleep *cooloff-time*))))
