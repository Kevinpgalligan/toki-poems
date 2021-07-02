#!/usr/bin/sbcl --script

;;;; Tweets a randomly-generated toki pona poem based on
;;;; Twitter credentials in a file called 'creds.txt' (which
;;;; is just a serialised association list) and based on a
;;;; corpus of toki pona text in a file called 'corpus.txt'.
;;;; Expects that SBCL and quicklisp have been set up.

;; This ensures that quicklisp is available.
(load "~/.sbclrc")

(defparameter dependencies
  (list 'chirp
	'toki
	'local-time))

(loop for dep in dependencies
      do (ql:quickload dep))

(setf *default-pathname-defaults* (truename (sb-posix:getcwd)))
(with-open-file (file "creds.txt" :direction :input)
  (let ((creds (read file)))
    (setf chirp:*oauth-api-key* (getf creds :api-key)
	  chirp:*oauth-api-secret* (getf creds :api-secret)
	  chirp:*oauth-access-token* (getf creds :access-token)
	  chirp:*oauth-access-secret* (getf creds :access-secret))))
(chirp:account/verify-credentials)


(in-package toki)

(defparameter *tweet-limit* 280)

(defun send-tweet (text)
  (if (> (length text) *tweet-limit*)
      (error (format nil "Tweet '~a' too long" text))
      (chirp:statuses/update text)))

(defun log-info (message)
  (write-log "INFO" message))

(defun log-error (message)
  (write-log "ERROR" message))

(defun write-log (type-prefix message)
  (format t "~a [~a] ~a~%" (local-time:format-timestring nil (local-time:now)) type-prefix message))


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


(let ((text (generate-poem *chain* (random-poem-structure))))
  (send-tweet text)
  (log-info (format nil "Sent tweet: '~a'" text)))
