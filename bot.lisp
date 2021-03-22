(defparameter dependencies
  (list 'chirp
	'toki))

(loop for dep in dependencies
      do (ql:quickload dep))

(in-package toki)

(defparameter *tweet-limit* 280)
(defparameter *poem-structures*
  (list "5A 7B 5C" ; haiku
	"11A 11A / 6B 6B 11A" ; limerick
	"10A 10B 10A 10B / 10G 10G" ; shortened sonnet (fits within tweet)
	"5A 7B 5A / 5B" ; something I made up
	))
(defparameter *chain* (make-toki-chain "corpus.txt"))
(defparameter *tweet-period-seconds* (* 60 60 24))
(defparameter *last-attempt-seconds* 0)

(chirp:initiate-authentication
 :api-key "D1pMCK17gI10bQ6orBPS0w"
 :api-secret "CBw2ugt9LIfInXV12b3itq7zsu7t8duYWcH2GsvdUDvR9mS3HE")



(defun send-tweet (text)
  (when (< *tweet-limit* (length text))
    (error (format nil "Tweet ~a too long" text)))
  ;; TODO actually send it
  )

(defun get-last-tweet-timestamp ()
  "Returns timestmap of the bot's last tweet, in seconds."
  ;; TODO
  )

(defun seconds-til-next-tweet ()
  (max
   0
   (- (+ (max *last-attempt-seconds*
	      (get-last-tweet-timestamp))
	 *tweet-period-seconds*)
      (current-time-seconds))))

(defun random-poem-structure ()
  (setf *random-state* (make-random-state t))
  (nth (random (length *poem-structures*)) *poem-structures*))

(defun log-info (message &rest fmt-args)
  ;; TODO
  )

(defun log-error (message &rest fmt-args)
  ;; TODO
  )

(loop repeat
      (handler-case
	  (unwind-protect
	       (let ((seconds-remaining (seconds-til-next-tweet)))
		 (if (zerop seconds-remaining)
		     (let ((text (generate-poem *chain* (random-poem-structure))))
		       (send-tweet text)
		       (log-info "Sent tweet ~a" text))
		     (progn (sleep seconds-remaining)
			    (log-info "Sleeping ~a seconds until next tweet." seconds-remaining))))
	    (setf *last-attempt-seconds* (current-time-seconds)))
	(error (condition)
	  (log-error (message condition)))))
