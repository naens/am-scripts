(load #p"~/.sbclrc")
(require 'lquery)
(require 'drakma)
(require 'cl-date-time-parser)
(require 'local-time)

(defparameter *url* "http://mangastream.com/")
(defvar *text*)

(defun trim (string)
  (string-trim " "  string))

(defun get-element (n)
  (let ((q (lquery:$ ".side-nav" "ul" "li"
	    (contents)
	    (inline (lambda (e) (aref e n)))
	    (combine "span" (contents)  "strong" "em")
	    (map-apply #'(lambda (e1 e2 e3 e4)
	    		   (list (lquery-funcs:text e1)
				 (lquery-funcs:text (lquery-funcs:filter e2 #'plump:text-node-p))
				 (lquery-funcs:text e3)
				 (lquery-funcs:text e4)))))))
    (mapcar #'(lambda (s) (trim (aref s 0))) (aref q 0))))

(defun count-elements ()
  (length (lquery:$ ".side-nav" "ul" "li")))

(defun limit-string (string n)
  (if (> (length string) n) (subseq string 0 (- n 1)) string))

(defun print-element (element)
  (let* ((date (nth 0 element))
	 (manga (limit-string (nth 1 element) 22))
	 (chapter (limit-string (nth 2 element) 10))
	 (title (limit-string (nth 3 element) 32)))
    (format t "~&~12A~24A~12A~A" date manga chapter title)))
;; (+ 12 24 12 32)

(defun make-random (value range)
  (if (> range 0)
      (let* ((base (- value range))
	     (var (random (* 2.0 range))))
	(+ base var))
      value))

(defvar wait-min 15)

;; sleep time + or - 1 min
;; if minutes <= 0: don't sleep
(defun sleep-minutes (minutes)
  (if (> minutes 0)
      (let* ((seconds (* 60 minutes))
	     (var (if (< seconds 60) (/ seconds 3.5) 60))
	     (sleep-seconds (make-random seconds var)))
	(format t "~&Sleep ~A minutes " (/ sleep-seconds 60))
	(finish-output)
	(sleep sleep-seconds))))

;;; TODO: notify on new elements (use notify-send string -t expire-time-ms)
;;; TODO (2): compare if last elements are equal after update
;;; TODO (3): send notification with 5 last elements on the list
(defvar *elements*)

(defun get-elements ()
  (let ((result nil))
    (dotimes (i (count-elements))
      (push (get-element i) result))
    result))

(defun now-string ()
  (multiple-value-bind (second minute hour) (get-decoded-time)
    (declare (ignore second))
    (format nil "~2,'0d:~2,'0d" hour minute)))

(defvar last-update-string)

(defun update-now ()
  (format t "~&Updating...")
  (finish-output)
  (setf *text* (drakma:http-request *url*))
  (lquery:$ (initialize *text*))
  (setf last-update-string (now-string)))

(defun print-all ()
  (dolist (e *elements*)
    (print-element e)))
  

;; init:
;; + update
;; + print list
(defun init ()
  (update-now)
  (setf *elements* (get-elements))
  (print-all))

(defun last-elements (n)
  (let* ((result nil)
	 (tot (length *elements*))
	 (times (min n tot)))
    (dotimes (i times)
      (push (get-element i) result))
    result))

(defun list-to-string (l s)
  (let ((result ""))
    (dolist (e l)
      (print e)
      (print s)
      (setf result (concatenate 'string result s (second e) " " (third e))))
    result))

(defun get-last ()
  (list-to-string (last-elements 8) "\\n"))

;; + update
;; + compare last elements
;; + if same: print: no change
;; + if change: print new list, set *elements*, notify
(defun upd-check ()
  (update-now)
  (let ((new-elements (get-elements)))
    (if (equal (last new-elements) (last *elements*))
	(progn
	  (format t "~&No change.")
	  (finish-output))
	(progn				; TODO: notify: print last new elements
	  (setf *elements* (get-elements))
	  (run-program "/usr/bin/notify-send" `("Latest Manga" ,(get-last)))
	  (print-all)))))


;; state entered if user presses ^C while in sleep mode
(defun interrupt-state ()
  (format t "~&Change wait time (currently ~A minutes)?"  wait-min)
  (format t "~&Enter number: wait minutes, u: update now, c: continue, q or e: exit) ")
  (finish-output)
  (let ((answer (read)))
    (cond ((symbolp answer) (case answer
			      (u (progn
				   (upd-check)
				   (print-loop)))
			      (c (print-loop))))
	  ((and (numberp answer) (> answer 0 ))
	   (progn
	     (setf wait-min answer)
	     (print-loop))))))

;; print-loop:
;; + sleep
;; + upd-check
(defun print-loop ()
  (format t "~&Last update: ~A. Wating ~A minutes.~%" last-update-string wait-min)
  (finish-output)
  (handler-case
      (progn
	(sleep-minutes wait-min)
	(upd-check)
	(print-loop))
    (SB-SYS:INTERACTIVE-INTERRUPT (e)
      (declare (ignorable e))
      (interrupt-state))))

(init)
(print-loop)
