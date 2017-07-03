(load #p"~/.sbclrc")
(require 'lquery)
(require 'drakma)
(require 'cl-date-time-parser)
(require 'local-time)
(defparameter url "http://www.detectiveconanworld.com/wiki/Anime")
(defvar text (drakma:http-request url))
(lquery:$ (initialize text))

(defun is-season (title)
  (equal 0 (search "Season" title)))

(defvar seasons)
(setf seasons (lquery:$ ".seasontable"))
;; (length seasons)

(defun get-episode (season episode)
  (lquery:$ ".seasontable"
	    (inline (lambda (seasons) (aref seasons (- season 1))))
	    (children)
	    (inline (lambda (episodes) (aref episodes (+ episode 1))))
	    (children)
	    (combine (text))))

(defun eps-count (season)
  (let ((eps (length (lquery:$ ".seasontable"
		       (inline (lambda (seasons) (aref seasons (- season 1))))
		       (children)))))
    (- eps 1)))

(defun ep-n (ep) (string-trim " " (car (aref ep 0))))
(defun ep-t (ep) (string-trim " " (car (aref ep 2))))
(defun ep-d (ep) (string-trim " " (car (aref ep 3))))
(defun ep-f (ep) (string-trim " " (car (aref ep 6))))

;;; time functions
(defun time-from-string (time-string)
  (let ((universal-time (cl-date-time-parser:parse-date-time time-string)))
    (local-time:universal-to-timestamp universal-time)))

(defun time-ep-engsub (time)
  (let* ((weeks 0)
	 (days 0)
	 (hours 11)
	 (minutes 30)
	 (days-delay (+ days (* 7 weeks)))
	 (hours-delay (+ hours (* 24 days-delay)))
	 (minutes-delay (+ minutes (* 60 hours-delay))))
    (local-time:timestamp+ time minutes-delay :minute)))

(defun date-time (time)
  (local-time:format-timestring nil time
				:format `(:long-weekday " " :long-month " " :day ", " :year
							" " (:HOUR 2) #\: (:MIN 2))))

;; example format
;;  ((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) #\T (:HOUR 2) #\: (:MIN 2) #\:
;;   (:SEC 2) #\. (:USEC 6) :GMT-OFFSET-OR-Z)

(defun date-short (time)
  (local-time:format-timestring nil time :format `((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2))))

(defun date-time-short (time)
  (local-time:format-timestring nil time
				:format `((:YEAR 4) #\- (:MONTH 2) #\- (:DAY 2) " " (:HOUR 2) #\: (:MIN 2))))

;;; print functions
(defun limit-string (string n)
  (if (> (length string) n) (subseq string 0 (- n 1)) string))

(defun print-episode (ep)
  (let* ((number (ep-n ep))
	 (title (limit-string (ep-t ep) 32))
	 (date (time-from-string (string-trim " " (ep-d ep))))
	 (date-string (date-short date))
	 (date2-string (date-time-short (time-ep-engsub date)))
	 (file (limit-string (ep-f ep) 8)))
    (format t "~&Ep. ~4A~9A~12A~18A~A" number file date-string date2-string title)))

;; (print-episode (get-episode 15 7))
;; (print-episode (get-episode 21 14))

(defun print-season (season)
  (dotimes (i (eps-count season))
    (print-episode (get-episode season i))))

(defun ask-season-loop ()
  (format t "~&Print season: ")
  (finish-output)
  (let ((x (read)))
    (if (equal x 'q)
	(format t "~&Goodbye!~%~%")
	(progn (if (and (numberp x) (> x 0) (<= x (length seasons)))
		   (print-season x))
	       (ask-season-loop)))))

(defun main ()
  (let ((last-season (length seasons)))
    (print-season last-season)
    (format t "~&Last season is ~A" last-season)
    (format t "~&Type 'q' to exit")
    (ask-season-loop)))

(main)
