
(ql:quickload :cl-who)
(ql:quickload :parenscript)
(require :sb-bsd-sockets)
(require :sb-concurrency)

#+nil
(eval-when (:compile-toplevel :load-toplevel)
  #+nil (require :cl-who))

;; socket handling in sbcl:
;; https://sourceforge.net/p/sbcl/mailman/message/3493412/

;; intro to vue with aframe:
;; http://blog.diepartments.de/webvr-webgl-vue-js-2-0-and-aframe-js-a-sweet-couple/

;; intro to vue:
;; https://vuejs.org/v2/guide/

(defpackage :serv
  (:use :cl :sb-bsd-sockets :ps))
(in-package :serv)
(use-package :cl-who)

(declaim (optimize (speed 0) (safety 3) (debug 3)))




(with-output-to-string (sm)
  (with-html-output (sm)
    (htm (:html
	  (:head (:title "hello")

		 (:script :src "https://aframe.io/releases/0.6.0/aframe.min.js")
		 (:script :src "https://unpkg.com/vue")
		 (:script (eval `(ps (progn
				       ((@ -Vue component) "simple-scene"
					(create data (lambda () (return (create data null)))
						template (who-ps-html
							  (:a-sphere :position ,(format nil "~{~a~^ ~}" '(0 1.25 -5))
								     :radius ,(format nil "~a" 1.25) :color "#EF2D5E")
							  (:a-plane :position ,(format nil "~{~a~^ ~}" '(0 0 -4))
								    :rotation ,(format nil "~{~a~^ ~}" '(-90 0 0))
								    :width "4"
								    :height "4"
								    :color "#7BC8A4"))))
				       (new (-Vue (create el "#example"))))))))
	  (:body
	   (:div :id "example" (:simple-scene)))))))




(defparameter cont
  (with-output-to-string (sm)
  (with-html-output (sm)
    (htm (:html
	  (:head (:title "hello")
		 (:script :src "https://aframe.io/releases/0.6.0/aframe.min.js")
		 (:script :src "https://unpkg.com/vue")
		 (:script :type "text/javascript"
			  (str (format nil "~%//<![CDATA[~%"))
			  (str (ps (progn
				     ((@ -Vue component) "simple-scene"
				      (create data (lambda () (return (create data null)))
					      template (who-ps-html
							(:a-sphere :position "0 1.25 -5" #+nil (lisp (format nil "~{~a~^ ~}" '(0 1.25 -5)))
								   :radius "1.25" #+nil (lisp (format nil "~a" 1.25)) :color "#0F2D5E")
							(:a-plane :position "0 0 -4" #+nil (lisp (format nil "~{~a~^ ~}" '(0 0 -4)))
								  :rotation "-90 0 0"  #+nil (lisp (format nil "~{~a~^ ~}" '(-90 0 0)))
								  :width "4"
								  :height "4"
								  :color "#0BC8A4"))))
				     (new (-Vue (create el "#example"))))))
			  (str (format nil "~%//]]>~%"))))
	  (:body
	   (:div :id "example" (:simple-scene))))))))

#+nil
(format t "~a" cont)


#+nil
(defparameter cont
  (format nil "
<script type='text/javascript'>
<!--
~a
//-->
</script>"
	  (eval `(ps (defun http-success (r) (try
					      (return (or (and (<= 200 r.status)
							       (< r.status 300))
							  (== 304 r.status)))
					      (:catch (e) (return false))))
		     (setf window.onload
			   (lambda ()
			     (let ((source (new (-event-source "event"))))
			       (source.add-event-listener
				"message"
				(lambda (e)
					;(console.log e.data)
				  (let ((s (document.get-element-by-id "feed")))
				    (setf s.inner-h-t-m-l e.data)))
				false))))))))



(defun init-serv ()
  (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address s) t)
    (socket-bind s (make-inet-address "127.0.0.1") 8080)
    (socket-listen s 5)
    s))



(defun update-swank ()
    (restart-case
	(let ((connection (or swank::*emacs-connection*
			      (swank::default-connection))))
	  (when connection
	    (swank::handle-requests connection t)))
      (continue () :report "Continuable: Continue")))

(defun simple-select (streams)
  (dolist (stream streams)
    (when (listen stream)
      (return-from simple-select stream))))

#+nil
(let ((stream (simple-select (list *socket-stream* *string-stream*))))
  (print stream)
  (read-line stream))


(defun read-get-request (sm)
  (loop for line = (read-line sm)
	while line
	do (let ((index (search "GET" line)))
	     (when index
	       (return-from read-get-request
		 (let ((start (+ index 1 (length "GET"))))
		   (subseq line
			 start
			 (search " " line :start2 start)))))))
  (error "no GET found in request"))

(defparameter *pusher-mb* (sb-concurrency:make-mailbox))


(let ((old-msg ""))
 (defun pusher-kernel (sm)
   (when *pusher-mb*
     (format sm "data: ~a~C~C~C~C"
	     (let ((msg (sb-concurrency:receive-message *pusher-mb* ;:timeout 1
							)))
	       (if msg
		   (setf old-msg msg)
		   (with-output-to-string (sm)
		     (with-html-output (sm)
		       (:a-sphere :position (format nil "~{~a~^ ~}" '(0 1.25 -5))
				  :radius (format nil "~a" 1.25) :color "#EF2D5E")
		       (:a-plane :position (format nil "~{~a~^ ~}" '(0 0 -4))
				 :rotation (format nil "~{~a~^ ~}" '(-90 0 0))
				 :width "4"
				 :height "4"
				 :color "#7BC8A4")))
		   #+nil (format nil "<b>no update</b>~a" old-msg)))
	     #\return #\linefeed #\return #\linefeed))))


(defun pusher (sm)
  (format sm "HTTP/1.1 200 OK~%Content-type: text/event-stream~%~%")
  (loop for i below 10000 do
       (pusher-kernel sm)
       )
  (close sm))

(defun handle-connection (s)
  (let ((sm (socket-make-stream (socket-accept s)
				:output t
				:input t
				:element-type 'character
				:buffering :none)))
    
    ;;(read-sequence a sm)
    (let ((r (read-get-request sm))
	  (cont cont))
      (format t "~a~%" r) 
      ;; 200 means Ok, request fullfilled document follows
      
      (cond ((string= r "/") 
	     (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
	     (with-html-output (sm)
	       (htm (str cont)
		    #+nil(:html
			(:head (:title "hello")
			       (:script :src "https://aframe.io/releases/0.6.0/aframe.min.js"))
			(:body
			 )
			(str cont))))
	     (close sm)) 
	    #+nil ((string= r "/test.txt")
	     (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
	     (format sm "<b>~a</b>" (get-internal-real-time))
	     (close sm))
	    ((string= r "/event")
	     (sb-thread:make-thread 
	      #'(lambda ()
		  (pusher sm))
	      :name "pusher"))
	    (t (format sm "error")
	       (close sm))))))


#+nil
(defvar s (init-serv))
#+nil
(sb-thread:make-thread
 #'(lambda ()
     (loop
	(handle-connection s)))
 :name "handle-connection")
#+nil
(socket-close s) 
