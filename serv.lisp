
(ql:quickload :cl-who)
(require :sb-bsd-sockets)
(require :sb-concurrency)

#+nil
(eval-when (:compile-toplevel :load-toplevel)
  #+nil (require :cl-who))

;; https://sourceforge.net/p/sbcl/mailman/message/3493412/

(defpackage :serv
  (:use :cl :sb-bsd-sockets))
(in-package :serv)
(use-package :cl-who)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter cont "
<script type='text/javascript'>
<!--
function httpSuccess(r){
    try{
	return (r.status>=200 && r.status<300) || // anything in 200 range is good
        r.status==304; // from browser cache
    } catch(e){}
    return false;
}
window.onload=function(){
    var source = new EventSource('event');
    source.addEventListener('message',function(e){
	console.log(e.data);
	var s=document.getElementById('feed');
	setTimeout( function(){ s.innerHTML=e.data; }, 300);
    },false);  
}
//-->
</script>")

;; var c=new XMLHttpRequest();
;;     c.onreadystatechange=function(){
;; 	if(c.readyState==4){
;; 	    if(httpSuccess(c)){
;; 		var s=document.getElementById('feed');
;;                 setTimeout(function(){s.innerHTML=c.responseText;},300);
;; 	    }
;; 	    c=null; 
;; 	}
;;     }
;;     c.open('GET','test.txt');
;;     c.send('127.0.0.1');

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <!DOCTYPE html>										       ;;
;; <html>											       ;;
;;   <head>											       ;;
;;     <title>Hello, WebVR! - A-Frame</title>							       ;;
;;     <meta name="description" content="Hello, WebVR! - A-Frame">				       ;;
;;     <script src="https://aframe.io/releases/0.6.0/aframe.min.js"></script>			       ;;
;;   </head>											       ;;
;;   <body>											       ;;
;;     <a-scene>										       ;;
;;       <a-sphere position="0 1.25 -5" radius="1.25" color="#EF2D5E"></a-sphere>		       ;;
;;       <a-plane position="0 0 -4" rotation="-90 0 0" width="4" height="4" color="#7BC8A4"></a-plane> ;;
;;       <a-sky color="#ECECEC"></a-sky>							       ;;
;;     </a-scene>										       ;;
;;   </body>											       ;;
;; </html>											       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; => "<table><tr><td>3</td><td>3</td><td>3</td><td>3</td><td>3</td></tr><tr><td>3</td><td>3</td><td>3</td><td>3</td><td>3</td></tr></table>" [3 times]

#+nil
(dotimes (i 10)
 (sleep .1)
 (sb-concurrency:send-message
  *pusher-mb*
  (with-output-to-string (sm)
    (with-html-output (sm)
      (:a-sphere :position (format nil "~{~a~^ ~}" (list i 1.25 -5))
		 :radius (format nil "~a" 1.25) :color "#EF2D5E")
      (:a-plane :position (format nil "~{~a~^ ~}" '(0 0 -4))
		:rotation (format nil "~{~a~^ ~}" '(-90 0 0))
		:width "4"
		:height "4"
		:color "#7BC8A4")))))

(with-output-to-string (sm)
     (with-html-output (sm)
       (:div :id "feed"

	     
	     (:a-scene
	      (:a-sphere :position (format nil "~{~a~^ ~}" '(0 1.25 -5))
			 :radius (format nil "~a" 1.25) :color "#EF2D5E")
	      (:a-plane :position (format nil "~{~a~^ ~}" '(0 0 -4))
			:rotation (format nil "~{~a~^ ~}" '(-90 0 0))
			:width "4"
			:height "4"
			:color "#7BC8A4")))
       ))

#+nil
(dotimes (k 10)
  (sleep .01)
  (sb-concurrency:send-message
   *pusher-mb*
   (with-output-to-string (sm)
     (with-html-output (sm)
       (:table
	(loop for i below 25 by 5 do
	     (htm (:tr
		   (loop for j from i below (+ i 5)
		      do
			(htm (:td
			      (if (= j 11)
				  (htm (:font :color "red"
					      (fmt "~a" (get-internal-run-time))))
				  (fmt "~a" k)))))))))))))

(let ((old-msg ""))
 (defun pusher-kernel (sm)
   (when *pusher-mb*
     (format sm "data: ~a~C~C~C~C"
	     (let ((msg (sb-concurrency:receive-message *pusher-mb* :timeout 1)))
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
       (pusher-kernel sm))
  (close sm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <head>								      ;;
;;     <title>Hello, WebVR! - A-Frame</title>				      ;;
;;     <meta name="description" content="Hello, WebVR! - A-Frame">	      ;;
;;     <script src="https://aframe.io/releases/0.6.0/aframe.min.js"></script> ;;
;; </head>								      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	       (htm (:html
		     (:head (:title "hello")
			    (:script :src "https://aframe.io/releases/0.6.0/aframe.min.js"))
		     (:body
		      (:a-scene :id "feed"
				(:a-sphere :position (format nil "~{~a~^ ~}" '(0 1.25 -5))
					   :radius (format nil "~a" 1.25) :color "#EF2D5E")
				(:a-plane :position (format nil "~{~a~^ ~}" '(0 0 -4))
					  :rotation (format nil "~{~a~^ ~}" '(-90 0 0))
					  :width "4"
					  :height "4"
					  :color "#7BC8A4")))
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
