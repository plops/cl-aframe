
(ql:quickload :cl-who)
(ql:quickload :cl-json)
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
;; https://www.youtube.com/watch?v=z6hQqgvGI4Y&t=710s

;; vue with data server sent data source 
;; https://chrisblackwell.me/server-sent-events-using-laravel-vue/
;; https://www.strehle.de/tim/weblog/archives/2017/06/02/1619

(defpackage :serv
  (:use :cl :sb-bsd-sockets :ps))
(in-package :serv)
(use-package :cl-who)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter cont
  (with-output-to-string (sm)
    (with-html-output (sm)
      (htm (:html
	    (:head (:link :rel "icon" :href "data:;base64,iVBORw0KGgo=")
		   (:title "hello vue and aframe")
		   (:script :src "/aframe.min.js" #+nil "https://aframe.io/releases/0.6.0/aframe.min.js")
		   (:script :src "/vue.js" #+nil "https://unpkg.com/vue")
		   )
	    (:body
	     
	     (:div :id "app"
		   (:my-ul
		    (:li :v-for "item in data"
			 "{{ item.message }}")))
	     #+nil
	     (:ul :id "example" (:simplescene))
	     
	     (:script :type "text/javascript"
		      (str (format nil "~%//<![CDATA[~%"))
		      (str (ps (progn
				 #+nil (new (-Vue (create el "#simple-scene"
						    data (create data (array (create message "foo")
									      (create message "bar"))))))
				 ((@ -Vue component) "my-ul"
				  (create data (lambda ()
						 ;; data must be a function
						 ;; https://vuejs.org/v2/guide/components.html
						 (return (create data (array (create message "foo")
									     (create message "bar"))
								 )))

					  ;template (who-ps-html (:ul ))
					 
					  #+nil template #+nil  (who-ps-html
						     #+kil
						     (:p "{{ data.length }}")
						     #+nil (:my-item :v-for "item :in data"
							       :key "item.cnt"
							       
							       :|v-bind:item| "item"
							       "{{ item }} ")
						    (:li :v-for "(it) :of data"
							       "{{ it }}"))))
				 
				 #+nil ((@ -Vue component) "simplescene"
				  (create data (lambda ()
						 ;; data must be a function
						 ;; https://vuejs.org/v2/guide/components.html
						 (return (create data
								 (array
								  (create id 1
									  position "1 1 1"
									  material "color: red"
									  scale "1 1 1"
									  geometry "primitive: box")
								  (create id 2
									  position "2 2 1"
									  material "color: green"
									  scale "2 2 2"
									  geometry "primitive: box"))
								 #+nil (-J-S-O-N.parse (lisp (with-output-to-string (sm)
											 (json:encode-json '#(((id . 1)
													       (position . "1 1 1")
													       (material . "color: red")
													       (scale . "1 1 1")
													       (geometry . "primitive: box"))
													      ((id . 2)
													       (position . "2 2 1")
													       (material . "color: green")
													       (scale . "2 2 2")
													       (geometry . "primitive: box")))
													   sm)
											 ))))))
					  created (lambda ()
						    ;; this code runs after instance is created
						    (this.setup-stream)
						    (console.log "created ran .. "))
					  methods (create
						    "setupStream" (lambda ()
								  ;; connect to event stream of the server
								    ;; if the server sends a message, parse it as json and store in the instances data field
								    (console.log "setup-stream called ..")
								    (let ((es (new (-event-source "./event"))))
								      (es.add-event-listener "message"
											     (lambda (event)
											       (console.log "setup-stream.message called ..")
											       #+nil (setf this.items (-J-S-O-N.parse event.data))
											       (console.log "event received" event.data)
											       null)
											   false)
								    (es.add-event-listener "error"
											   (lambda (event)
											     (console.log "setup-stream.error called ..")
											     (if (== -event-source.-C-L-O-S-E-D event.ready-state)
												 (console.log "event was closed"))
											     null)
											   false)
								    null)))
					  template (who-ps-html
						    ;; :a-scene
						    (:template :v-for "(item,idx) in items"
							       (:a-entity ;:|v-bind:id| "item.id"
									  :|v-bind:geometry| "item.geometry"
									  :|v-bind:position| "item.position"
									  :|v-bind:scale| "item.scale"
									  :|v-bind:material| "item.material"))
						    #+nil
						    (:a-sphere :position "0 1.25 -5" #+nil (lisp (format nil "~{~a~^ ~}" '(0 1.25 -5)))
							       :radius "1.25" #+nil (lisp (format nil "~a" 1.25)) :color "#0F2D5E")
						    #+nil
						    (:a-plane :position "0 0 -4" #+nil (lisp (format nil "~{~a~^ ~}" '(0 0 -4)))
							      :rotation "-90 0 0"  #+nil (lisp (format nil "~{~a~^ ~}" '(-90 0 0)))
							      :width "4"
							      :height "4"
							      :color "#0BC8A4"))))
				 (new (-Vue (create el "#app")))
				 )))
		      (str (format nil "~%//]]>~%")))))))))

#+nil
(format nil "~a" cont)



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

#+nil
(sb-concurrency:send-message *pusher-mb* "hello")


(let ((old-msg ""))
  (defun pusher-kernel (sm)
    (sleep .1)
   (when *pusher-mb*
     (format sm "data: ~a~C~C~C~C"
	     (let ((msg (sb-concurrency:receive-message *pusher-mb* :timeout 10
							)))
	       (if msg
		   (progn
		     (setf old-msg msg)
		     (json:encode-json '#(((id . 1)
					   (position . "1 1 1")
					   (material . "color: red")
					   (scale . "1 1 1")
					   (geometry . "primitive: box"))
					  ((id . 2)
					   (position . "2 2 1")
					   (material . "color: green")
					   (scale . "2 2 2")
					   (geometry . "primitive: box")))
				       sm)
		     #+nil
		     (with-output-to-string (sm)
			     (with-html-output (sm)
			       (:a-sphere :position (format nil "~{~a~^ ~}" '(0 1.25 -5))
					  :radius (format nil "~a" 1.25) :color "#EF2D5E")
			       (:a-plane :position (format nil "~{~a~^ ~}" '(0 0 -4))
					 :rotation (format nil "~{~a~^ ~}" '(-90 0 0))
					 :width "4"
					 :height "4"
					 :color "#7BC8A4"))))
		   (json:encode-json '#(((id . 1)
					   (position . "1 1 1")
					   (material . "color: red")
					   (scale . "1 1 1")
					 (geometry . "primitive: box"))
					)
				     sm)
		   #+nil (format nil "<b>no update</b>~a" old-msg)))
	     #\return #\linefeed #\return #\linefeed))))
#+nil
(with-output-to-string (sm)
  (with-html-output (sm)
    (:form
     :|v-on:submit.prevent| "run")))

(defun pusher (sm)
  (format sm "HTTP/1.1 200 OK~%Content-type: text/event-stream~%~%")
  (loop for i below 10000 do
       (pusher-kernel sm)
       )
  (close sm))

(defparameter *vue.js*
 (with-open-file (s "vue.js")
   (let ((a (make-string (file-length s))))
     (read-sequence a s)
     a)))


(defparameter *backend.js*
 (with-open-file (s "backend.js")
   (let ((a (make-string (file-length s))))
     (read-sequence a s)
     a)))

(defparameter *aframe.min.js*
 (with-open-file (s "aframe.min.js")
   (let ((a (make-string (file-length s))))
     (read-sequence a s)
     a)))

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
	    ((string= r "/vue.js") 
	     (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
	     (write-sequence *vue.js* sm)
	     (close sm))
	    ((string= r "/backend.js") 
	     (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
	     (write-sequence *backend.js* sm)
	     (close sm))
	    ((string= r "/aframe.min.js") 
	     (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
	     (write-sequence *aframe.min.js* sm)
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
