(in-package :psvm)

;;; VM State.
;;; ========

;;; To speed up changing between processes we keep the state of the VM
;;; in a separate class.  Each process change then simply sets the
;;; VM's state to the state of the new process.

(defclass vm-state ()
  (;; The code for the currently running process.
   (code :accessor code :initarg :code :initform '())
   ;; The program counter for the currently running process.
   (pc :accessor pc :initarg :pc 
       :type non-negative-fixnum :initform 0)
   ;; The global variables of the currently running process.
   (global-variables :accessor global-variables
		     :initarg :global-variables
		     :type hash-table
		     :initform (make-hash-table))
   ;; The lexical environment of the currently running process.
   (environment :accessor environment :initarg :environment)
   ;; The stack of the currently running process.
   (stack :accessor stack :initarg :stack
	  :type list :initform '()))
  (:documentation "The state needed for running a process.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

(defclass vm-state-mixin ()
  (;; Storage for the state of the VM.
   (state :accessor state :initarg :state))
  (:documentation "Mixin for classes that need to save a VM state.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))


;;; Schedulers.
;;; ==========

(defclass vm-scheduler ()
  ()
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

(defgeneric pick-component (scheduler vm)
  (:documentation "Returns the next component that should run or NIL
  if the VM has no executable components."))
(defgeneric pick-process (scheduler vm component)
  (:documentation "Returns the next process of COMPONENT that should
  run, or NIL if a new component should be scheduled."))
(defgeneric steps-for-current-process (scheduler vm process)
  (:documentation "Returns the number of VM opcodes that the current
  process should be run before the scheduler is called again."))

(defclass simple-scheduler (vm-scheduler)
  ((steps-per-process :accessor steps-per-process
		      :initarg :steps-per-process
		      :type (non-negative-fixnum)
		      :initform 20)
   (current-component-index :accessor current-component-index
			    :initarg :current-component-index
			    :initform -1)
   (current-process-index :accessor current-process-index
			  :initarg :current-process-index
			  :initform -1))
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

(defmethod pick-component ((scheduler simple-scheduler) vm)
  (let ((components (active-components vm)))
    (cond ((emptyp components)
	   (setf (current-component-index scheduler) -1
		 (current-process-index scheduler) -1)
	   nil)
	  ((< (current-component-index scheduler) (length components))
	   (incf (current-component-index scheduler)))
	  (t
	   (setf (current-component-index scheduler) 0
		 (current-process-index scheduler) -1)
	   0))))

(defmethod steps-for-current-process ((scheduler simple-scheduler) vm process)
  (steps-per-process scheduler))

;;; Interfaces.
;;; ==========

(defclass vm-interface ()
  ()
  (:documentation "The interface of a component, i.e., the tuples it
accepts from other components.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

(defgeneric tuple-allowed-p (component-or-interface tuple)
  (:documentation "Is TUPLE allowed on INTERFACE?")
  ;; We allow T and NIL for interfaces that allow all tuples or no
  ;; tuples at all, respectively.
  (:method ((interface (eql t)) tuple)
    t)
  (:method ((interface (eql nil)) tuple)
    nil))

;;; Processes.
;;; =========

(defclass vm-process (vm-state-mixin)
  (;; The component to which we belong.
   (component :accessor component :initarg :component
	      :type vm-component))
  (:documentation "A single process.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

;;; Components.
;;; ==========

(deftype process-vector ()
  '(vector vm-process))

(declaim (ftype (function () process-vector) make-process-vector))
(defun make-process-vector ()
  (make-array 10 :adjustable t :fill-pointer 0
	      :element-type 'vm-process))

(defclass vm-component ()
  (;; The VM on which we are running.
   (vm :accessor vm :initarg :vm)
   ;; The interface of this component, i.e., a description of the
   ;; tuples its tuple space accepts.
   (interface :accessor interface :initarg :interface
	      :type (or boolean vm-interface) :initform nil)
   ;; The tuple space of this component.
   (tuple-space :accessor tuple-space :initarg :tuple-space)
   ;; The processes currently running in this component.
   (active-processes :accessor active-processes :initarg :active-processes
		     :type process-vector :initform (make-process-vector))
   ;; The processes in this component which are waiting for input into
   ;; the tuple-space.
   (suspended-processes :accessor suspended-processes
			:initarg :suspended-processes
			:initform '())
   ;; The index of the currently running process in ACTIVE-PROCESSES
   ;; or -1 if no process is able to run.
   (index-of-running-process :accessor index-of-running-process
			     :initarg index-of-running-process
			     :type (and (integer -1) fixnum)
			     :initform -1))
  (:documentation "A component running on the vm.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))


;;; The VM.
;;; ======

(deftype component-vector ()
  "An array of components."
  '(array (or null vm-component) *))

(declaim (ftype (function () component-vector) make-component-vector))
(defun make-component-vector ()
  (make-array 10 :adjustable t :fill-pointer 0
	      :element-type 'vm-component))

(defclass psc-vm (vm-state-mixin)
  (;; The components that are currently running, i.e., that have at
   ;; least one process that is not blocked.
   (active-components :accessor active-components
		      :initarg :active-components
		      :type component-vector
		      :initform (make-component-vector))
   ;; The components that are currently blocked because they need
   ;; input from the tuple space.
   (suspended-components :accessor suspended-components
			 :initarg :suspended-components
			 :type component-vector
			 :initform (make-component-vector))
   ;; The index of the currently running component in the
   ;; ACTIVE-COMPONENTS array, or -1 if no component is executable.
   (index-of-running-component :accessor index-of-running-component
			       :initarg :index-of-running-component
			       :type (and (integer -1) fixnum)
			       :initform -1)
   (scheduler :accessor scheduler :initarg :scheduler
	      :type vm-scheduler :initform (make-instance 'simple-scheduler)))
  (:documentation "The virtual machine for the Proto SCEL Compiler.")
  #+(and sbcl optimize-vm)
  (:metaclass structure-class))

