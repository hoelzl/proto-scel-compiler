(in-package :psvm)

(defmacro define-vm-instruction (name bytecode args &body body)
  (declare (ignore bytecode))
  (let* ((arglist (mapcar (lambda (arg)
			   (if (consp arg)
			       (destructuring-bind
				     (slot-name init &key &allow-other-keys) arg
				 (declare (ignore init))
				 slot-name)
			       arg))
			 args))
	(readers  (mapcar (lambda (slot-name)
			    (list slot-name `(,(symbolicate name "-" slot-name) self)))
			  arglist)))
    `(progn
       (defstruct (,name (:include vm-instruction)
			 (:constructor ,name ,arglist))
	 ,@args)
       (defmethod execute-instruction ((self ,name) vm)
	 (symbol-macrolet ((env (environment vm))
			   (stack (stack vm))
			   ,@readers)
	   ,@body)))))
