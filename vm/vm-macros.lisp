(in-package :psvm)

(defmacro define-vm-opcode (name bytecode args &body body)
  "Define a new bytecode opcode.

NAME is the assembler name of the opcode; this will be used as the
name of a structure that is generated and as the name of the
constructor for this structure.

BYTECODE is an integer that is currently ignored.  It is intended to
serve as the instruction opcode when we move the interpreter to a real
bytecode some time in the future.

ARGS is a list of defstruct slot-descriptions.  It is used in the
definition of the structure and in the definition of the code block
that should be executed for this bytecode.

BODY is a function body that implements the actual functionality for
this opcode.  The following names are available:

  ENV - the lexical environment of the vm
  STACK - the stack of the vm

Furthermore, the name of each arg in ARGS is symbol-macrolet to the
corresponding struct accessor in the structure."
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
       (defstruct (,name (:include vm-opcode)
			 (:constructor ,name ,arglist))
	 ,@args)
       (defmethod execute-opcode ((self ,name) vm)
	 (symbol-macrolet ((env (environment vm))
			   (stack (stack vm))
			   ,@readers)
	   ,@body)))))
