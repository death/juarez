;;;; +----------------------------------------------------------------+
;;;; | JUAREZ - Warez tools                               DEATH, 2010 |
;;;; +----------------------------------------------------------------+

(in-package #:juarez.prolog)


;;;; Prolog interpreter (mostly from PAIP)

(defconstant fail nil
  "Indicates pat-match failure.")

(defparameter no-bindings '((t . t))
  "Indicates pat-match success, with no bindings.")

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (acons var val
         (if (eq bindings no-bindings)
             '()
             bindings)))

(defun match-variable (var input bindings)
  "Does var match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((null binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defparameter *occurs-check* t
  "Should we do the occurs check?")

(defun unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((equal x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings)))
        (t fail)))

(defun unify-variable (var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable-p x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x)
         (or (occurs-check var (car x) bindings)
             (occurs-check var (cdr x) bindings)))
        (t nil)))

(defun reuse-cons (x y c)
  "If c is (x . y) then return it, otherwise cons x and y."
  (if (and (eql (car c) x) (eql (cdr c) y))
      c
      (cons x y)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x, taking
recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

(defun unifier (x y)
  "Return something that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))

(defun clause-head (clause) (car clause))
(defun clause-body (clause) (cdr clause))

(defun get-clauses (pred) (get pred 'clauses))
(defun predicate (relation) (car relation))

(defvar *db-predicates* '()
  "A list of all predicates stored in the database.")

(defmacro <- (&rest clause)
  "Add a clause to the database."
  `(add-clause ',(replace-?-vars clause)))

(defun add-clause (clause)
  "Add a clause to the database, indexed by head's predicate."
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (variable-p pred))) ()
            "Predicate must be a non-variable symbol.")
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred) (list clause)))
    pred))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the database."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (pred)
  "Remove the clauses for a single predicate."
  (setf (get pred 'clauses) '()))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to a goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some (lambda (clause)
                (let ((new-clause (rename-variables clause)))
                  (prove-all (append (clause-body new-clause) other-goals)
                             (unify goal (clause-head new-clause) bindings))))
              clauses)
        (funcall clauses (cdr goal) bindings other-goals))))

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) (list bindings))
        (t (prove (first goals) bindings (rest goals)))))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar (lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun variables-in (exp)
  "Return a list of all the variables in exp."
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if predicate
                               (car tree)
                               (unique-find-anywhere-if predicate
                                                        (cdr tree)
                                                        found-so-far))))

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (car exp))
                       (replace-?-vars (cdr exp))
                       exp))))

(defmacro ?- (&rest goals)
  `(top-level-prove ',(replace-?-vars goals)))

(defvar *top-level-solutions*)
(defvar *continue-p* (constantly t))

(defun top-level-prove (goals)
  "Prove the goals and return bindings for top-level variables."
  (let ((*top-level-solutions* '()))
    (prove-all `(,@goals (top-level-solutions ,@(variables-in goals))) no-bindings)
    *top-level-solutions*))

(setf (get 'top-level-solutions 'clauses) 'top-level-solutions)

(defun top-level-solutions (vars bindings other-goals)
  "Add solutions to *top-level-solutions* while *continue-p* returns true."
  (push (if (null vars)
            't
            (top-level-solution vars bindings))
        *top-level-solutions*)
  (if (funcall *continue-p*)
      fail
      (prove-all other-goals bindings)))

(defun top-level-solution (vars solution)
  "Return only the top-level part of the solution."
  (mapcar (lambda (var)
            (cons var
                  (subst-bindings solution var)))
          vars))

(defmacro do-answers (goals &body forms)
  (let ((vars (remove '? (variables-in goals))))
    `(block nil
       (top-level-do-answers ',(replace-?-vars goals)
                             (lambda ,vars
                               (declare (ignorable ,@vars))
                               ,@forms)
                             ',vars))))

(defun top-level-do-answers (goals function vars)
  (prove-all `(,@goals (top-level-solutions-for-function ,function ,vars)) no-bindings)
  (values))

(setf (get 'top-level-solutions-for-function 'clauses) 'top-level-solutions-for-function)

(defun top-level-solutions-for-function (stuff bindings other-goals)
  (declare (ignore other-goals))
  (destructuring-bind (function vars) stuff
    (apply function (mapcar (lambda (var) (subst-bindings bindings var)) vars)))
  fail)

(defun add-facts (facts)
  (dolist (fact facts)
    (add-clause (replace-?-vars (list fact)))))

(defmacro declare-prolog-predicate (name)
  `(progn
     (setf (get ',name 'clauses)
           (lambda (vars bindings other-goals)
             (if (apply (function ,name) (subst-bindings bindings vars))
                 (prove-all other-goals bindings)
                 fail)))
     ',name))
