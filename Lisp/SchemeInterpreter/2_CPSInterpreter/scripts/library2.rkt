; Various library form

(defmacro let (_ fname name-value-list . exp-list)
  `((lambda () 
      (define ,fname (lambda ,(map car name-value-list) ,@exp-list)) 
      (,fname ,@(map cadr name-value-list))))
  )

(defmacro let (_ ((name value) . rest-name-values) . exp-list)
  `((lambda ,(cons name (map car rest-name-values)) ,@exp-list) ,@(cons value (map cadr rest-name-values)))
  )

(defmacro let* (_ name-value-list . exp-list)
  (if (= 1 (length name-value-list))
    `(let ,name-value-list ,@exp-list)
    `(let ,(list (car name-value-list)) (let* ,(cdr name-value-list) ,@exp-list)))
  )

(defmacro letrec (_ name-value-list . exp-list)
  `((lambda ,(map car name-value-list)
      ,@(map (lambda (pair) `(set! ,(car pair) ,(cadr pair))) name-value-list)
      ,@exp-list) 
    ,@(map (lambda (pair) ''undefine) name-value-list))
  )

(defmacro cond (_ (pred . exp-list) . rest-list)
  (if (empty? rest-list)
    `(begin ,@exp-list)
    `(if ,pred (begin ,@exp-list) (cond ,@rest-list)))
  )

(defmacro and (_ . exp-list)
  (if (= 1 (length exp-list))
    (car exp-list)
    `(if ,(car exp-list) (and ,@(cdr exp-list)) false))
  )

(defmacro or (_ . exp-list)
  (let ([tmp-name (gensym)])
    (if (= 1 (length exp-list))
      (car exp-list)
      `(let ([,tmp-name ,(car exp-list)]) 
         (if ,tmp-name
           ,tmp-name
           (or ,@(cdr exp-list))))))
  )

(defmacro do (_ ([name init update] . rest-vars) (pred . exit-list) . do-list)
  (let ([fname (gensym)])
    `(let ,fname ,(cons (list name init) (map (lambda (triple) (list (car triple) (cadr triple))) rest-vars))
       (if ,pred
         (begin the-void ,@exit-list)
         (begin ,@do-list
                (,fname ,@(cons update (map caddr rest-vars)))))))
  )
