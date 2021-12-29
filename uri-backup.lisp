(defun identificatoreHelper (lista &optional listaDelimitatori listaBannati accumulatore)
    (cond   ((null lista) nil)
            ((member (car lista) listaBannati) nil)
            ((member (car lista) listaDelimitatori)
                (values (nreverse accumulatore)  lista))
            (T (identificatoreHelper 
                 (cdr lista)
                 listaDelimitatori
                 listaBannati
                 (cons (car lista) accumulatore)
               )
            )
    )
)

(defun identificatore (lista &optional listaDelimitatori listaBannati)
    (let ((helperReturn 
            (multiple-value-list 
                (identificatoreHelper lista listaDelimitatori listaBannati)
            )
          )
         )
        (if (first helperReturn) (values-list helperReturn) (values nil lista))
    )
)

(defun idHelperPort (lista &optional listaDelimitatori accumulatore)
    (cond ((null lista) nil)
          ((member (car lista) listaDelimitatori)
           (values (nreverse accumulatore) lista)
          )
          ((null (digit-char-p (car lista))) nil)
          (T (idHelperPort (cdr lista)
                           listaDelimitatori
                           (cons (car lista) accumulatore)
             )
          )
    )
)

(defun identificatorePort (lista &optional listaDelimitatori)
    (let ((helperReturn
            (multiple-value-list
                (idHelperPort lista listaDelimitatori)
            )
          )
         )
         (if (first helperReturn) (values-list helperReturn) (values 80 lista))
    )
)

(defun fragment (lista)
    (cond ((eq (car lista) #\#)
            (multiple-value-bind
                (parsedFragment remaining)
                (identificatore (cdr lista) (list 'end))
                (values parsedFragment remaining)
            )
            )
            (T  nil)
    )
)


(defun query (lista)
    (cond ((eq (car lista) #\?)
            (multiple-value-bind
                (parsedQuery remaining)
                (identificatore (cdr lista) (list #\# 'end))
                (values parsedQuery remaining)
            )
          )
          (T (values nil lista))
    )
)


(defun path (lista)
    (multiple-value-bind
        (parsedPath remaining)
        (identificatore lista (list #\/ #\? 'end) (list #\# #\@ #\:))
        (cond ((eq (car remaining) #\/)
                (append parsedPath '(#\/) (path (cdr remaining)))
            )
            (T (values parsedPath remaining))
        )
    )
)

(defun subdomain (lista)
    (cond ((eq (car lista) #\/)
            (let* (
                    (Path
                        (multiple-value-list
                         (path (cdr lista))
                        )
                    )
                    (Query
                        (multiple-value-list
                         (query (second Path))
                        )
                    )
                    (Fragment
                        (multiple-value-list
                         (fragment (second Query)))
                    )
                )
                (values (first Path) (first Query) (first Fragment))
            )
          )
          (T (values nil nil nil))
    )
)


(defun port (lista)
    (cond ((eq (car lista) #\:)
            (multiple-value-bind
                (parsedPort remaining)
                (identificatorePort (cdr lista) (list #\/ 'end))
                (values parsedPort remaining)
            )
          )
          (T (values 80 lista))
    )
)


(defun host (lista)
   (multiple-value-bind
    (parsedHost remaining)
    (identificatore lista (list #\. #\/ #\: 'end) (list #\? #\# #\@))
    (cond ((eq (car remaining) #\.)
            (append parsedHost '(#\.) (host (cdr remaining)))
          )
          (T (values parsedHost remaining))
    )
   )
)


(defun userinfo (lista)
    (multiple-value-bind
        (parsedUserinfo remaining)
        (identificatore lista (list #\@) (list #\/ #\? #\# #\:))
        (cond ((null parsedUserinfo) (values nil remaining))
               (T (values parsedUserinfo remaining))
        )
    )
)


(defun authority (lista)
    (cond ((and
                (eq (first lista) #\/)
                (eq (second lista) #\/)
           )
            (let* (
                  (Userinfo
                    (multiple-value-list
                        (userinfo (cdr (cdr lista)))
                    )
                  )
                  (Host
                    (cond ((null (first Userinfo)) 
                        (multiple-value-list
                            (host (second Userinfo))))
                        (T (multiple-value-list
                            (host (cdr (second Userinfo)))
                            )
                        )
                    )
                  )
                  (Port
                    (cond ((null (first Host)) (error "Uri non valido"))
                            (T (multiple-value-list
                                (port (second Host)))
                            )
                    )
                  )
                )
                (values (first Userinfo) (first Host) (first Port))
            )
        )
        ((eq (car lista) #\/) (values nil nil nil lista))
        ((eq (car lista) 'end) (values nil nil nil 'end))
        (T (error "Uri non valido"))
    )
)


(defun helpuri (lista)
    (let* ((Authority
            (multiple-value-list
                (authority (cdr lista))
            )
           )
           (Subdomain
            (cond ((eq (fourth Authority) 'end) 
                        (values nil nil nil)
                  )
                    (T (multiple-value-list 
                            (subdomain (fourth Authority))
                        )
                    )
            )
           )
           (helpUriReturn
            (append (list (first Authority) 
                          (second Authority) 
                          (third Authority)) 
                          Subdomain
            )
           )
          )
          helpUriReturn
    )
)



(defun scheme (lista)
    (multiple-value-bind
        (parsedScheme remaining)
        (identificatore lista (list #\:) (list #\/ #\? #\# #\@))
        (values parsedScheme remaining)
    )
)

(defun helpScheme (lista)
    (multiple-value-bind
        (parsedScheme remaining)
        (scheme lista)
        (cond ((null parsedScheme) nil)
                ;controllo scheme speciale
               (T (values parsedScheme (helpuri remaining)))
        )
    )
)

(defun uri-parse (uriString)
    (helpScheme (append (coerce uriString 'list) (list 'end)))
)


#| 
TODO
 - Host come IP
 - gestione errori per caratteri non ammessi
 - return del resto della lista in host e path
 - schemi speciali
 - uri display
|#