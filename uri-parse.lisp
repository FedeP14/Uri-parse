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
          ((null (digit-char-p (car lista))) (error "port solo numeri"))
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
                (values (coerce parsedFragment 'string) remaining)
            )
            )
            (T  (values (coerce nil 'string) lista))
    )
)


(defun query (lista)
    (cond ((eq (car lista) #\?)
            (multiple-value-bind
                (parsedQuery remaining)
                (identificatore (cdr lista) (list #\# 'end))
                (values (coerce parsedQuery 'string) remaining)
            )
          )
          (T (values (coerce nil 'string) lista))
    )
)


(defun path (lista)
    (multiple-value-bind
        (parsedPath remaining)
        (identificatore lista (list #\/ #\? #\# 'end) (list #\@ #\:))
        (cond ((and (eq (car remaining) #\/)
                    (eq (second remaining) 'end))
                    (error "Errore path"))
            ((and (eq (car remaining) #\/)
                    (not (member (second remaining) (list #\# #\@ #\: #\/ #\?))))
                (multiple-value-bind
                    (parsedSubPath subRemaining)
                    (path (cdr remaining))
                    (values (concatenate 'string parsedPath '(#\/) parsedSubPath) subRemaining)
                ))
            ((and (eq (car remaining) #\/)
                  (member (second remaining) (list #\# #\@ #\: #\/ #\?)))
                    (error "Errore path")
            )
            (T (values (coerce parsedPath 'string) remaining))
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
                        (cond ((and (eq (car (second Path)) #\?) (member (second (second Path)) (list #\# 'end))) (error "Errore query"))  
                              (T (multiple-value-list
                                    (query (second Path))))))
                    (Fragment
                        (cond ((and (eq (car (second Query)) #\#) (eq (second (second Query)) 'end)) (error "Errore fragment"))
                            (T (multiple-value-list
                                        (fragment (second Query))
                                    )))
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
                (values (parse-integer (coerce parsedPort 'string)) remaining)
            )
          )
          (T (values 80 lista))
    )
)


(defun ipHelper (lista &optional final)
    (multiple-value-bind 
        (parsedTriplet remaining)
        (identificatore lista (list #\. #\: #\/ 'end))
            (cond ((or (> (parse-integer (coerce parsedTriplet 'string)) 255) (< (parse-integer (coerce parsedTriplet 'string)) 0)) (error "ip non valido"))
                ((and final (member (car remaining) (list #\. #\? #\@ #\#))) (error "errore fine ip"))
                ((and (not final) (not (eq (car remaining) #\.))) (error "errore punto ip")) 
                (T (values (coerce parsedTriplet 'string) remaining))
            )
    )
)

(defun ip (lista)
    (let* ((firstTriplet
                (multiple-value-list
                    (ipHelper lista)
                )  
           )
           (secondTriplet
                (multiple-value-list
                (ipHelper (cdr (second firstTriplet)))
                )
           )
           (thirdTriplet
                (multiple-value-list
                (ipHelper (cdr (second secondTriplet)))
                )
           )
           (fourthTriplet
                (multiple-value-list
                (ipHelper (cdr (second thirdTriplet)) T)
                )
           )
           )
            (values (append (first firstTriplet) '(#\.) (first secondTriplet)'(#\.) (first thirdTriplet) '(#\.) (first fourthTriplet)) (second fourthTriplet))
    )
)

(defun host (lista)
   (multiple-value-bind
    (parsedHost remaining)
    (identificatore lista (list #\. #\/ #\: 'end) (list #\? #\# #\@))
    (cond ((eq (car remaining) #\.)
            (multiple-value-bind
                (parsedSubHost subRemaining)
                (host (cdr remaining))
                (values (concatenate 'string parsedHost '(#\.) parsedSubHost) subRemaining)
            )
          )
          (T (values (coerce parsedHost 'string) remaining))
    )
   )
)

(defun hostHelper (lista)
    (handler-case (ip lista)
        (error ()
            (host lista)
        )
    )

)

(defun mailHost (lista)
    (multiple-value-bind
    (parsedHost remaining)
    (identificatore lista (list #\. 'end) (list #\? #\# #\@ #\/ #\:))
    (cond ((eq (car remaining) #\.)
            (multiple-value-bind 
                (parsedSubHost subRemaining)
                (host (cdr remaining))
                (values (append parsedHost '(#\.) parsedSubHost) subRemaining)
            )
          )
          (T (values parsedHost))
    )
   )
)


(defun userinfo (lista)
    (multiple-value-bind
        (parsedUserinfo remaining)
        (identificatore lista (list #\@) (list #\/ #\? #\# #\:))
        (cond ((null parsedUserinfo) (values nil remaining))
               (T (values (coerce parsedUserinfo 'string) (cdr remaining)))
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
                     (multiple-value-list
                        (hostHelper  (second Userinfo))
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
                (values (first Userinfo) (first Host) (first Port) (second Port))
            )
        )
        ((member (third lista) (list #\/ #\? #\# #\@ #\:)) (error "Uri non valido"))
        ((eq (car lista) #\/) (values nil nil nil lista))
        ((eq (car lista) 'end) (values nil nil nil 'end))
        (T (error "Uri non valido"))
    )
)


(defun helpuri (lista)
    (let* ((Authority
            (multiple-value-list
                (authority  lista)
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

(defun zosIDHelper (lista &optional listaDelimitatori listaBannati accumulatore)
    (cond   ((null lista) nil)
            ((member (car lista) listaBannati) nil)
            ((member (car lista) listaDelimitatori)
                (values (nreverse accumulatore)  lista))
            ((eq (car lista) #\SPACE) (error "Non sono ammessi spazi in id44"))
            ((not (alphanumericp (car lista))) (error "id44 deve essere formato da valori alfanumerici"))
            (T (zosIDHelper 
                 (cdr lista)
                 listaDelimitatori
                 listaBannati
                 (cons (car lista) accumulatore)
               )
            )
    )
)

(defun identificatoreID (lista &optional listaDelimitatori listaBannati)
    (let ((helperReturn 
            (multiple-value-list 
                (zosIDHelper lista listaDelimitatori listaBannati)
            )
          )
         )
        (if (first helperReturn) (values-list helperReturn) (values nil lista))
    )
)

(defun id44 (lista)
    (if (alpha-char-p (car lista)) 
        (multiple-value-bind
            (parsedID44 remaining)
            (identificatoreID lista (list #\. #\? #\# #\[ 'end))
            (cond ((eq (car remaining) #\.)
                (multiple-value-bind
                    (parsedSubID44 subRemaining)
                    (id44 (cdr remaining))
                    (values (append parsedID44 '(#\.) parsedSubID44) subRemaining)
                ))
                ((> (list-length parsedID44) 44) (error "ID44 troppo lungo"))
                (T (values parsedID44 remaining))
            )
        )
        (error "id44 deve iniziare con un carttere alfabetico")
    )
)

(defun id8 (lista)
    (if (alpha-char-p (car lista))
    (multiple-value-bind
        (parsedID8 remaining)
        (identificatoreID lista (list #\] 'end) (list #\. #\? #\# 'end))
        (if (> (list-length parsedID8 ) 8) (error "ID8 troppo lungo") (values parsedID8 remaining))
    )
    (error "id8 deve iniziare con un carattere afabetico"))
)

(defun zosPath (lista)
    (cond ((member (second lista) (list #\? #\#  #\[ 'end)) (error "id44 obbligatorio"))
            (T (let* ((ID44
                            (multiple-value-list
                                (id44 (cdr lista))
                            )
                      )
                      (ID8
                             (cond ((eq (car (second ID44)) #\[)
                                (multiple-value-list
                                    (id8 (cdr (second ID44)))))
                                (T (values nil (second ID44)))
                             )
                      )
            )
            (values (first id44) (first ID8) (cdr (second ID8)))
            ))
    ) 
)


(defun zos (lista)
     (let* ((Authority
                (multiple-value-list
                    (authority lista))
            )
            (Path
                (cond ((null (second Authority)) (error "Host obbligatorio in zos"))
                (T (multiple-value-list
                    (zosPath (fourth Authority)))))
            )
            (Query
                (cond ((and (eq (car (third Path)) #\?) (member (second (third Path)) (list #\# 'end))) (error "Errore query"))
                        (T (multiple-value-list
                            (query (third Path))))))
            (Fragment
                (cond ((and (eq (car (second Query)) #\#) (eq (second (second Query)) 'end)) (error "Errore fragment"))
                    (T (multiple-value-list
                        (fragment (second Query))
                                    )))
            )
            (zosReturn
                (values (append (list (first Authority))
                                        (list (second Authority))
                                        (list (third Authority))
                                        (append (list (first Path))
                                        (list (second Path)))
                                        (list (first Query))
                                        (list (first Fragment))
                                ))
            )
        )
        zosReturn
    )
)

(defun telfax (lista)
    (multiple-value-bind
        (parsedTelFax remaining)
        (identificatore (cdr lista) (list 'end) (list #\/ #\? #\# #\@ #\:))
        (cond ((not (member (first remaining) (list #\: 'end))) (error "telfax non valido"))
            (T (values (append (list parsedTelFax)
                        (list nil 80 nil nil nil))))))

)

(defun news (lista)
    (multiple-value-bind
        (parsedNews remaining)
        (host (cdr lista))
        (cond ((not (eq (first remaining) 'end)) (error "news non valido"))
            (T (values (append (list nil)
                        (list parsedNews)
                        (list 80 nil nil nil)))))))

(defun mailto (lista)
    (let* ((Userinfo
                (multiple-value-list
                    (identificatore lista (list #\@ 'end) (list #\/ #\? #\# #\:))
                ))
            (Host
                (cond ((and (null (first Userinfo))
                            (not (eq (first (second Userinfo)) 'end))) (error "mailto non valido"))
                        ((null (first Userinfo)) (values nil nil))
                    (T (multiple-value-list
                        (mailHost (cdr (second Userinfo)))
                        )
                    )
                )
            )
            (mailReturn
                    (append (list (first Userinfo))
                            (list (first Host))
                            (list 80 nil nil nil)
                    )
                )
            )
            mailReturn
    )
)

(defun scheme (lista)
    (multiple-value-bind
        (parsedScheme remaining)
        (identificatore lista (list #\:) (list #\/ #\? #\# #\@))
        (values (coerce parsedScheme 'string) (cdr remaining))
    )
)

(defun helpScheme (lista)
    (multiple-value-bind
        (parsedScheme remaining)
        (scheme lista)
        (cond ((null parsedScheme) (error "Uri non valido"))
              ((equal parsedScheme (list #\m #\a #\i #\l #\t #\o)) (values (append (list parsedScheme) (mailto remaining))))
              ((equal parsedScheme (list #\n #\e #\w #\s)) (values (append (list parsedScheme) (news remaining))))
              ((or (equal parsedScheme (list #\t #\e #\l)) (equal parsedScheme (list #\f #\a #\x))) (values (append (list parsedScheme) (telfax remaining))))
              ((equal parsedScheme (list #\z #\o #\s)) (values (append (list parsedScheme) (zos remaining))))
              (T (values (append (list parsedScheme) (helpuri remaining))))
        )
    )
)

(defun uri-parse (uriString)
    (handler-case (helpScheme (append (coerce uriString 'list) (list 'end)))
        (error (c)
        (values c));; ALLA FINE LEVALO
    )
)

;; GET SCHEME
(defun uri-scheme (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (first lista)))
)

;; GET USERINFO
(defun uri-userinfo (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (second lista)))
)

;; GET HOST
(defun uri-host (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (third lista)))
)

;; GET PORT
(defun uri-port (lista)
    (fourth lista)
)

;; GET PATH
(defun uri-path (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (fifth lista)))
)

;; GET QUERY
(defun uri-query (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (sixth lista)))
)

;; GET FRAGMENT
(defun uri-fragment (lista)
    (cond ((eq (length (first lista)) 0) nil)
        (T (seventh lista)))
)


(defun uri-display (lista &optional (stream t))
    (format stream 
    "Scheme: ~d~@
    Userinfo: ~d~@
    Host: ~d~@
    Port: ~d~@
    Path: ~d~@
    Query: ~d~@
    Fragment: ~d"
    (uri-scheme lista)
    (uri-userinfo lista)
    (uri-host lista)
    (uri-port lista)
    (uri-path lista)
    (uri-query lista)
    (uri-fragment lista)
    )
)