;872491 - Pulcino Federico

; Sezione identificatori
(defun helper-identificatore 
 (lista &optional listaDelimitatori listaBannati accumulatore)
 (cond ((null lista) nil)
       ((member (car lista) listaBannati) nil)
       ((member (car lista) listaDelimitatori)
        (values (nreverse accumulatore)  lista)) 
       (T (helper-identificatore 
          (cdr lista)
          listaDelimitatori
          listaBannati
          (cons (car lista) accumulatore)))))

(defun identificatore 
 (lista &optional listaDelimitatori listaBannati)
 (let ((helperReturn 
        (multiple-value-list 
         (helper-identificatore lista listaDelimitatori listaBannati))))
      (if (first helperReturn) 
          (values-list helperReturn) 
          (values nil lista))))

(defun helper-id-port (lista &optional listaDelimitatori accumulatore)
 (cond ((null lista) nil)
       ((member (car lista) listaDelimitatori)
        (values (nreverse accumulatore) lista))
       ((null (digit-char-p (car lista))) (error "port solo numeri"))
       (T (helper-id-port (cdr lista)
           listaDelimitatori
           (cons (car lista) accumulatore)))))

(defun identificatore-port (lista &optional listaDelimitatori)
 (let ((helperReturn
        (multiple-value-list
         (helper-id-port lista listaDelimitatori))))
        (if (first helperReturn)
            (values-list helperReturn)
            (values 80 lista))))
; fine sezione identificatori

; Sezione Subdomain
(defun fragment (lista)
 (cond  ((eq (car lista) #\#)
         (multiple-value-bind
            (parsedFragment remaining)
            (identificatore (cdr lista) (list 'end))
            (values (coerce parsedFragment 'string) remaining)))
        (T  (values  nil lista))))

(defun query (lista)
 (cond ((eq (car lista) #\?)
        (multiple-value-bind
         (parsedQuery remaining)
         (identificatore (cdr lista) (list #\# 'end))
         (values (coerce parsedQuery 'string) remaining)))
       (T (values nil lista))))

(defun path (lista)
 (multiple-value-bind
  (parsedPath remaining)
  (identificatore lista (list #\/ #\? #\# 'end) (list #\@ #\:))
  (cond ((null parsedPath) (values nil remaining))
        ((and (eq (car remaining) #\/)
              (eq (second remaining) 'end))
              (error "Errore path"))
        ((and (eq (car remaining) #\/) 
              (eq (second remaining) 'end)) 
              (error "errore path"))
        ((and (eq (car remaining) #\/)
              (not (member (second remaining) (list #\# #\@ #\: #\/ #\?))))
         (multiple-value-bind
              (parsedSubPath subRemaining)
              (path (cdr remaining))
              (values (concatenate 'string parsedPath '(#\/) parsedSubPath) 
               subRemaining)))
        ((and (eq (car remaining) #\/)
              (member (second remaining) (list #\# #\@ #\: #\/ #\?)))
         (error "Errore path"))
        (T (values (coerce parsedPath 'string) remaining)))))

; Struttura subdomain
(defun subdomain (lista)
 (cond ((eq (car lista) #\/)
        (let* (
         (Path
          (multiple-value-list
            (path (cdr lista))))
         (Query
          (cond ((and (eq (car (second Path)) #\?) 
                      (member (second (second Path)) (list #\# 'end))) 
                      (error "Errore query"))  
                (T (multiple-value-list (query (second Path))))))
         (Fragment
          (cond ((and (eq (car (second Query)) #\#) 
                      (eq (second (second Query)) 'end)) 
                      (error "Errore fragment"))
                (T (multiple-value-list (fragment (second Query)))))))
         (cond ((eq (car (second Fragment)) 'end) 
                 (values (first Path) (first Query) (first Fragment)))
               (T (error "Errore subdomain")))))
       (T (values nil nil nil))))
; Fine sezione Subdomain

; Sezione Authority
(defun port (lista)
 (cond ((eq (car lista) #\:)
        (multiple-value-bind
         (parsedPort remaining)
         (identificatore-port (cdr lista) (list #\/ 'end))
         (values (parse-integer (coerce parsedPort 'string)) remaining)))
       (T (values 80 lista))))


(defun helper-ip (lista &optional final)
 (multiple-value-bind 
  (parsedTriplet remaining)
  (identificatore lista (list #\. #\: #\/ 'end))
  (cond ((or (> (parse-integer (coerce parsedTriplet 'string)) 255) 
             (< (parse-integer (coerce parsedTriplet 'string)) 0)) 
         (error "ip non valido"))
        ((and final (member (car remaining) (list #\. #\? #\@ #\#))) 
         (error "errore fine ip"))
        ((and (not final) (not (eq (car remaining) #\.))) 
         (error "errore punto ip")) 
        (T (values (coerce parsedTriplet 'string) remaining)))))

(defun ip (lista)
 (let* ((firstTriplet
         (multiple-value-list
          (helper-ip lista)))
        (secondTriplet
         (multiple-value-list
          (helper-ip (cdr (second firstTriplet)))))
        (thirdTriplet
         (multiple-value-list
          (helper-ip (cdr (second secondTriplet)))))
        (fourthTriplet
         (multiple-value-list
          (helper-ip (cdr (second thirdTriplet)) T))))
       (values (append (first firstTriplet) '(#\.) 
                       (first secondTriplet)'(#\.) 
                       (first thirdTriplet) '(#\.) 
                       (first fourthTriplet)) (second fourthTriplet))))

(defun host (lista)
 (multiple-value-bind
  (parsedHost remaining)
  (identificatore lista (list #\. #\/ #\: 'end) (list #\? #\# #\@))
  (cond ((and (eq (car remaining) #\.) (eq (second remaining) #\.)) 
         (error "errore host (due punti consecutivi)"))
        ((eq (length parsedHost) 0) (error "host obbligatorio"))
        ((eq (car remaining) #\.)
         (multiple-value-bind
          (parsedSubHost subRemaining)
          (host (cdr remaining))
          (cond ((eq (length parsedSubHost) 0) 
                 (error "subHost non deve essere nullo"))
                (T (values (concatenate 'string parsedHost '(#\.) parsedSubHost) 
                    subRemaining)))))
        (T (values (coerce parsedHost 'string) remaining)))))

(defun helper-host (lista)
 (handler-case (ip lista)
  (error ()
   (host lista))))


(defun userinfo (lista)
 (multiple-value-bind
  (parsedUserinfo remaining)
  (identificatore lista (list #\@) (list #\/ #\? #\# #\:))
  (cond ((null parsedUserinfo) (values nil remaining))
        (T (values (coerce parsedUserinfo 'string) (cdr remaining))))))



(defun authority (lista)
 (cond ((and
         (eq (first lista) #\/)
         (eq (second lista) #\/))
        (let* ((Userinfo
                (multiple-value-list
                 (userinfo (cdr (cdr lista)))))
               (Host
                (multiple-value-list
                 (helper-host  (second Userinfo))))
               (Port
                (cond ((eq (length (first Host)) 0) (error "Host obbligatorio"))
                      (T (multiple-value-list
                          (port (second Host)))))))
             (values (first Userinfo) (first Host) (first Port) (second Port))))
       ((member (third lista) (list #\/ #\? #\# #\@ #\:)) 
        (error "carattere non ammesso"))
       ((eq (car lista) #\/) (values nil nil 80 lista))
       ((eq (car lista) 'end) (values nil nil 80 nil nil nil 'end))
       (T (error "Uri non valido"))))
; Fine sezione Authority

; Struttura URI
(defun helper-uri (lista)
 (let* ((Authority
         (multiple-value-list
          (authority  lista)))
        (Subdomain
         (cond ((eq (fourth Authority) 'end) 
                (values nil nil nil))
               (T (multiple-value-list 
                   (subdomain (fourth Authority))))))
        (helpUriReturn
         (append (list (first Authority) 
                       (second Authority) 
                       (third Authority))
                          Subdomain)))
        helpUriReturn))
; Sezione Schemi speciali
; ZOS
(defun zos-id8-helper 
 (lista &optional listaDelimitatori listaBannati accumulatore)
 (cond ((null lista) nil)
       ((member (car lista) listaBannati) (error "carattere non valido id8"))
       ((member (car lista) listaDelimitatori)
        (values (nreverse accumulatore)  lista))
       ((eq (car lista) #\SPACE) (error "spazio non valido"))
       ((not (alphanumericp (car lista))) (error "id8 solo alphanumeric char"))
       (T (zos-id8-helper 
           (cdr lista)
           listaDelimitatori
           listaBannati
           (cons (car lista) accumulatore)))))

(defun identificatore-id8 (lista &optional listaDelimitatori listaBannati)
 (let ((helperReturn 
        (multiple-value-list 
         (zos-id8-helper lista listaDelimitatori listaBannati))))
      (if (first helperReturn) (values-list helperReturn) (values nil lista))))

(defun zos-id44-helper 
 (lista &optional listaDelimitatori listaBannati accumulatore)
 (cond ((null lista) nil)
       ((member (car lista) listaBannati) (error "carattere non valido id44"))
       ((member (car lista) listaDelimitatori)
        (values (nreverse accumulatore)  lista))
       ((eq (car lista) #\SPACE) (error "spazio non valido in id44"))
       ((and (not (alphanumericp (car lista))) (not(eq (car lista) #\.))) 
        (error "id44 solo alphanumeric char o ."))
       (T (zos-id44-helper 
           (cdr lista)
           listaDelimitatori
           listaBannati
           (cons (car lista) accumulatore)))))

(defun identificatore-id44 (lista &optional listaDelimitatori listaBannati)
 (let ((helperReturn 
        (multiple-value-list 
         (zos-id44-helper lista listaDelimitatori listaBannati))))
      (if (first helperReturn) (values-list helperReturn) (values nil lista))))

(defun id44 (lista)
 (if (alpha-char-p (car lista)) 
     (multiple-value-bind
      (parsedID44 remaining)
      (identificatore-id44 lista (list #\? #\# #\( 'end))
      (cond ((eq (car (last parsedID44)) #\.) 
             (error "ID44 non può terminare con ."))
            ((> (list-length parsedID44) 44) (error "ID44 troppo lungo"))
            (T (values (coerce parsedID44 'string) remaining))))
     (error "id44 deve iniziare con un carttere alfabetico")))


(defun id8 (lista)
 (if (alpha-char-p (car lista))
     (multiple-value-bind
      (parsedID8 remaining)
      (identificatore-id8 lista (list #\) 'end) (list #\. #\? #\# 'end))
      (if (> (list-length parsedID8 ) 8) 
          (error "ID8 troppo lungo") 
          (values (coerce parsedID8 'string) remaining)))
     (error "id8 deve iniziare con un carattere afabetico")))

(defun helper-id8 (lista)
 (cond ((eq (car lista) #\()
        (id8 (cdr lista)))
       (T  (values nil lista))))

(defun zos-path (lista)
 (cond ((member (second lista) (list #\? #\#  #\( 'end)) 
        (error "id44 obbligatorio"))
       (T (let* ((ID44
                  (multiple-value-list
                   (id44 (cdr lista))))
                 (ID8
                  (multiple-value-list
                   (helper-id8 (second ID44)))))
                (cond ((null (first ID8)) (values (first ID44)  (second ID8)))
                      (T (values (concatenate 'string (first ID44) 
                                  "(" (first ID8) ")") 
                                  (cdr (second ID8)))))))))
(defun zos (lista)
 (let* ((Authority
         (multiple-value-list
          (authority lista)))
        (Path
         (multiple-value-list
          (zos-path (fourth Authority))))
        (Query
         (cond ((and (eq (car (second Path)) #\?) 
                     (member (second (second Path)) (list #\# 'end))) 
                     (error "Errore query"))
               (T (multiple-value-list
                     (query (second Path))))))
        (Fragment
         (cond ((and (eq (car (second Query)) #\#) 
                     (eq (second (second Query)) 'end)) 
                     (error "Errore fragment"))
               (T (multiple-value-list
                    (fragment (second Query))))))
        (zosReturn
         (values (append (list (first Authority))
                         (list (second Authority))
                         (list (third Authority))
                         (list (first Path))
                         (list (first Query))
                         (list (first Fragment))))))
        zosReturn))


; Tel&fax
(defun telfax (lista)
 (multiple-value-bind
  (parsedTelFax remaining)
  (identificatore lista (list 'end) (list #\/ #\? #\# #\@ #\:))
  (cond ((not (member (first remaining) (list #\: 'end))) 
         (error "telfax non valido"))
        (T (values (list (coerce parsedTelFax 'string) nil 80 nil nil nil))))))

; News
(defun news (lista)
 (multiple-value-bind
  (parsedNews remaining)
  (handler-case (host lista)
   (error ()
   (values nil lista)))
  (cond 
   ((and (null parsedNews) (eq (first remaining) 'end)) 
    (values (list nil nil 80 nil nil nil)))
   ((not (eq (first remaining) 'end)) (error "news non valido"))
   (T (values (list nil (coerce parsedNews 'string) 80 nil nil nil))))))

; Mailto
(defun mail-host (lista)
 (multiple-value-bind
  (parsedHost remaining)
  (identificatore lista (list #\. 'end) (list #\? #\# #\@ #\/ #\:))
  (cond ((and (eq (car remaining) #\.) (eq (second remaining) #\.)) 
         (error "errore host"))
        ((null parsedHost) (error "host obbligatorio"))
        ((eq (car remaining) #\.)
         (multiple-value-bind 
          (parsedSubHost subRemaining)
          (host (cdr remaining))
          (values (concatenate 'string parsedHost '(#\.) parsedSubHost) 
           subRemaining)))
        (T (values (coerce parsedHost 'string))))))

(defun mail-userinfo (lista)
 (multiple-value-bind 
  (parsedMail remaining)
  (identificatore lista (list #\@ 'end) (list #\/ #\? #\# #\:))
  (cond ((eq (car lista) 'end) (values nil lista))
        (T (values (coerce parsedMail 'string) remaining)))))

(defun mailto (lista)
 (let* ((Userinfo
         (multiple-value-list
          (mail-userinfo lista)))
        (Host
         (cond ((and (eq (length (first Userinfo)) 0)
                     (not (eq (first (second Userinfo)) 'end))) 
                     (error "mailto non valido"))
               ((and (equal (first (second Userinfo)) #\@) 
                     (eq (second (second Userinfo)) 'end)) 
                     (error "mailto non valido"))
               ((equal (first (second Userinfo)) #\@)
                 (multiple-value-list
                  (mail-host  (cdr (second Userinfo)))))
               (T (values nil))))
        (mailReturn
         (append (list (first Userinfo))
                 (list (first Host))
                 (list 80 nil nil nil))))
        mailReturn))
; Fine schemi speciali

; Riconoscimento scheme
(defun scheme (lista)
 (multiple-value-bind
  (parsedScheme remaining)
  (identificatore lista (list #\:) (list #\/ #\? #\# #\@))
  (values (coerce parsedScheme 'string) (cdr remaining))))

(defun helper-scheme (lista)
 (multiple-value-bind
  (parsedScheme remaining)
  (scheme lista)
  (cond ((eq (length parsedScheme) 0) (error "Scheme obbligatorio"))
        ((string-equal parsedScheme "mailto") 
         (values (append (list parsedScheme) 
          (mailto remaining))))
        ((equal parsedScheme "news") 
         (values (append (list parsedScheme) (news remaining))))
        ((or (equal parsedScheme "tel") (equal parsedScheme "fax")) 
         (values (append (list parsedScheme) (telfax remaining))))
        ((equal parsedScheme "zos") 
         (values (append (list parsedScheme) (zos remaining))))
        (T (values (append (list parsedScheme) (helper-uri remaining)))))))
; fine riconoscimento scheme

; funzione iniziale uri-parse
(defun uri-parse (uriString)
 (handler-case (helper-scheme (append (coerce uriString 'list) (list 'end)))
  (error ()
   nil)))

; GETTER singoli elementi URI da struttura
; get scheme
(defun uri-scheme (lista)
 (cond ((eq (length (first lista)) 0) nil)
       (T (first lista))))

; get userinfo
(defun uri-userinfo (lista)
 (cond ((eq (length (second lista)) 0) nil)
       (T (second lista))))

; get host
(defun uri-host (lista)
 (cond ((eq (length (third lista)) 0) nil)
       (T (third lista))))

; get port
(defun uri-port (lista)
 (fourth lista))

; get path
(defun uri-path (lista)
 (cond ((eq (length (fifth lista)) 0) nil)
       (T (fifth lista))))

; get query
(defun uri-query (lista)
 (cond ((eq (length (sixth lista)) 0) nil)
       (T (sixth lista))))

; get fragment
(defun uri-fragment (lista)
 (cond ((eq (length (seventh lista)) 0) nil)
       (T (seventh lista))))
; fine Getters

; funzione uri-display
(defun uri-display (lista &optional (stream t))
 (format stream
  "~%Scheme: ~S~%Userinfo: ~S~%Host: ~S~%Port: ~D~%"
  (uri-scheme lista)
  (uri-userinfo lista)
  (uri-host lista)
  (uri-port lista))

 (format stream
  "Path: ~S~%Query: ~S~%Fragment: ~S"
  (uri-path lista)
  (uri-query lista)
  (uri-fragment lista)))