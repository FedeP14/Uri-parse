# Progetto URI parse

## Introduzione
Il progetto consiste nel creare una libreria che costruisca una
struttura rappresentate un URI _(Uniform resource identifier)_, attraverso un
processo di analisi sintattica **(Parsing)** a partire da una grammatica
definita nel testo del progetto e/o dagli standard RFC.
In caso che l'URI rispetti la grammatica data identifica i seguenti elementi:

- Scheme
- Userinfo
- Host
- Port
- Path
- Query
- Fragment
<p></p>
Il progetto è stato scritto nei seguenti linguaggi:  <br/>
<p>

![](https://i.imgur.com/KwQjlTM.png)
![](https://imgur.com/btQMc3v.png)

</p>

1. [Lisp](#lisp)
2. [Prolog](#prolog)

---
# Lisp

## Tabella dei contentuti

1. [Funzionamento](#funzionamento-lisp)
2. [Scelte di realizzazione](#scelte-di-realizzazione-lisp)



## Funzionamento Lisp
 
 la funzione principale "_uri-parse_" riceve in input una stringa e richiama la
 funzione "_helper-scheme_" che a sua volta chiamerà la funzione che identifica
 **scheme** e, in base al valore di ritorno di quest'ultimo richiamerà 
 le funzioni per il parsing del tipo URI1, di uno schema speciale o se null 
 darà errore; per esempio nel casp di un URI di tipo URI1 verrà chiamata una 
 funzione che chiamera il parsing dei singoli elementi da cui è formato e li 
 restituirà in una lista con gli elementi in ordine come descritti 
 nella sezione [introduzione](#introduzione) (in caso di valore mancate 
 al suo posto ritornerà _nil_)
 Nonostante _uri-parse_ sia la funzione principale un'importante menzione da fare riguarda la funzione identificatore, che fa comprendere l'approcio al progetto, nel caso di lisp è carattere per carattere, definendo in particolare
 caratteri non ammessi e caratteri delimitatori, i primi con il solo obiettivo di rispettare la grammatica data, mentre i secondi sono serviti per suddividere la stringa nei suoi elementi singoli.

---


 ### Scelte di realizzazione Lisp

 - le funzioni con helper nel nome sono considerate funzioni ausiliarie
 - viene inteso come "subdomain" l'unione di path query e fragment
   _(nonostante non sia proprio il termine corretto)_
 - path query e fragment vengono "riuniti" poiché mi sembrava più 
   coerente con la grammatica specificata nel progetto (ma vengono poi 
   controllati e parsati singolarmente)
 - le stringe sono considerate nel programma come liste di caratteri in modo 
   tale che sia possibile riutilizzare il rimanente della stringa totale dopo 
   aver parsato un singolo elemento senza ricorrere all'utilizzo di 
   sottostringhe
 - per una migliora leggibilità del codice (dato che Lisp "costringe" a
   scrivere le funzioni di alto livello dopo le funzioni di basso livello) ho
   cercato, anche solo tramite i commenti, di dividere il codice in sezioni

---
---

# Prolog

## Tabella dei contentuti

1. [Scelte di implementazione](#scelte-di-implementazione-prolog)
2. [Funzionamento](#funzionamento-prolog)
3. [Scelte di realizzazione](#scelte-di-realizzazione-prolog)


### Scelte di implementazione Prolog

l'implementazione del progetto avviene attraverso le Prolog
_definite clause grammar (DCG)_ poichè risultate ad hoc
per raggiungere il fine del progetto

## Funzionamento Prolog

La funzione principale è _uri_parse_/2 che prende in input una stringa e la
ritorna scomposta nelle parti precedentemente citate
(se un componente non è nella stringa allora restituisce "[]".
Un esempio potrebbe essere:

> uri_parse("http://google.it", X).
>
> X = uri(http, [], 'google.com', '80', [], [], []).

---

### Scelte di realizzazione Prolog

- le DCG con helper nel nome sono considerate funzioni ausiliarie
- viene inteso come "subdomain" l'unione di path query e fragment
  _(nonostante non sia proprio il termine corretto)_
- path query e fragment vengono "riuniti" poiché mi sembrava più
  coerente con la grammatica specificata nel progetto (ma vengono poi
  controllati e parsati singolarmente)
- le stringe sono considerate nel programma come liste di caratteri in modo
  tale che sia possibile riutilizzare il rimanente della stringa totale dopo
  aver parsato un singolo elemento senza ricorrere all'utilizzo di
  sottostringhe

