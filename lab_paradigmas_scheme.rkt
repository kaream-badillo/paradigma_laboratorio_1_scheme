#lang racket
;;LAB PARADIGMAS
;-----------------------------------
;MIS HERRAMIENTAS
(define (d-mio lista mensaje)
  (display mensaje)
  (newline)
  (display lista)
  (newline))
;esto esta de más
(define (agregar-elemento-inicio elemento lista)
  (cons elemento lista))
(define (agregar-elemento-final elemento lista)
  (append lista elemento))

;f(x) si se repite en una lista
(define (repite-en-lista numero lista)
  (cond
    [(null? lista) (append lista numero)]
    [(= numero (car lista)) "ya existe id"]
    [else (cons (car lista) (repite-en-lista numero (cdr lista)))]))

;funcion elegir posicion en una lista
(define (posicion-lista posicion lista)
  (define (posicion-lista-aux posicion lista contador)
    (cond
      [(null? lista) "no existe posicion"]
      [(= posicion contador) (car lista)]
      [else (posicion-lista-aux posicion (cdr lista) (+ 1 contador))]))
  (posicion-lista-aux posicion lista 0))

;funcion para sumar una posision de todas las listas dentro de una lista
(define (sumar-all-posiciones posicion lista)
  (apply + (map (posicion-lista posicion lista))))

;-----------------------------------



(define lista-id-options (list))
;;TDA Option - constructor
;Nombre función: option
;Dominio: code (Int)  X message (String)  X ChatbotCodeLink (Int) X InitialFlowCodeLink (Int) X Keyword* (en referencia a 0 o más palabras claves)
;Recorrido: option(lista)
(define (option code message ChatbotCodeLink InitialFlowCodeLink . keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink keyword))

(display "(option code message ChatbotCodeLink InitialFlowCodeLink . keyword)")
(newline)
(define op1 (option 1 "m1" 0 0))
(define op2 (option 2 "m2" 0 0 "k2"))
(define op3 (option 3 "m3" 0 0 "k2" "k3"))
(display "op1") op1
(display "op2") op2
(display "op3") op3

(newline)
(define lista-id-flows (list))
;TDA Flow - constructor
;Nombre función: flow
;Dominio: id (int) X name (String)  X Option*  (Indica que puede recibir cero o más opciones)
;Recorrido: flow

;creando un nuevo flow
;(define f10 (flow 1 “Flujo1”))
;alternativamente podría usarse:
;(define f12 (flow 1 “Flujo1” op1 op2))
;(define (flow id nombre-flujo keyword))

(define (verificacion-duplicados-agregar lista-options)
  (cond
    [(null? lista-options) '()] 
    [else (cons (car lista-options)
                (verificacion-duplicados-agregar (filter (lambda(x) (not (equal? (car lista-options) x))) (cdr lista-options))))]))

(define (flow id name . options)
  (list id name (verificacion-duplicados-agregar options)))

(display "(flow id name . options)")
(newline)
(define f10 (flow 0 "Flujo1"))
(define f12 (flow 2 "Flujo1" op1))
(display "f10") f10
(display "f12") f12

(newline)
;Nombre función: flow-add-option
;La función también verifica que las opciones añadidas no se repitan en base al id de éstos.
;Dominio: flow X option
;Recorrido: flow

;Ejemplo de uso
;añadiendo opciones 1 y 2 al flujo f10
;el resultado alcanzado en f12 es equivalente al ilustrado en f12 de la función 3.
;(define f11 (flow-add-option f10 op1))
;(define f12 (flow-add-option f11 op2))



(define (agregar-con-verificacion option lista)
  (if (not (member option lista)) (cons option lista)
      lista))


(define (flow-add-option flow-n . option-n) 
  (list (car flow-n) 
        (cadr flow-n) 
        (verificacion-duplicados-agregar option-n)))

(display "(flow-add-option flow-n . option-n)")
(newline)
(define f11 (flow-add-option f12 op2))
(display "f11") f11
(define f13 (flow-add-option f12 op1 op2))
(display "f13") f13

(newline)
;Nombre función: chatbot
;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows* (indicando que puede recibir 0 o más flujos)
;Recorrido: chatbot

;Ejemplo de uso
;creando un nuevo chatbot
;(define cb10 (chatbot 0 “Asistente” “Bienvenido\n¿Qué te gustaría hacer?”) 1)
;alternativamente podría usarse:
;(define cb11  (chatbot 0 “Asistente” “Bienvenido\n¿Qué te gustaría hacer?”  1 f12))
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (verificacion-duplicados-agregar flows)))

(display "(chatbot chatbotID name welcomeMessage startFlowId . flows)")
(newline)
(define cb10 (chatbot 0 "name10" "mss" 1))
(define cb11  (chatbot 1 "name11" "mss"  1 f10 f11 f10))
(display "cb10") cb10
(display "cb11") cb11

(newline)
;TDA chatbot - modificador. Función modificadora para añadir flujos a un chatbot.
;Nombre función: chatbot-add-flow
;*Usar recursión de cola o natural para añadir flujos al final de la lista de flujos.
;Dominio: chatbot X flow
;Recorrido: chatbot

;Ejemplo de uso
;añadiendo flujo a un chatbot
;el resultado alcanzado en cb11 es equivalente al ilustrado en cb11 de la función 5.
;(define cb11 (chatbot-add-flow cb10 f12))
(define (chatbot-add-flow chatbot . flows)
  (list (car chatbot) (cadr chatbot) (caddr chatbot) (cadddr chatbot) (verificacion-duplicados-agregar flows)))


(display "(chatbot-add-flow chatbot flow)")
(newline)
(define cb12 (chatbot-add-flow cb11 f11))
(display "cb12") cb12

(newline)
;TDA system - constructor. Función constructora de un sistema de chatbots. Deja registro de la fecha de creación.
;Nombre función: system
;*El sistema además de contener los distintos chatbots, también contiene el chatHistory de cada usuario que interactúa con el sistema. Sobre el chatHistory, éste corresponde al registro completo del diálogo entre usuario y cada uno de los chatbots con los que interactúa. El historial se mantiene para cada usuario y debe tener el String formateado de cada mensaje del usuario y chatbot (para luego ser visualizado con la función display), fecha, hora y emisor (usuario o sistema).
;Dominio: name (string) X InitialChatbotCodeLink (Int) X chatbot* (indicando que pueden recibir 0 o más chatbots)
;Recorrido: system

;Ejemplo de uso
;creando la un nuevo sistema de chatbots con nombre “NewSystem”
;(define s0 (system “NewSystem” 0))
;alternativamente podría usarse:
;(define s1 (system “NewSystem” cb11))

(define (system name InitialChatbotCodeLink . chatbot)
  (list (current-seconds) name InitialChatbotCodeLink chatbot))

(display "(system name InitialChatbotCodeLink . chatbot)")
(newline)
(define s0 (system "NewSystem" 0 cb10 cb11 cb12 cb10 cb11 cb12))
(display "s0") s0
(define s1 (system "NewSystem" cb10))
(display "s1") s1

(newline)
;TDA system - modificador: Función modificadora para añadir chatbots a un sistema.
;Nombre función: system-add-chatbot
;No usar recursividad
;Debe verificar que el chatbot no exista en el sistema a partir del id de éste.
;Dominio: system X chatbot
;Recorrido: system

;Ejemplo de uso
;añadiendo un chatbot al sistema.
;el resultado alcanzado en e1 es equivalente al ilustrado en s1 de la función 7.
;(define s1 (system-add-chatbot s0 cb11))

(define (get-id-cb lista-cb)
  (car lista-cb))


(define (get-it-cb-system system)
  (map get-id-cb (cadddr system)))

(define s2 (get-it-cb-system s0))
(display "s2") s2




;(define (system-add-chatbot system chatbot)
 ; (cond
  ;  [if (filter (= (car chatbot) (get-it-cb-system system))) #f]
   ; [cons chatbot (cadddr system)]))
(define op11 (option  1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
(define op22 (option  2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define f1010 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
(define f1111 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada
(define cb00 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  ;solo añade una ocurrencia de f10
op11
op22
f1010
f1111
cb00