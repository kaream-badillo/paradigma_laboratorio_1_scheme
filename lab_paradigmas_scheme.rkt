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

;f(x) si sse repite en una lista
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
;Dominio: code (Int)  X message (String)  X ChatbotCodeLink (Int) X FlowCodeLink (Int) X Keyword* (en referencia a 0 o más palabras claves)
;Recorrido: option(lista)
(define (option code message ChatbotCodeLink InitialFlowCodeLink . keyword)
  (list code message ChatbotCodeLink InitialFlowCodeLink keyword))
(define op1 (option 0 "hola1" 0 1))
(define op2 (option 0 "hola2" 0 2 "key1"))
(define op3 (option 0 "hola3" 0 3 "key1" "key2"))
(display "op1")
op1
(display "op2")
op2
(display "op3")
op3


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
(define (flow id name . options)
  (list  (+ (+ id (apply + (map cadddr options))) 1) name options)
  ;(repite-en-lista (+ (+ id (apply + (map cadddr options))) 1) lista-id-flows)
  )
(define f10 (flow 1 "Flujo1"))
(define f12 (flow 1 "Flujo1" op1 op2))
(display "f10")
f10
(display "f12") f12

(define (flow2 id name . options)
  (list  (+ (+ id (sumar-all-posiciones 4 options)) 1) name options)
         (repite-en-lista (+ (+ id (sumar-all-posiciones 4 options)) 1) lista-id-flows))



;Nombre función: flow-add-option
;La función también verifica que las opciones añadidas no se repitan en base al id de éstos.
;Dominio: flow X option
;Recorrido: flow

;Ejemplo de uso
;añadiendo opciones 1 y 2 al flujo f10
;el resultado alcanzado en f12 es equivalente al ilustrado en f12 de la función 3.
;(define f11 (flow-add-option f10 op1))
;(define f12 (flow-add-option f11 op2))
(define (flow-add-option flow-n . option-n)  
  (list (+ (car flow-n) 1)  (cadr flow-n) (append (caddr flow-n) option-n)))
(define f11 (flow-add-option f10 op1))
(display "f11") f11
(define f13 (flow-add-option f12 op1 op2 op3))
(display "f13")
f13


;Nombre función: chatbot
;Dominio: chatbotID (int) X name (String) X welcomeMessage (String) X startFlowId(int) X  flows* (indicando que puede recibir 0 o más flujos)
;Recorrido: chatbot

;Ejemplo de uso
;creando un nuevo chatbot
;(define cb10 (chatbot 0 “Asistente” “Bienvenido\n¿Qué te gustaría hacer?”) 1)
;alternativamente podría usarse:
;(define cb11  (chatbot 0 “Asistente” “Bienvenido\n¿Qué te gustaría hacer?”  1 f12))
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list (+ 1 (+ chatbotID (apply + (map car flows)))) name welcomeMessage startFlowId flows))
(define cb10 (chatbot 0 "Asistente" "Bienvenido\n¿Qué te gustaría hacer?" 1))
(define cb11  (chatbot 0 "Asistente" "Bienvenido\n¿Qué te gustaría hacer?"  1 f10))
(display "cb10") cb10
(display "cb11") cb11


;TDA chatbot - modificador. Función modificadora para añadir flujos a un chatbot.
;Nombre función: chatbot-add-flow
;*Usar recursión de cola o natural para añadir flujos al final de la lista de flujos.
;Dominio: chatbot X flow
;Recorrido: chatbot

;Ejemplo de uso
;añadiendo flujo a un chatbot
;el resultado alcanzado en cb11 es equivalente al ilustrado en cb11 de la función 5.
;(define cb11 (chatbot-add-flow cb10 f12))
(define (chatbot-add-flow chatbot flow)
  (list (+ 1 (car chatbot)) (cadr chatbot) (caddr chatbot) (cadddr chatbot) (cons flow (car (reverse chatbot)))))
(define cb12 (chatbot-add-flow cb10 f12))
(display "cb12") cb12


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

