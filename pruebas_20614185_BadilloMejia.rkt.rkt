#lang racket

(load "tu_codigo.scm")
; Script de Pruebas de Funciones

;; Creación de opciones (op1, op2, op3)
(define op1 (option 1 "¡Viajar!" 2 1 "viajar" "turistear" "conocer"))
(define op2 (option 2 "¡Aprender!" 3 1 "estudiar" "aprender" "perfeccionarme"))
(define op3 (option 3 "¡Comer!" 4 2 "comer" "restaurantes"))

;; Muestra de opciones
(display "op1: ") (display op1) (newline)
(display "op2: ") (display op2) (newline)
(display "op3: ") (display op3) (newline)


;; Creación de flujos (f1, f2, f3)
(define f1 (flow 1 "¡Flujo de Viaje" op1 op2))
(define f2 (flow 2 "¡Flujo de Actividades" op3))
(define f3 (flow 3 "¡Flujo Vacío"))
;Flujos adicionales
(define f4 (flow 4 "Nuevo Flujo" op2 op3))
(define f5 (flow 5 "Otro Flujo" op1 op2 op3))
(define f6 (flow 6 "Flujo Adicional" op3 op1))

;; Muestra de flujos
(display "f1: ") (display f1) (newline)
(display "f2: ") (display f2) (newline)
(display "f3: ") (display f3) (newline)


;; Creación de chatbots (chatbot1, chatbot2, chatbot3)
(define chatbot1 (chatbot 1 "Asistente de Viaje" "¡Bienvenido a nuestro asistente de viaje! ¿En qué puedo ayudarte hoy?" 1 f1 f2))
(define chatbot2 (chatbot 2 "Asistente de Estudios" "¡Hola! Soy tu asistente de estudios. ¿En qué te gustaría aprender hoy?" 2 f1 f2))
(define chatbot3 (chatbot 3 "Asistente Gourmet" "¡Bienvenido al mundo de la gastronomía! ¿Qué te gustaría comer hoy?" 3 f3))
;*adicionales
(define chatbot4 (chatbot 4 "Asistente de Deportes" "¡Bienvenido al mundo del deporte! ¿En qué puedo ayudarte hoy?" 1 f2 f3))
(define chatbot5 (chatbot 5 "Asistente de Música" "¡Hola! Soy tu asistente musical. ¿Qué música te gustaría escuchar?" 2 f1 f2))

;; Muestra de chatbots
(display "chatbot1: ") (display chatbot1) (newline)
(display "chatbot2: ") (display chatbot2) (newline)
(display "chatbot3: ") (display chatbot3) (newline)



;; Modificación de chatbots
(define chatbot2 (chatbot-add-flow chatbot2 f4))
(define chatbot3 (chatbot-add-flow chatbot3 f5))
(define chatbot1 (chatbot-add-flow chatbot1 f6))

;; Muestra de chatbots modificados
(display "chatbot1 modificado (Ejemplo 1): ") (display chatbot1) (newline)
(display "chatbot2 modificado (Ejemplo 2): ") (display chatbot2) (newline)
(display "chatbot3 modificado (Ejemplo 3): ") (display chatbot3) (newline)

;;Creacion de sistemas
(define system1 (system "Sistema de Chatbots" 1 chatbot1 chatbot2 chatbot3))
(define system2 (system-add-chatbot system1 chatbot4))
(define system3 (system-add-chatbot system2 chatbot5))
