(in-package :deadlock)

;;;;;;;;;;;;;;;;;
;; money-holds ;;
;;;;;;;;;;;;;;;;;

(defgeneric hold-money (subaccount count)
  (:documentation "удержать нужную сумму денег со счета, возвращает удержание денег"))
(defgeneric withdraw-money (subaccount money)
  (:documentation "withdraw some money from subaccount"))
(defgeneric free-holded-money (money-hold))

;;;;;;;;;;;;;
;; hystory ;;
;;;;;;;;;;;;;

(defgeneric finalize-hystory (hyst))
(defgeneric back-step-candle (hystory candle period-type &optional steps))
(defgeneric make-candle-from-period (hystory datetime period))
(defgeneric back-step-existing-candle (hystory candle period-type &optional steps)
  (:documentation "вернет предыдущую суцествующую свечу, тобиш в которой проходили сделки(не было выходных проходили сделки и прочее"))
(defgeneric reduce-candle-values (hystory reduce-function map-function &key start end period))

;;;;;;;;;;;;;
;; candles ;;
;;;;;;;;;;;;;

(defgeneric period-to-seconds (per))
(defgeneric neg-period (per))
(defgeneric print-candle (candle))
(defgeneric datetime-add-period (datetime period &key times))
(defgeneric start-of-the-period (candle-or-datetime period))
(defgeneric end-of-the-period (candle-or-datetime period))

;;;;;;;;;;;
;; quick ;;
;;;;;;;;;;;

(defgeneric set-request (quick instrument direction count price &key subaccount overtime on-set on-execute on-overtime)
  (:documentation "выставляет заявку в quick"))