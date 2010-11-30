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
(defgeneric make-candle (hystory start end priod-type))
(defgeneric back-step-existing-candle (hystory candle period-type &optional steps)
  (:documentation "вернет предыдущую суцествующую свечу, тобиш в которой проходили сделки(не было выходных проходили сделки и прочее"))
(defgeneric reduce-candle-values (hystory reduce-function map-function &key start end period))
(defgeneric hystory-go-next (hystory))
(defgeneric hystory-get-current-candle (hystory &optional period))

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
(defgeneric execute-request (quick request)
  (:documentation "return t if executed, nil otherwise. execution of request generates new deal, deal being commited in opened position, or position opening before it"))
(defgeneric open-deal-in-position (quick subaccount pos direction  count price &optional commission)
  (:documentation "creates new deal in opened position, does not check if position has deals with other instruments, if `open-position-count' becomes 0 then automatically finalize and log position"))
(defgeneric open-new-position (quick subaccount instrument direction count price &optional commission)
  (:documentation "opens new position and creates deal in it"))
(defgeneric check-overtime-request (quick request)
  (:documentation "returns t if request is overtimed and execute callback in it if set"))
(defgeneric quick-log-and-finalize-request (quick request))
(defgeneric quick-log-and-finalize-position (quick position))
(defgeneric quick-log-request (quick request))
(defgeneric quick-finalize-request (quick request))

;;;;;;;;;;;;;;;
;; quick-log ;;
;;;;;;;;;;;;;;;

(defgeneric finalize-quick-log (quick-log))

;;;;;;;;;;;;;
;; request ;;
;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;; open-positions ;;
;;;;;;;;;;;;;;;;;;;;

