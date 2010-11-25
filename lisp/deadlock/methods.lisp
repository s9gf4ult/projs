(in-package :deadlock)

(defmethod hold-money ((subaccount subaccount) (count number))
  (make-instance 'money-hold :subaccount subaccount :money count))

(defmethod withdraw-money ((subaccount subaccount) (money number))
  (with-slots (free-money) subaccount
    (if (<= money free-money)
        (setf free-money (- free-money money))
        (error (make-condition 'not-enought-money :need-money money :got-money free-money :format-control "there is not enought money to withdraw")))))

(defmethod free-holded-money ((hold money-hold))
  (with-slots (subaccount money) hold
    (with-slots (free-money) subaccount
      (incf free-money money)
      (setf money 0
            subaccount nil))))