(asdf:defsystem kladr
  :depends-on (:sqlite :iterate :cl-gtk2-gtk :uuid)
  :components ((:file "packages")
               (:file "sqlite")
               (:file "kladr" :depends-on ("packages" "sqlite"))))