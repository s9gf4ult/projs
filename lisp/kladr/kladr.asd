(asdf:defsystem kladr
  :depends-on (:sqlite :iterate)
  :components ((:file "packages")
               (:file "sqlite")
               (:file "kladr" :depends-on ("packages" "sqlite"))))