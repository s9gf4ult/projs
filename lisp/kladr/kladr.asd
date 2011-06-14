(asdf:defsystem kladr
  :depends-on (:sqlite :iterate)
  :components ((:file "packages")
               (:file "kladr" :depends-on ("packages"))))