(asdf:defsystem "presage"
  :serial t
  :components ((:file "packages")
               (:file "test-utils")
               (:file "string-utils")
               (:file "byte-utils")
               (:file "http/parser")
               (:file "http/server")))
