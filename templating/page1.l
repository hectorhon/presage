(:extends
 "layout.l"
 (:block content
   (ul ((class "sitemap")
        (id "asdf"))
       (div ((class "xxx")) "yyy")
       "zzz"
       (:for user :in *users*
             (li ()
                 (span () "here is ")
                 (a ((href "/link")) (:eval user)))))))
