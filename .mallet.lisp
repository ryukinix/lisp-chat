(:mallet-config
  (:extends :default)  ; or :all

  ;; Enable rules with options
  (:enable :line-length :max 120)
  (:enable :consecutive-blank-lines :max 2)
  ;; Disable specific rules
  (:disable :double-colon-access))
