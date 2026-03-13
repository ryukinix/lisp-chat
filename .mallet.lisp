(:mallet-config
  (:extends :default)  ; or :all

  ;; Ignore files/directories (uses glob patterns)
  (:ignore "**/example.lisp"         ; ignore at any level
           "**/*-generated.lisp"     ; ignore generated files
           "vendor/**/*.lisp")       ; ignore vendor directory

  ;; Enable rules with options
  (:enable :line-length :max 120)
  (:enable :consecutive-blank-lines :max 2)

  ;; Enable a rule with a custom severity override (wins over :set-severity)
  (:enable :cyclomatic-complexity :severity :warning)

  ;; Disable specific rules
  (:disable :double-colon-access)

  ;; Override severity for all rules in a category.
  ;; Per-rule :severity (above) takes precedence over :set-severity.
  (:set-severity :metrics :info))
