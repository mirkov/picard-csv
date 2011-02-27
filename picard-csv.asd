(asdf:defsystem picard-csv
    :name "picard-csv"
    :author "Alain Picard <apicard@optushome.com.au>
         (also alain.picard@memetrics.com)"
    :description "CSV parsing/writing utilities, a la Microsoft Excel"
    :components ((:file "csv-parser"))
    :depends-on (lisp-unit))
