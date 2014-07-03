#|
  This file is a part of datafly project.
  Copyright (c) 2014 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage datafly
  (:use :cl)
  (:import-from :datafly.db
                :*connection*
                :*trace-sql*
                :*default-row-type*
                :connect-cached
                :connect-toplevel
                :disconnect-toplevel
                :retrieve-one
                :retrieve-one-value
                :retrieve-all
                :execute)
  (:import-from :datafly.model
                :model
                :defmodel
                :object-to-plist
                :copy-model)
  (:import-from :datafly.cache
                :clear-model-caches
                :clear-object-caches)
  (:import-from :datafly.inflate
                :tinyint-to-boolean
                :datetime-to-timestamp
                :unixtime-to-timestamp
                :string-to-keyword
                :octet-vector-to-string)
  (:import-from :datafly.json
                :encode-json)
  (:export :*connection*
           :*trace-sql*
           :*default-row-type*
           :connect-cached
           :connect-toplevel
           :disconnect-toplevel
           :retrieve-one
           :retrieve-one-value
           :retrieve-all
           :execute

           :model
           :defmodel
           :object-to-plist
           :copy-model

           :clear-model-caches
           :clear-object-caches

           :tinyint-to-boolean
           :datetime-to-timestamp
           :unixtime-to-timestamp
           :string-to-keyword

           :encode-json))
(in-package :datafly)
