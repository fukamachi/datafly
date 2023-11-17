(uiop:define-package datafly.logger
  (:use :cl
        :datafly.syntax))
(in-package datafly.logger)

(enable-syntax)

@export
(defvar *trace-sql* nil)

(defparameter *sql-logger-pattern*
  "[%D{%H:%M:%S}] <DB> %m%n")

@export
(defvar *sql-logger*
  (let ((logger (log:category '(datafly))))
    (log:config logger :own :trace)
    (log4cl:add-appender logger
                         (make-instance 'log4cl:this-console-appender
                                        :layout (make-instance 'log4cl:pattern-layout
                                                               :conversion-pattern *sql-logger-pattern*)))
    logger))
