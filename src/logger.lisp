(in-package :cl-user)
(defpackage datafly.logger
  (:use :cl))
(in-package datafly.logger)

(syntax:use-syntax :annot)

@export
(defvar *trace-sql* nil)

(defparameter *sql-logger-pattern*
  "[%D{%H:%M:%S}] <DB> %m%n")

@export
(defvar *sql-logger*
  (let ((logger (log:category '(datafly))))
    (log:config logger :own :trace)
    (log4cl:remove-all-appenders logger)
    (log4cl:add-appender logger
                         (make-instance 'log4cl:tricky-console-appender
                                        :layout (make-instance 'log4cl:pattern-layout
                                                               :conversion-pattern *sql-logger-pattern*)))
    logger))
