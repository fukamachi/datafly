# datafly

[![Build Status](https://travis-ci.org/fukamachi/datafly.svg?branch=master)](https://travis-ci.org/fukamachi/datafly)

Datafly is a lightweight database library for Common Lisp.

## Quickstart

Datafly provides 3 functions which wrap [CL-DBI](https://github.com/fukamachi/cl-dbi) &mdash; `retrieve-one`, `retrieve-all` and `execute`.

They take a [SxQL](https://github.com/fukamachi/sxql) statement.

```common-lisp
(use-package :sxql)

(connect-toplevel :sqlite3 :database-name #P"myapp.db")

(retrieve-one
  (select :*
    (from :user)
    (where (:= :name "nitro_idiot"))))
;=> (:ID 1 :NAME "nitro_idiot" :EMAIL "nitro_idiot@example.com" :REGISTERED-AT "2014-04-14T19:20:13")

(retrieve-all
  (select :*
    (from :user)))
;=> ((:ID 1 :NAME "nitro_idiot" :EMAIL "nitro_idiot@example.com" :REGISTERED-AT "2014-04-14T19:20:13")
;    (:ID 2 :NAME "m2ym" :EMAIL "m2ym@example.com" :REGISTERED-AT "2014-04-15T01:03:42"))

(execute
 (insert-into :tweet
   (set= :id 1
         :user_id 1
         :body "Hi."
         :created_at (princ-to-string *now*))))
```

If you specify `:as` option with a class name to retrieval functions, they create instances of the class for each rows.

```common-lisp
(defstruct user
  id
  name
  email
  registered-at)

(retrieve-one
  (select :*
    (from :user)
    (where (:= :name "nitro_idiot")))
  :as 'user)
;=> #S(USER :ID 1 :NAME "nitro_idiot" :EMAIL "nitro_idiot@example.com" :REGISTERED-AT "2014-04-14T19:20:13")

(retrieve-all
  (select :*
    (from :user))
  :as 'user)
;=> (#S(USER :ID 1 :NAME "nitro_idiot" :EMAIL "nitro_idiot@example.com" :REGISTERED-AT "2014-04-14T19:20:13")
;    #S(USER :ID 2 :NAME "m2ym" :EMAIL "m2ym@example.com" :REGISTERED-AT "2014-04-15T01:03:42"))
```

The structure doesn't require having slots same as an existing table's in a database. It is acceptable even if the names are different. This might be convenient when you'd like to treat a JOINed table result as a structure object.

## Model Definitions

Datafly provides a macro `defmodel` which defines a flavored structure class.

```common-lisp
(defmodel (user (:inflate registered-at #'datetime-to-timestamp))
  id
  name
  email
  registered-at)


;; Same as the above.
(syntax:use-syntax :annot)

@model
(defstruct (user (:inflate registered-at #'datetime-to-timestamp))
  id
  name
  email
  registered-at)
```

`(:inflate <columns> <inflation-function>)` options mean `inflation-function` will be applied to each `<columns>` when creating an instance.

```common-lisp
(defvar *user*
  (retrieve-one
    (select :*
      (from :user)
      (where (:= :name "nitro_idiot")))
    :as 'user))

;; Returns a local-time:timestamp.
(user-registered-at *user*)
;=> @2014-04-15T04:20:13.000000+09:00
```

`defmodel` also allows `:has-a` and `:has-many` options.

```common-lisp
(use-package :sxql)

(defmodel (user (:inflate registered-at #'datetime-to-timestamp)
                (:has-a config (where (:= :user_id id)))
                (:has-many (tweets tweet)
                 (select :*
                   (from :tweet)
                   (where (:= :user_id id))
                   (order-by (:desc :created_at)))))
  id
  name
  email
  registered-at)

(defvar *user*
  (retrieve-one
    (select :*
      (from :user)
      (where (:= :name "nitro_idiot")))
    :as 'user))

(user-config *user*)
;=> #S(CONFIG :ID 4 :USER-ID 1 :TIMEZONE "JST" :COUNTRY "jp" :LANGUAGE "ja")

(user-tweets *user*)
;=> (#S(TWEET :ID 2 :USER-ID 1 :BODY "Is it working?" :CREATED-AT @2014-04-16T11:02:31.000000+09:00)
;    #S(TWEET :ID 1 :USER-ID 1 :BODY "Hi." :CREATED-AT @2014-04-15T18:58:20.000000+09:00))
```

## Provided inflation functions

* `tinyint-to-boolean`
* `datetime-to-timestamp`
* `unixtime-to-timestamp`
* `string-to-keyword`
* `octet-vector-to-string`

## Tips: Getting Association List or Hash Table for each rows

`retrieve-one` and `retrieve-all` return row(s) as a property list or a list of property lists by default.

If you'd like they were other types, for example "Association List" or "Hash Table", you can do it by passing `:as` parameter.

```common-lisp
(retrieve-one
  (select :*
    (from :user)
    (where (:= :name "nitro_idiot")))
  :as 'trivial-types:association-list)
;=> ((:ID . 1) (:NAME . "nitro_idiot") (:EMAIL . "nitro_idiot@example.com") (:REGISTERED-AT . "2014-04-14T19:20:13"))

(retrieve-one
  (select :*
    (from :user)
    (where (:= :name "nitro_idiot")))
  :as 'hash-table)
;=> #<HASH-TABLE :TEST EQL :COUNT 4 {1007AE3CD3}>
```

If no `:as` parameter is specified, `*default-row-type*` will be used.

```common-lisp
(let ((*default-row-type* 'hash-table))
  (retrieve-all
    (select :*
      (from :user))))
;=> (#<HASH-TABLE :TEST EQL :COUNT 4 {100815FA03}> #<HASH-TABLE :TEST EQL :COUNT 4 {100815FE43}>)
```

## See Also

* [SxQL](https://github.com/fukamachi/sxql)
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [cl-annot](https://github.com/arielnetworks/cl-annot), [CL-SYNTAX](https://github.com/m2ym/cl-syntax)
* [function-cache](https://github.com/AccelerationNet/function-cache)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.
