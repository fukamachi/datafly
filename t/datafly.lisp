(in-package :cl-user)
(defpackage datafly-test
  (:use :cl
        :datafly
        :cl-test-more
        :sxql))
(in-package :datafly-test)

(plan 16)

(defparameter *db-path*
  (asdf:system-relative-pathname :datafly #P"t/test.db"))

(defparameter *now* (local-time:now))

(uiop:delete-file-if-exists *db-path*)

(defmacro with-connection (&body body)
  `(let ((*connection* (connect-cached :sqlite3 :database-name *db-path*)))
     ,@body))

(with-connection
  (execute
   (create-table :user
       ((id :type :bigint
            :primary-key t)
        (name :type '(:varchar 32))
        (email :type '(:varchar 255))
        (status :type '(:varchar 20))
        (created_at :type :datetime)
        (updated_at :type :datetime))))
  (execute
   (create-table :tweet
       ((id :type :bigint
            :primary-key t)
        (user_id :type :bigint)
        (body :type :text)
        (created_at :type :datetime))))
  (execute
   (insert-into :user
     (set= :id 1
           :name "nitro_idiot"
           :email "e.arrows@gmail.com"
           :status "normal"
           :created_at (princ-to-string *now*))))
  (execute
   (insert-into :tweet
     (set= :id 1
           :user_id 1
           :body "Hi."
           :created_at (princ-to-string *now*))))
  (execute
   (insert-into :tweet
     (set= :id 2
           :user_id 1
           :body "Is it working?"
           :created_at (princ-to-string *now*)))))

(with-connection
  (let ((user
          (retrieve-one
           (select :*
             (from :user)
             (where (:= :name "nitro_idiot"))))))
    (is user `(:id 1 :name "nitro_idiot" :email "e.arrows@gmail.com" :status "normal" :created-at ,(princ-to-string *now*) :updated-at nil))))

(syntax:use-syntax :annot)

@model
(defstruct (user (:inflate (created-at updated-at) #'datetime-to-timestamp)
                 (:inflate status #'string-to-keyword)
                 (:has-many (tweets tweet)
                  (select :*
                    (from :tweet)

                    (where (:= :user_id id))
                    (order-by (:desc :created_at) (:desc :id)))))
  id
  name
  email
  status
  created-at
  updated-at)

@model
(defstruct (tweet (:inflate created-at #'datetime-to-timestamp))
  id
  user-id
  body
  created-at)

(with-connection
  (let ((user
          (retrieve-one :user
                        :by :name := "nitro_idiot"
                        :as 'user)))
    (is-type (user-created-at user) 'local-time:timestamp)
    (is (user-updated-at user) nil)
    (is (user-status user) :normal)

    (is (length (user-tweets user)) 2)
    (is (tweet-id (car (user-tweets user :limit 1))) 2)
    (is (tweet-id (car (user-tweets user :limit 1 :offset 1))) 1)
    (is (length (user-tweets user :limit 2)) 2)
    (ok (every (lambda (tweet)
                 (typep tweet 'tweet))
               (user-tweets user)))

    (execute
     (insert-into :tweet
       (set= :id 3
             :user_id 1
             :body "Looks good."
             :created_at (princ-to-string *now*))))

    (is (length (user-tweets user)) 2
        "Result cached")

    (let ((test-val '#:test-val))
      (setf (user-tweets user) test-val)
      (is (user-tweets user) test-val :test #'eq
          "Set cache value"))

    (datafly.cache:clear-object-caches user)

    (is (length (user-tweets user)) 3)))

(with-connection
  (let ((user (retrieve-one :user
                            :by :name := "nitro_idiot")))
    (is-type user 'trivial-types:property-list))
  (let ((user (retrieve-one :user
                            :by :name := "nitro_idiot"
                            :as 'hash-table)))
    (is-type user 'hash-table))
  (let* ((*default-row-type* 'hash-table)
         (user (retrieve-one :user
                             :by :name := "nitro_idiot")))
    (is-type user 'hash-table))
  (let* ((*default-row-type* 'hash-table)
         (user (retrieve-one :user
                             :by :name := "nitro_idiot"
                             :as 'trivial-types:association-list)))
    (is-type user 'trivial-types:association-list)))

(finalize)
