(ns actus.common-db-sql

  (:use korma.db)
  (:use korma.core)

  (:require [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc])

  )

;; COMMON FUNCTIONS ----------------------------------------------------

(defn common-save-for-field
  "Сохранить сущность"
  [entity field vals]
  (let [r (update entity (set-fields vals) (where (= field (vals field))))]
    (if (nil? r)
      (insert entity (values vals))
      r)) )

(defn common-save-for-id
  "Сохранить сущность"
  [entity vals]
  (let [r (update entity (set-fields vals) (where (= :id (vals :id))))]
    (if (nil? r)
      (insert entity (values vals))
      r)) )

(defn common-delete*
  "Сохранить сущность"
  [entity]
  (delete* entity))

(defn common-delete-for-id
  "Сохранить сущность"
  [entity id]
  (delete entity (where (= :id id))))

(defn common-find
  "Найти сущность по :id"
  [entity id]
  (first (select entity (where (= :id id)))) )

(defn common-count
  "Количество всех элементов"
  [entity]
  (-> (select entity (aggregate (count :*) :c)) first :c)  )

(defn common-exec
  "Выполнить запрос"
  [query]
  (exec query))


;;**************************************************************************************************
;;* BEGIN Entity fields transformation
;;* tag: <entity field transfotmation>
;;*
;;* description: Функционал трансформации полей
;;*
;;**************************************************************************************************

(defn common-fields-transformation
  " (common-transformator-for-fields [:cdate :udate] (fn [value] ......))"
  [field-keys fn-transformation vals]
  (reduce
   (fn [vals field-key]
     (let [v (field-key vals)]
       (if (nil? v) vals
           (assoc vals field-key (fn-transformation v)))))
   vals field-keys))


(defn fields-transformation-clj-keyword->-string [field-keys vals]
  (common-fields-transformation field-keys name vals))

(defn fields-transformation-clj-keyword-<-string [field-keys vals]
  (common-fields-transformation field-keys keyword vals))

(defn fields-transformation-clj-time->-sql-time [field-keys vals]
  (common-fields-transformation field-keys
                                #(tc/to-sql-time (tco/to-time-zone % (tco/default-time-zone)))
                                vals))

(defn fields-transformation-clj-time-<-sql-time [field-keys vals]
  (common-fields-transformation field-keys
                                #(tco/to-time-zone (tc/from-sql-time %) (tco/default-time-zone))
                                vals))

;; END Entity fields transformation
;;..................................................................................................



;; PAGES AND SORTING --------------------------------------------------
(defn common-page
  "Установть страницу и ее размер"
  [query page size]
  (-> query (limit size) (offset (* page size))) )

(defn common-sort-by
  "Сортировать..."
  [query order-by asc-desc]
  (-> query (order order-by asc-desc)) )

(defn common-filter-by=
  "Предикат для фильтрования"
  [query key val]
  (-> query (where (= key val))) )


;;------------------------------------------------------------------------------
;; BEGIN: Common predicates
;; tag: <common predicates>
;; description: Общие предикаты
;;------------------------------------------------------------------------------

(defn where-eq-for-fields [entity vals fields]
  (reduce (fn [entity k] (where entity (= k (k vals)))) entity fields))

(defn where-eq-for-each-field [entity vals]
  (where-eq-for-fields entity vals (keys vals)))

;; END Common predicates
;;..............................................................................

;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------

(defmacro def-files-entitys-map []
  '(def files-entitys-map {})) ;; :entity [entity field]

;;(declare-files-entitys-map)

(defmacro def-files-entitys-map-add [entity-key entity field]
  (concat '(def files-entitys-map) [(concat '(assoc files-entitys-map) [entity-key [entity field]])]))

;; пример определения
;; (files-entitys-map-add :e1 select*-entity :field1)

(defn files-for* [files-entitys-map files entity-key entity-id filegroup]
  (let [[entity field] (files-entitys-map entity-key)]
    (-> (select* entity)
        (with files)
        (common-filter-by= :files.typegroup filegroup)
        (common-filter-by= field entity-id))))

(defn save-entity-file-rel [files-entitys-map entity-key e-id f-id]
  (let [[entity field] (files-entitys-map entity-key)]
    (insert entity (values {:files_id f-id field e-id}))))

(defn delete-entity-file-rel [files-entitys-map entity-key e-id f-id]
  (let [[entity field] (files-entitys-map entity-key)]
    (delete entity (where (and (= :files_id f-id) (= field e-id))))))

 
(defn save-file [files vals]
  (common-save-for-id files vals))
