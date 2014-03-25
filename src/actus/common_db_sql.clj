(ns actus.common-db-sql

  (:use korma.db)
  (:use korma.core)

  (:require [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc])

  )

;; COMMON FUNCTIONS ----------------------------------------------------


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
