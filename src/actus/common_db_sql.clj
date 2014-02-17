(ns actus.common-db-sql
  
  (:use korma.db)
  (:use korma.core)

  (:require [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc])

  )

;; COMMON FUNCTIONS ----------------------------------------------------

(defn common-save [entity vals]
  "Сохранить сущность"
  (let [r (update entity (set-fields vals) (where (= :id (vals :id))))]
    (if (nil? r)
      (insert entity (values vals))
      r)) )

(defn common-find [entity id]
  "Найти сущность по :id"
  (first (select entity (where (= :id id)))) )

(defn common-count [entity]
  "Количество всех элементов"
  (-> (select entity (aggregate (count :*) :c)) first :c)  )

(defn common-exec [query]
  "Выполнить запрос"
  (exec query) )

;; PAGES AND SORTING --------------------------------------------------
(defn common-page [query page size]
  "Установть страницу и ее размер"
  (-> query (limit size) (offset (* page size))) )

(defn common-sort-by [query order-by asc-desc]
  "Сортировать..."
  (-> query (order order-by asc-desc)) )

(defn common-filter-by= [query key val]
  "Предикат для фильтрования"
  (-> query (where (= key val))) )

