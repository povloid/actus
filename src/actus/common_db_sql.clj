(ns actus.common-db-sql
  
  (:use korma.db)
  (:use korma.core)

  (:require [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc])

  )

;; COMMON FUNCTIONS ----------------------------------------------------

(defn common-save 
  "Сохранить сущность"
  [entity vals]
  (let [r (update entity (set-fields vals) (where (= :id (vals :id))))]
    (if (nil? r)
      (insert entity (values vals))
      r)) )

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
  (exec query) )

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


;; ---------------------------------------------------------------------





