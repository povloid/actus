(ns actus.common-db-sql

  (:use korma.db)
  (:use korma.core)

  (:require [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc])

  )

;; ADAPT-SQL-TYPES

(defn adapt-row [adapter fn-num entity]
  (reduce (fn [entity field]
            (let [convert-fn  ((field adapter) fn-num) ]
              (assoc entity field (convert-fn (field entity)))))
          entity (keys adapter)))

(defn adapt-rows [adapter fn-num entitys]
  (map #(adapt-row adapter fn-num %) entitys))


;; COMMON FUNCTIONS ----------------------------------------------------

(defn common-save
  "Сохранить сущность"
  [entity vals]
  (let [r (update entity (set-fields vals) (where (= :id (vals :id))))]
    (if (nil? r)
      (insert entity (values vals))
      r)) )

(defn common-save-ad
  "Сохранить сущность"
  [entity vals adapter]
  (->> vals
      (adapt-row adapter 1)
      (common-save entity)
      (adapt-row adapter 0) )) ;; После операции тоже нужна конвертация

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

(defn common-find-ad
  "Найти сущность по :id"
  [entity id adapter]
  (adapt-row adapter 0 (common-find entity id)))

(defn common-count
  "Количество всех элементов"
  [entity]
  (-> (select entity (aggregate (count :*) :c)) first :c)  )

(defn common-exec
  "Выполнить запрос"
  [query]
  (exec query))

(defn common-exec-ad
  "Выполнить запрос"
  [query adapter]
  (adapt-rows adapter 0 (common-exec query)))

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
