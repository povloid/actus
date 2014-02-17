(ns actus.common-web

  (:use hiccup.core)
  (:use hiccup.page)
  (:use hiccup.form)
  (:use hiccup.element)

  (:require [net.cgrand.enlive-html :as h]
            [actus.common-db-sql :as cdbsql]
            ))




;; ************************************************************************************************
;; HTML ELEMENTS

(defn create-sub-e-group-id [e-group-id id]
  (keyword (str (name e-group-id) "_" (name id))))

(defn get-param [{params :params} k default]
  (let [{n k :or {n default}} params] n))

(defn js-e-set-1 [id]
  (str " this.form.elements['" (name id) "'].value = 1;"))

(defn js-e-inc [id]
  (str " v = this.form.elements['" (name id) "'].value; this.form.elements['" (name id) "'].value = parseInt(v) + 1;"))

(defn js-e-dec [id]
  (str " v = this.form.elements['" (name id) "'].value; if(v > 1) this.form.elements['" (name id) "'].value = parseInt(v) - 1;"))

(defn paginator [e-group-id request p-page p-size onclick]
  "Пэйджер"
  (let [
        paginator-id (create-sub-e-group-id e-group-id :paginator)
        page-id (create-sub-e-group-id paginator-id :page)
        size-id (create-sub-e-group-id paginator-id :size)

        {{page p-page size p-size :or {page "1" size "10"}} :params }  request

        ]
    [:div {:id paginator-id :class "ui-widget-header ui-corner-all" }

     (label {} page-id "страница:")

     [:input {:type "button" :value "<<" :onclick (str (js-e-set-1 page-id) onclick)}]
     [:input {:type "button" :value "<" :onclick (str (js-e-dec page-id) onclick)}]

     (text-field {:id page-id
                  :type "number" :pattern "\\d+" :placeholder "страница"
                  :onkeydown (str "if (event.keyCode == 13){" onclick  ";return false;}")
                  :class "ui-button ui-widget ui-state-default ui-corner-all"
                  :style "width: 50px"}
                 p-page page)

     [:input {:type "button" :value ">" :onclick (str (js-e-inc page-id) onclick)}]

     (label {} size-id "размер:")

     (drop-down {:id size-id
                 :onchange onclick
                 :class "ui-button ui-widget"
                 }
                p-size [5 10 15 20 50 100 1000] (Integer/parseInt size))

     [:div {:style "display: inline;"} " "]
     ]))

(defn columm-sorter [e-group-id label-text request columns p-sort-column-name p-sort-column-type onclick]
  (let [sorter-id (create-sub-e-group-id e-group-id p-sort-column-name)
        {{sort-column p-sort-column-name sort-type p-sort-column-type
          :or {sort-column :NONE sort-type :ASC}} :params }  request ]
    [:div {:style "display: inline;"}
     (label {} sorter-id label-text)
     (drop-down {:id sorter-id
                 :onchange onclick
                 :class "ui-button ui-widget"}
                p-sort-column-name
                columns
                (keyword sort-column))

     (drop-down {:id sorter-id
                 :onchange onclick
                 :class "ui-button ui-widget"}
                p-sort-column-type
                [["воз." :ASC] ["уб." :DESC]]
                (keyword sort-type))
     ]))

(defn column-sorters [e-group-id fn-label-text request columns pars onclick]
  [:div {:style "display: inline;"} "Сортировка: "
   (map (fn [[p-sort-column-name p-sort-column-type i]]
          (columm-sorter e-group-id (fn-label-text i) request columns p-sort-column-name p-sort-column-type onclick))
        (map #(conj (vec %) (inc %2)) pars (range))
        )])

(defn add-sorted-predicates [query sorters request]
  (let [sorters-m (apply hash-map sorters)
        params (request :params)
        ;;_ (println sorters-m)
        ]
    (->> sorters
         (filter keyword?)
         (reduce (fn [query k]
                   (let [[pc pt] (k sorters-m)
                         {c pc t pt} params]
                     (cond (or (nil? c) (nil? t)) query
                           (= (keyword c) :NONE) query
                           :else (cdbsql/common-sort-by query (keyword c) (keyword t)))))
                 query)
         ) ))



(defn html-table [{e-group-id :e-group-id columns :columns items :items}]
  "Генерирует HTML таблицу"
  [:table {:id (create-sub-e-group-id e-group-id :table)
           :class "tg" :width "100%"}

   [:tr
    (for [column columns]
      [:th (column :text)])]

   (for [item  (map #(assoc % :css-c-type %2) items (cycle ["0" "1"]))  ]
     [:tr
      (for [{getfn :getfn align :align style :style
             :or {align "l"} ;; Значения по умолчанию
             } columns]
        [:td {:class (str "tg-" align "-" (item  :css-c-type)) :style style}
         (getfn item)])
      ])
   ]
  )


(defn html-table-with-page-sort [request table-describe e-group-id-suff]
  "Отрендерить таблицу. Пример определения:

 (def table-news-1 {:name :news
                   :columns [
                             {:field :id
                              :text \"№\"
                              :align \"r\" :style \"font-size:18px;\"
                              :getfn :id
                              :sorter true
                              }

                             {:field :cdate
                              :text \"Дата\" :align \"c\"
                              :getfn :cdate
                              :sorter true
                              }

                             {:text \"Наименование\" :align \"l\"
                              :getfn #(vec [:div [:b (:keyname %)] [:br] (:top_description %) ])
                              }

                             {:text \"Рписание\" :align \"l\"
                              :getfn :description
                              }
                             ]

                   :items (-> service/news-select)
                   })
"
  (let [{n :name columns :columns items :items} table-describe
        e-group-id (-> :table (create-sub-e-group-id n) (create-sub-e-group-id e-group-id-suff))
        p-page (create-sub-e-group-id e-group-id :page)
        p-size (create-sub-e-group-id e-group-id :size)

        ;; Выпадающий список полей (меню вариантов сортировки)
        sorted-columns (->> columns
                            (filter :sorter) ;;TODO: тут можно оптимизировать 1
                            (reduce #(conj % [(%2 :text) (%2 :field)])
                                    [["Нет" :NONE]] ;; Добавляем отключающий пункт первым
                                    ))

        ;; Модель описание сортировщиков
        sorters-describe (->> columns
                              (filter :sorter) ;;TODO: тут можно оптимизировать 1
                              (reduce #(let [f (%2 :field)]
                                         (conj % f
                                               [(-> e-group-id (create-sub-e-group-id :sortcol) (create-sub-e-group-id f))
                                                (-> e-group-id (create-sub-e-group-id :sorttype) (create-sub-e-group-id f))]))
                                      []))
        ]

    [:div
     ;; Формируем верхнюю панель управления таблицей
     (conj
      (paginator (create-sub-e-group-id e-group-id :1) request p-page p-size " this.form.submit();")
      (column-sorters e-group-id #(str % " по ") request sorted-columns (filter coll? sorters-describe) " this.form.submit();")

      ;; Одиночные варианты - Возможно в будущем пригодится
      ;;(columm-sorter e-group-id "1 по" request [["Нет" :NONE]  ["№" :id] ["Дата" :cdate]] :scol1 :scolt1 nil)
      ;;(columm-sorter e-group-id "2 по" request [["Нет" :NONE]  ["№" :id] ["Дата" :cdate]] :scol2 :scolt2 nil)

      )

     ;; Формируем таблицу
     (-> table-describe
         (assoc :e-group-id e-group-id)
         (assoc :items (-> items
                           (cdbsql/common-page (dec (Integer/parseInt (get-param request p-page "1")))
                                               (Integer/parseInt (get-param request p-size "10")))

                           (add-sorted-predicates sorters-describe request)

                           cdbsql/common-exec))
         html-table)
     ]
    ))



(defn items-do-fn [{items :items :as table-describe} f]
  (assoc table-describe
    :items (f items)))
