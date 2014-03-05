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


(defn js-text-compressor [text]
  "Сжимает текст в одну строчку"
  (-> text
      (clojure.string/replace #"\n" " ")
      (clojure.string/replace #"\s+" " ")
      ))

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
    [:div {:id paginator-id}

     (label {} page-id " страница:")

     [:input {:type "button" :class "btn btn-default" :value "<<" :onclick (str (js-e-set-1 page-id) onclick)}]
     [:input {:type "button" :class "btn btn-default" :value "<" :onclick (str (js-e-dec page-id) onclick)}]

     (text-field {:id page-id
                  :type "number" :pattern "\\d+" :placeholder "страница"
                  :onkeydown (str "if (event.keyCode == 13){" onclick  ";return false;}")
                  :style "width: 100px"
                  :class "btn"}
                 p-page page)

     [:input {:type "button" :class"btn btn-default"
              :value ">" :onclick (str (js-e-inc page-id) onclick)}]

     (label {} size-id " размер:")


     (drop-down {:id size-id :class "btn btn-default dropdown-toggle"
                 :onchange onclick
                 }
                p-size [5 10 15 20 50 100 1000] (Integer/parseInt size))

     [:div {:style "display: inline;"}]
     ]))

(defn columm-sorter [e-group-id label-text request columns p-sort-column-name p-sort-column-type onclick]
  (let [sorter-id (create-sub-e-group-id e-group-id p-sort-column-name)
        {{sort-column p-sort-column-name sort-type p-sort-column-type
          :or {sort-column :NONE sort-type :ASC}} :params }  request ]
    [:div {:style "display: inline;"}
     (label {} sorter-id label-text)
     (drop-down {:id sorter-id
                 :onchange onclick
                 :class "btn btn-default dropdown-toggle"}
                p-sort-column-name
                columns
                (keyword sort-column))

     (drop-down {:id sorter-id
                 :onchange onclick
                 :class "btn btn-default dropdown-toggle"}
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
  [:div {:class "bs-example table-responsive"}
   [:table {:id (create-sub-e-group-id e-group-id :table)
            :class "table table-striped table-hover" :width "100%"}
    [:thead
     [:tr
      (for [column columns]
        [:th (column :text)])]]

    (for [item
          ;;(map #(assoc % :css-c-type %2) items (cycle ["0" "1"])) ;; Код чередования для старого CSS (чередование цвета)
          items]
      [:tr
       (for [{getfn :getfn style :style} columns]
         [:td {:style style}  (getfn item)])
       ])
    ]]
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

    [:div {:class "panel panel-default"}
     [:div {:class "panel-heading"}
      ;; Формируем верхнюю панель управления таблицей
      (conj
       (paginator (create-sub-e-group-id e-group-id :1) request p-page p-size " this.form.submit();")
       " "
       (column-sorters e-group-id #(str " " % " по: ") request sorted-columns (filter coll? sorters-describe) " this.form.submit();")

       ;; Одиночные варианты - Возможно в будущем пригодится
       ;;(columm-sorter e-group-id "1 по" request [["Нет" :NONE]  ["№" :id] ["Дата" :cdate]] :scol1 :scolt1 nil)
       ;;(columm-sorter e-group-id "2 по" request [["Нет" :NONE]  ["№" :id] ["Дата" :cdate]] :scol2 :scolt2 nil)

       )]
     [:div {:class "panel-body"}
      ;; Формируем таблицу
      (-> table-describe
          (assoc :e-group-id e-group-id)
          (assoc :items (-> items
                            (cdbsql/common-page (dec (Integer/parseInt (get-param request p-page "1")))
                                                (Integer/parseInt (get-param request p-size "10")))

                            (add-sorted-predicates sorters-describe request)

                            cdbsql/common-exec))
          html-table)]
     ]
    ))


(defn add-columns [{columns :columns :as table-describe} cols-describes]
  (assoc table-describe :columns (into columns cols-describes)))

(defn add-column [{columns :columns :as table-describe} col-describe]
  (assoc table-describe :columns (conj columns col-describe)))


(defn items-do-fn [{items :items :as table-describe} f]
  (assoc table-describe :items (f items)))


;; table buttons


;; MENU -------------------------------------------------------------------------------------------------------------------

(defn html-navbar-link [text url]
  [:li [:a {:href url} text ]])

(defn html-navbar-menu-item [text url]
  [:li [:a {:href url} text ]])

(def html-navbar-menu-devider-------------------------------
  [:li {:class "divider"} ])

(defn html-navbar-menu-header [text]
  [:li {:class "dropdown-header"} text])

(defn html-navbar-menu [text sub-items]
  [:li {:class "dropdown"}
   [:a {:href "#" :class "dropdown-toggle" :data-toggle "dropdown"} text [:b {:class "caret"} ]]
   (reduce conj [:ul {:class "dropdown-menu"} ] sub-items)
   ]
  )


(defn html-navbar [header_text
                   header_url

                   items-left

                   any-middle-elements

                   items-right
                   ]

  [:div {:class "navbar navbar-default navbar-fixed-top"} ;; 1. navbar
   [:div {:class "container"} ;; 2. container

    ;; заголовок / ссылка на домашнюю страницу ff -------------------------------------------------------------------------
    [:div {:class "navbar-header"} ;; заголовок
     [:button {:type "button" :class "navbar-toggle" :data-toggle "collapse" :data-target ".navbar-responsive-collapse"}
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]
      [:span {:class "icon-bar"}]
      ]
     [:a {:class "navbar-brand" :href header_url} header_text]
     ] ;; заголовок ...

    ;; Меню и прочие элементы
    (-> [:div {:class "navbar-collapse collapse navbar-responsive-collapse"}]
        (conj (reduce conj [:ul {:class "nav navbar-nav"}] items-left))

        (into any-middle-elements)

        (conj (reduce conj [:ul {:class "nav navbar-nav  navbar-right"}] items-right))
        )
    ;;
    ] ;; 2. container
   ] ;; 1. navbar
  )

;; MENU ...


;; WEB FORMS
(defn actus-in-form [{{actus :actus :as params} :params :as request}
                     actus-fns
                     render-form-fm]

  ;;(println "\n\n" params)

  (let [actus-fn (actus-fns (keyword actus))]
    (if (nil? actus-fn)
      (render-form-fm request) ;; render form request
      #_(html5 [:h1 "Не найден actus: " (keyword actus)]
               [:br]
               [:h2 "Результат:"]
               [:br]
               (str request) )

      (let [result (actus-fn request)]
        (if (and (or (vector? result)
                     (list? result))
                 (keyword? (first result)))

          ;; Если условие проходит то считаем что это action
          (let [[tag result] result]
            (cond (= :form tag) (render-form-fm result) ;; render form request
                  (= :response tag) (ring.util.response/response result)
                  (= :redirect tag) (ring.util.response/redirect result)
                  :else (html5 [:h1 "Не найден tag: " tag ]
                               [:br]
                               [:h2 "Результат:"]
                               [:br]
                               (str [tag result]) )))

          ;; Иначе выводим то что есть как строку
          (html5 [:h1 "Результат не имеет структуры вида [:tag some-result] или [:tag some-result]."]
                 [:br]
                 [:h2 "Получено только:"]
                 [:br]
                 (str result))
          )
        )
      )
    )
  )


(defn actus-form-to [id [method action] body]
  [:div
   (javascript-tag (str "window.onload=function(){document.forms['" (name id) "'].actus.value=null;}"))
   ;;(javascript-tag "document.forms['form1'].actus.value=null;") ;; неработает в ишаке
   (into (form-to {:id id} [method action]
                  (hidden-field {} :actus nil)
                  )
         body)
   ]
  )

(defn actus-button [actus value]
  [:input {:type "button"
           :class "btn btn-default" :value value
           :onclick (str "this.form.elements['actus'].value='" (name actus) "';this.form.submit();")}])

(defn actus-button-wapl [actus value params]
  (let [[input attrs & other] (actus-button actus value)
        onclick (:onclick attrs)]
    (into [input (assoc attrs :onclick (->> (map #(str "this.form.elements['" (name %) "'].value='" (params %) "';") (keys params))
                                            (apply str)
                                            (#(str % onclick))
                                           ))
           ]
          other)))
