(ns actus.common-web

  (:use compojure.core)

  (:use hiccup.core)
  (:use hiccup.page)
  (:use hiccup.form)
  (:use hiccup.element)
  (:use hiccup.util)

  (:require [net.cgrand.enlive-html :as h]
            [actus.common-db-sql :as cdbsql]

            [clj-time.core :as tco]
            [clj-time.format :as tf]
            [clj-time.coerce :as tc]
            [clj-time.local :as tl]

            ))


(declare alert- alert-page)


;;**************************************************************************************************
;;* BEGIN PATH
;;* tag: <path>
;;*
;;* description: Пути
;;*
;;**************************************************************************************************

(def path-actus-root "/actus")

(defmacro path-actus [path]
  (let [p (str path-actus-root path)]
    p))

;; END PATH
;;..................................................................................................


;;------------------------------------------------------------------------------
;; BEGIN: TEMPLATE
;; tag: <template>
;; description: Основной шаблон
;;------------------------------------------------------------------------------

(def head
  [:head
   [:title "Politrend"]
   [:meta {:charset "utf-8" :name "viewport" :content "width=device-width, initial-scale=1"}]

   (include-css (path-actus "/css/bootstrap.css"))
   (include-css (path-actus "/css/actus.css"))
   ;;(include-css "/css/bootswatch.min.css")

   (include-js  (path-actus "/js/jquery-2.1.0.min.js"))
   (include-js  (path-actus "/js/ckeditor/ckeditor.js"))
   ])

(def page-footer
  [:footer "Подвал"])

(defmacro actus-content-template [navbar-panel]
  (let [n 'actus-content-template]
    `(def ~n (fn n [content#]
               (html5 {:lang "ru"}
                      head
                      [:body
                       [:nav ~navbar-panel]
                       [:br]
                       [:br]
                       [:br]

                       [:div {:class "container"}

                        content#
                        ;;(repeat 20 content)

                        [:br]
                        ]
                       page-footer

                       (include-js (path-actus "/js/bootstrap.min.js"))
                       ]
                      )))))


;; END TEMPLATE
;;..............................................................................

;; END TEMPLATES
;;..................................................................................................


;;**************************************************************************************************
;;* BEGIN common tools
;;* tag: <common tools>
;;*
;;* description:
;;*
;;**************************************************************************************************

(defn assoc-maps-with-cycle-value [maps-list upd-key cycle-vals]
  (map #(assoc % upd-key %2) maps-list (cycle cycle-vals)))

;; END common tools
;;..................................................................................................



;;**************************************************************************************************
;;* BEGIN FORMATTERS
;;* tag: <formatters>
;;*
;;* description: Форматировщики данных
;;*
;;**************************************************************************************************

(def formatter-yyyy-MM-dd-HH:mm:ss (tf/formatter "yyyy-MM-dd HH:mm:ss"))

(def formatter-yyyy-MM-dd (tf/formatter "yyyy-MM-dd"))

(def formatter-HH:mm:ss (tf/formatter "HH:mm:ss"))


(def formatter-local-yyyy-MM-dd-HH:mm:ss (tf/formatter-local "yyyy-MM-dd HH:mm:ss"))

(def formatter-local-yyyy-MM-dd (tf/formatter-local "yyyy-MM-dd"))

(def formatter-local-HH:mm:ss (tf/formatter-local "HH:mm:ss"))

;; END FORMATTERS
;;..................................................................................................

;;**************************************************************************************************
;;* BEGIN Javascript tools
;;* tag: <javascript>
;;*
;;* description: Утилиты для работы с кодом javascript
;;*
;;**************************************************************************************************

(defn create-sub-e-group-id [e-group-id id]
  (-> (str (name e-group-id) "_" (name id))
      (clojure.string/replace #"-" "_")
      keyword))

(defn get-param [{params :params} k default]
  (let [{n k :or {n default}} params] n))

(defn js-text-compressor-
  "Сжимает текст в одну строчку. Убирает пробелы и переносы, для коментариев надо применять /*....*/"
  [text]
  (-> text
      (clojure.string/replace #"\n" " ")
      (clojure.string/replace #"\s+" " ")
      ))

(defmacro js-text-compressor [& text]
  (println "!js-text-compressor")
  (->> (if (> (count text) 1)
         (map #(if (string? %) (js-text-compressor- %) %) text)
         `[(js-text-compressor- text)])
       (into (list str))
       reverse))

(defmacro js-text-compressor-no
  "Unformatted mock"
  [& body] `(str ~@body))

(defn js-e-set-1 [id]
  (str " this.form.elements['" (name id) "'].value = 1;"))

(defn js-e-inc [id]
  (str " v = this.form.elements['" (name id) "'].value; this.form.elements['" (name id) "'].value = parseInt(v) + 1;"))

(defn js-e-dec [id]
  (str " v = this.form.elements['" (name id) "'].value; if(v > 1) this.form.elements['" (name id) "'].value = parseInt(v) - 1;"))


;;**************************************************************************************************
;;* BEGIN Button
;;* tag: <button>
;;*
;;* description: Кнопки
;;*
;;**************************************************************************************************

(defn a-button [value attrs]
  [:button (merge {:type "button" :class "btn btn-default"} attrs) value ])

(defn a-button-default [value attrs]
  (a-button value (merge attrs {:class "btn btn-default"})))

(defn a-button-primary [value attrs]
  (a-button value (merge attrs {:class "btn btn-primary"})))

(defn a-button-success [value attrs]
  (a-button value (merge attrs {:class "btn btn-success"})))

(defn a-button-info [value attrs]
  (a-button value (merge attrs {:class "btn btn-info"})))

(defn a-button-warning [value attrs]
  (a-button value (merge attrs {:class "btn btn-warning"})))

(defn a-button-danger [value attrs]
  (a-button value (merge attrs {:class "btn btn-danger"})))

(defn a-button-link [value attrs]
  (a-button value (merge attrs {:class "btn btn-link"})))


(defn a-button-onclick [value attrs onclick]
  (a-button value (assoc attrs :onclick onclick)))

(defn a-button-onclick-default [value attrs onclick]
  (a-button-default value (assoc attrs :onclick onclick)))

(defn a-button-onclick-primary [value attrs onclick]
  (a-button-primary value (assoc attrs :onclick onclick)))

(defn a-button-onclick-success [value attrs onclick]
  (a-button-success value (assoc attrs :onclick onclick)))

(defn a-button-onclick-info [value attrs onclick]
  (a-button-info value (assoc attrs :onclick onclick)))

(defn a-button-onclick-warning [value attrs onclick]
  (a-button-warning value (assoc attrs :onclick onclick)))

(defn a-button-onclick-danger [value attrs onclick]
  (a-button-danger value (assoc attrs :onclick onclick)))

(defn a-button-onclick-link [value attrs onclick]
  (a-button-link value (assoc attrs :onclick onclick)))



;; END Button
;;..................................................................................................


;;------------------------------------------------------------------------------
;; BEGIN: AJAX
;; tag: <ajax>
;; description: ajax функционал
;;------------------------------------------------------------------------------

;; cache: false  - Очень важен для коректной работы в IE

(defn jquery-get-ua [url after-update-js-script]
  (js-text-compressor "$.get( '"url"',{}, function(){" after-update-js-script "});"))


(defn ajax-ua [url after-update-js-script]
  (js-text-compressor "
$.ajax({
url: \"" (str url) "\",
cache: false,
success: function() {
" after-update-js-script "
}});"
))

(defn ajax-udate-div-ua [url div-id after-update-js-script]
  (js-text-compressor "
$.ajax({
url: \"" (str url) "\",
cache: false,
success: function(data) {
$( \"#" (name div-id) "\" ).html(data);
" after-update-js-script "
}});"
))

(defn ajax-udate-div [url div-id]
  (ajax-udate-div-ua url div-id ""))

(defn ajax-fn-udate-div-au [f-name url div-id after-update-js-script]
  (js-text-compressor
   "function " f-name "(){ " (ajax-udate-div-ua url div-id after-update-js-script)  " };"
   ))

(defn ajax-fn-udate-div [f-name url div-id]
  (ajax-fn-udate-div-au f-name url div-id ""))

(defmacro defn-js-fn-and-call [js-fn f-name & pars]
  `(str (~js-fn ~f-name ~@pars) ";" ~f-name "();"))

;; dinamical url as js function param -----------------------------------------

(defn ajax-fn-udate-div-au-p-url [f-name div-id after-update-js-script]
  (js-text-compressor
   "function " f-name "(url){
$.ajax({
url: url,
cache: false,
success: function(data) {
$( \"#" (name div-id) "\" ).html(data);
" after-update-js-script "
}})};"
))

(defn ajax-fn-udate-div-p-url [f-name div-id]
  (ajax-fn-udate-div-au-p-url f-name div-id ""))








;; END AJAX
;;..............................................................................


;; END Javascript tools
;;..................................................................................................


;;**************************************************************************************************
;;* BEGIN Dialogs
;;* tag: <dialogs>
;;*
;;* description: Функции для работы с диалогами
;;*
;;**************************************************************************************************


(defn dialog-test []
  [:div
   [:div {:id :source-modal :class "modal fade"}
    [:div {:class "modal-dialog modal-lg"}
     [:div {:class "modal-content"}
      [:div {:class "modal-header"}
       [:button {:type "button" :class "close" :data-dismiss "modal" :aria-hidden "true"} "&times;"]
       [:h4 {:class "modal-title"} "Title" ]
       ]
      [:div {:class "modal-body"}
       "Dialog text"
       ]]]]

   [:button {:type "button" :onclick "$(\"#source-modal\").modal();"} "Dialog"]
   ])

;;------------------------------------------------------------------------------
;; BEGIN: Ajax dialog
;; tag: <ajax dialog>
;; description: Диалог с AJAX подгрузкой
;;------------------------------------------------------------------------------

(defn button-show-dialog [caption dialog-id url]
  (a-button-onclick caption {} (str "update_" (name dialog-id) "('" url "')")))

(defn dialog-ajax [e-tag-id title dialog-footer]
  (let [e-tag-id-s (name e-tag-id)
        body-id-s  (str e-tag-id-s "_dialog_body" )]
    [:div
     [:div {:id e-tag-id :class "modal fade"}
      [:div {:class "modal-dialog modal-lg"}
       [:div {:class "modal-content"}
        [:div {:class "modal-header"}
         [:button {:type "button" :class "close" :data-dismiss "modal" :aria-hidden "true"} "&times;"]
         [:h4 {:class "modal-title"} title ] ]
        [:div {:class "modal-body"}
         [:div {:id body-id-s}] [:hr] dialog-footer]
        ]]]

     (javascript-tag
      (ajax-fn-udate-div-au-p-url (str "update_" e-tag-id-s)
                                  body-id-s
                                  (str "$(\"#" e-tag-id-s "\").modal();")))


     ]))

(defn button-close-modal [caption]
  (a-button caption {:data-dismiss "modal" :aria-hidden "true"}))

;; ---

(defn a-button-dialog-ajax-cl [value attrs onclick]
  (a-button-onclick value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-default [value attrs onclick]
  (a-button-onclick-default value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-primary [value attrs onclick]
  (a-button-onclick-primary value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-success [value attrs onclick]
  (a-button-onclick-success value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-info [value attrs onclick]
  (a-button-onclick-info value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-warning [value attrs onclick]
  (a-button-onclick-warning value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-danger [value attrs onclick]
  (a-button-onclick-danger value (merge attrs {:data-dismiss "modal" }) onclick))

(defn a-button-dialog-ajax-cl-link [value attrs onclick]
  (a-button-onclick-link value (merge attrs {:data-dismiss "modal" }) onclick))



;; END Ajax dialog
;;..............................................................................


;; END Dialogs
;;..................................................................................................





;;**************************************************************************************************
;;* BEGIN table
;;* tag: <table>
;;*
;;* description: Функционал формирования таблиц
;;*
;;**************************************************************************************************


;;------------------------------------------------------------------------------
;; BEGIN: Common html table
;; tag: <table>
;; description: Функция рендеринга таблиц
;;------------------------------------------------------------------------------

(defn html-table
  "Генерирует HTML таблицу"
  [{e-group-id :e-group-id columns :columns items :items
    :or {e-group-id :simple-table}}]
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

;; Функционал для дополнения структур

(defn add-columns [{columns :columns :as table-describe} cols-describes]
  (assoc table-describe :columns (into columns cols-describes)))

(defn add-column [{columns :columns :as table-describe} col-describe]
  (assoc table-describe :columns (conj columns col-describe)))

(defn items-do-fn [{items :items :as table-describe} f]
  (assoc table-describe :items (f items)))

;; END Common html table
;;..............................................................................

;;**************************************************************************************************
;;* BEGIN html table
;;* tag: <html table>
;;*
;;* description: ПРостая таблица
;;*
;;**************************************************************************************************

(defn table-list [e-group-id {items :items :as table-describe}]
  (let [tag-id (create-sub-e-group-id e-group-id :table-list)]
    (-> table-describe
        (assoc :e-group-id tag-id)
        (assoc :items (-> items cdbsql/common-exec))
        html-table)))

;; END html table
;;..................................................................................................


;;------------------------------------------------------------------------------
;; BEGIN: Paginator
;; tag: <paginator>
;; description: Пэйджер
;;------------------------------------------------------------------------------

(defn paginator
  "Пэйджер"
  [e-group-id request p-page p-size onclick]
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

     [:input {:type "button" :class "btn btn-default"
              :value ">" :onclick (str (js-e-inc page-id) onclick)}]

     (label {} size-id " размер:")


     (drop-down {:id size-id :class "btn btn-default dropdown-toggle"
                 :onchange onclick
                 }
                p-size [5 10 15 20 50 100 1000] (Integer/parseInt size))

     [:div {:style "display: inline;"}]
     ]))


;; END Paginator
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Sorter
;; tag: <sorter>
;; description: Сортировщик
;;------------------------------------------------------------------------------

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

;; END Sorter
;;..............................................................................


;;------------------------------------------------------------------------------
;; BEGIN: Sorted and paged table
;; tag: <table>
;; description: Сортируемая постраничная таблица
;;------------------------------------------------------------------------------

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

;; END Sorted and paged table
;;..............................................................................

;; END table
;;..................................................................................................

;;**************************************************************************************************
;;* BEGIN Navbar and menu
;;* tag: <navbar-menu>
;;*
;;* description: Панель навигации
;;*
;;**************************************************************************************************

(defn html-navbar-link [text url]
  [:li [:a {:href url} text ]])

(defn html-navbar-menu-item [text url]
  [:li [:a {:href url} text ]])

(def html-navbar-menu-devider
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

;; END mavbar
;;..................................................................................................

;;**************************************************************************************************
;;* BEGIN ACTUS
;;* tag: <actus>
;;*
;;* description: Функционал создания и обработки событий для веб форм
;;*
;;**************************************************************************************************

;; ВАЖНЫЕ ПЕРЕМЕННЫЕ
(def actus-keyword :actus)
(def actus-keyword-s (name actus-keyword))

(def last-actus-keyword :last-actus)
(def actus-errors-keyword :actus-errors)
(def actus-has-es-keyword :actus-has-es)
(def actus-alerts-keyword :actus-alerts)

;;------------------------------------------------------------------------------
;; BEGIN: Alerts
;; tag: <alert>
;; description: Функционал для создания различных сообщений и подсветок
;;------------------------------------------------------------------------------

;; Добавление ошибки
(defn add-error [{actus-errors actus-errors-keyword :or {actus-errors []} :as request} error]
  (assoc request actus-errors-keyword (conj actus-errors error)))

(defn add-errors [{actus-errors actus-errors-keyword :or {actus-errors []} :as request} errors]
  (assoc request actus-errors-keyword (into actus-errors errors)))


(defn actus-add-alert
  "Добавляет сообщение в массив сообщений по ключу :actus-alerts
Пример:
:update! #(vector :form (-> %
                            (cw/actus-add-alert :info \"info\")
                            (cw/actus-add-alert :danger \"danger!\")
                            (cw/actus-add-alert :info \"info\")
                            (cw/actus-add-alert :info \"info\")
                            (cw/actus-add-alert :warning \"warning\")
                            (cw/actus-add-alert :success \"success\")
                            (cw/actus-add-e-has :keyname :error)
                            (cw/actus-add-e-has :inputEmail :warning)
                            (cw/actus-add-e-has :inputEmail nil) ;; Отключает
                         ))"
  [{actus-alerts actus-alerts-keyword :or {actus-alerts []} :as request}
   alert-type message]
  (assoc request actus-alerts-keyword
         (conj actus-alerts [alert-type message])))

(defn actus-add-e-has
  "Подсветка элемента по ключу id"
  [{actus-has-es actus-has-es-keyword :or {actus-has-es {}} :as request}
   id has-es-type]
  (assoc request actus-has-es-keyword (assoc actus-has-es id has-es-type)))

;; END Alerts
;;..............................................................................


;; ACTUS-CORE ---------------------------------------------------------------------------

(defn actus-in-form
  " Главная функция для отработки событий
Параметр:

1. request - запрос,колекция от ring

2. actus-fns - карта событий и функция от запроса (fn [request] .....)
Примеры actus-fns:
:action1 #(vec [nil (str %)])
:action2 #(vec [:form %])
:action3 #(vec [%])
:action4 #(vec [:some-tag %])
:action7 #(vector :redirect \"http://www.linux.org.ru\" %)
:action8 #(vector :response \"some body\" %)

3. render-form-fm - Функция рендер формы вида (fn [request] .....)"
  [{{actus actus-keyword :as params} :params :as request}
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
                  (= :response tag) (-> (ring.util.response/response result)
                                        (ring.util.response/charset "UTF-8"))
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


(defn actus-form-head [id]

  "Заголовок, вынесенный в def для оптимизации по скорости"
  ;;$(function() {alert('!1');});
  ;;$(function() {alert('!2');});
  ;;document.forms['form1'].actus.value=null; /* неработает в ишаке */
  ;;window.onload=function(){document.forms['" (name id) "'].actus.value=null;}
  (let [ids (name id)]
    (js-text-compressor
     "
$(function(){

document.forms['" ids "']." actus-keyword-s ".value=null;

$('#" ids  "').focusin(function(){
  document.forms['" ids "']." actus-keyword-s ".value=null;
});

$(window).load(function () {
  /* alert('манипуляции с готовой страницей'); */
});

/* alert('!'); */

})

")))


(defn actus-form-to [{{ret-url :ret-url :as params}
                      :params :as request}
                     id [method action] body]
  [:div
   (javascript-tag (actus-form-head id))

   (-> (form-to {:id id} [method action]

                (if (nil? ret-url) nil
                    (hidden-field {} :ret-url ret-url))

                (hidden-field {} actus-keyword nil) )

       (into (->> request actus-alerts-keyword
                  (map #(let [[k message] %]
                          (alert-page k message)
                          ))
                  ))

       ;;FOR DEBUG
       ;;(conj (alert-page :info "!"))

       (into body)
       )
   ]
  )


;; BUTTONS ------------------------------------------------------------------------------------------


;;------------------------------------------------------------------------------
;; BEGIN: Actus button functional
;; tag: <actus-button>
;; description: Кнопки актуса
;;------------------------------------------------------------------------------

(defn actus-button [actus value attrs]
  [:input (merge {:type "button"
                  :class "btn btn-default" :value value
                  :onclick (str "this.form.elements['" actus-keyword-s "'].value='"
                                (name actus) "';this.form.submit();")}
                 attrs)
   ])

(defn actus-button-wapl [actus value params attrs]
  (let [[input attrs-1 & other] (actus-button actus value attrs)
        onclick (:onclick attrs-1)]
    (into [input
           (assoc attrs-1
             :onclick (->> (map #(str "this.form.elements['" (name %) "'].value='" (params %) "';") (keys params))
                           (apply str)
                           (#(str % onclick))
                           ))]
          other)))

(defn actus-button-wapl-default [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-default"}))

(defn actus-button-wapl-primary [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-primary"}))

(defn actus-button-wapl-success [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-success"}))

(defn actus-button-wapl-info [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-info"}))

(defn actus-button-wapl-warning [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-warning"}))

(defn actus-button-wapl-danger [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-danger"}))

(defn actus-button-wapl-link [actus value params]
  (actus-button-wapl actus value params {:class "btn btn-link"}))

;; END Actus button functional
;;..............................................................................


;; END ACTUS
;;..................................................................................................



;;**************************************************************************************************
;;* BEGIN Url tools
;;* tag: <url>
;;*
;;* description: Функционал для работы с url
;;*
;;**************************************************************************************************

;; :params
;; :uri "/content"
;; :query-string "actus=action3&id=&table_news_0_page=2&table_news_0_size=5&table_news_0_sortcol_id=id&table_news_0_sorttype_id=DESC&table_news_0_sortcol_cdate=NONE&table_news_0_sorttype_cdate=ASC"

(defn go-to-url [{{actus actus-keyword :as params} :params uri :uri query-string :query-string}
                 url-string add-params add-ret-params]
  (letfn [(actus-str-fn [x] (str actus-keyword-s "=" (name x)))]
    (let [actus-str (actus-str-fn actus)
          new-query (url uri (-> params
                                 (dissoc actus-keyword)
                                 (assoc last-actus-keyword actus)
                                 (merge add-ret-params)
                                 ))
          ]

      ;; (url "/group/" 4 "/products" {:page 9})

      (str (url url-string
                (->> {:ret-url (str new-query)}
                     (merge add-params) )
                ))
      )))


(defn add-params-to-url
  "Добавляет парамтры в url или списка {} где есть уже другие параметры"
  [url-str add-params]
  (if (empty? add-params)
    url-str
    (str url-str "&" (url-encode add-params))))

;; END Url tools
;;..................................................................................................

;;**************************************************************************************************
;;* BEGIN INPUT ELEMENTS
;;* tag: <inputs>
;;*
;;* description: Элементы ввода форм
;;*
;;**************************************************************************************************


(defn get-parametr [request p-name]
  (-> request :params p-name))

(defn a-hidden-field [request p-name]
  (hidden-field {} p-name (get-parametr request p-name)))

(defn actus-hidden-field [params attrs id default-value]
  (let [{value id :or {value default-value}} params]
    (hidden-field attrs id value)))

(defn actus-text-field [params attrs id default-value]
  (let [{value id :or {value default-value}} params]
    (text-field (merge {:class "form-control"} attrs) id value)))

(defn actus-text-area [params attrs id default-value]
  (let [{value id :or {value default-value}} params]
    (text-area (merge {:class "form-control"} attrs) id value)))


;;------------------------------------------------------------------------------
;; BEGIN: File uploading
;; tag: <file upload>
;; description: Элемент выгрузки фалов
;;------------------------------------------------------------------------------

(defn actus-file-upload [id url-str header-params update-javascript attrs]
  (let [ids (name id)]
    [:div {:class "input-group"}
     [:span {:class "input-group-btn"}
      [:span {:class "btn btn-default btn-file"}
       "Открыть..."
       (file-upload (merge {:multiple true } attrs) id)]]
     [:div {:class "progress progress-striped" :style "margin: 10px"}
      [:div {:id (str ids "-progress") :class "progress-bar" :style "width: 0%"}
       ]]

     (javascript-tag
      (js-text-compressor "

$('#" ids "').change(function(){

progressbar = $('#" ids "-progress');
progressbar.css('width','0%');
progressbar.html('0%');

var i = 0;
for(i = 0; i < this.files.length; i++) {
file = this.files[i];

var x = new XMLHttpRequest();

x.upload.onprogress=function(e){
if (e.lengthComputable) {
var percentComplete = Math.round(100 * e.loaded / e.total );
progressbar.html(percentComplete + '%');
progressbar.css('width','' + percentComplete + '%');
} else {

}
};

x.onreadystatechange = function() {
if (x.readyState != 4) return;
if (x.status == 200) {

/* var url=this.getResponseHeader('path'); */
/* alert(this.getAllResponseHeaders()); */
" update-javascript "
} else {
progressbar.css('width','0%');
progressbar.html('0%');
alert(x.statusText +  ' Возможно размер файла слишком велик!');
}
};

x.upload.onload=function(e){
progressbar.css('width','100%');
progressbar.html('100%');
};

x.upload.onloadend=function(e){};

x.open('POST', '" url-str "');
x.setRequestHeader('filename', encodeURIComponent(file.name));"

(reduce
 #(str % "x.setRequestHeader('" (name %2) "', '" (%2 header-params)  "');")
 ""
 (keys header-params)) "
x.send(file);
}

this.value = '';
progressbar.css('width','0%');


});
"))

]))

;;------------------------------------------------------------------------------
;; BEGIN: File uploading tools
;; tag: <file upload tools>
;; description: Функции для выгрузки файлов
;;------------------------------------------------------------------------------

(defn upload-file [{{filename "filename" typegroup "type-group" } :headers body :body
                    content-length :content-length :as request}
                   & [buffer-size base-dir next-dir max-length]]
  (let [file-name (ring.util.codec/url-decode filename)
        type-group (Integer/parseInt typegroup)
        buf-size (or buffer-size (* 1024 1024))
        ;;buf (byte-array buf-size)
        tmp-next-path (str (or next-dir "") "/" file-name)
        tmp-path (str (or base-dir "/tmp") tmp-next-path)
        tmp (clojure.java.io/file tmp-path)]

    (println "\nUploading file: " filename  " to: " tmp-path)

    (if (>= content-length (or max-length (* 1024 1024)))
      (throw (Exception. "Content lenght is very long!"))

      (do
        (clojure.java.io/make-parents tmp)
        (with-open [in (clojure.java.io/input-stream body)
                    out (clojure.java.io/output-stream tmp)]

          ;; HARD VARIANT
          ;; (loop [bytes-read (.read in buf 0 buf-size)]
          ;;   (if (> bytes-read 0)
          ;;     (do
          ;;       (.write out buf 0 bytes-read)
          ;;       (recur (.read in buf 0 buf-size)))
          ;;     (do
          ;;       (.close in)
          ;;       (.close out)
          ;;       {:file-name-utf8 file-name :file-name-web filename})
          ;;     ))

          (try
            (do
              (clojure.java.io/copy in out :buffer-size buf-size)
              (.close in)
              (.close out)
              {:path tmp-next-path
               :urlpath (ring.util.codec/url-encode tmp-next-path)
               :filename file-name :typegroup type-group})
            (catch Exception ex
              (do
                (.close in)
                (.close out)
                (throw ex) )))
          )))))


(defn make-date-dirs [base-dir suffix]
  (str base-dir (tf/unparse
                 ;; (tf/formatter "/yyyy/MM/dd/HH/mm/ss")
                 (tf/formatter-local "/yyyy/MM/dd/HH/mm/ss")
                 ;;(tco/now)
                 (tl/local-now)
                 ) suffix))


(defn fn-file-upload-and-save [files-entity buffer-size base-dir prefix-dir suffix-dir max-file-size]
  (let [t-buffer-size (or buffer-size 1024)
        t-base-dir (or base-dir "/tmp")
        t-prefix-dir (or prefix-dir "")
        t-suffix-dir (or suffix-dir "")
        t-max-file-size (or max-file-size (* 10 1024 1024))]

    (fn [request]
      (cdbsql/common-save-for-id files-entity
                                 (upload-file request t-buffer-size t-base-dir
                                              (make-date-dirs t-prefix-dir t-suffix-dir) ;; "/prefix" "/sufix"
                                              t-max-file-size)))))

;; END File uploading tools
;;..............................................................................
;; END File uploading
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: Files table list
;; tag: <files table list>
;; description: Вывод списка фалов
;;------------------------------------------------------------------------------

(def table-files-list
  {:name :files
   :columns [
             {:field :files_id
              :text "№"
              :getfn :files_id
              :sorter true
              }

             {:field :filename
              :text "Наименование"
              :getfn (fn [row]
                       (let [url-path (str "/file" (:path row))]
                         [:a {:href url-path :target "_blank"} (:filename row)]))
              }
             ]
   })

(def table-images-list
  {:name :images
   :columns [
             {:field :files_id
              :text "№"
              :getfn :files_id
              :sorter true
              }

             {:field :filename
              :text "Наименование"
              :getfn (fn [row]
                       (let [url-path (str "/image" (:path row))]
                         [:div
                          [:img {:src url-path :alt "нет изобр." :height "42" :width "42"}]
                          [:a {:href url-path :target "_blank"} (:filename row)]
                          ]))
              }

             ]
   })

(defn files-list [files-entitys-map files-entity entity-key-s e-id-s group-s dialog-tag-id upd-fn]
  (let [entity-key (keyword entity-key-s)
        e-id (Integer/parseInt e-id-s)
        group (Integer/parseInt group-s)
        tag-id (create-sub-e-group-id entity-key (str e-id))]

    [:div
     (table-list :files (assoc (-> (cond (= group 0) table-files-list
                                         (= group 1) table-images-list
                                         :else table-files-list)

                                   (add-column
                                    {:text "Действие"
                                     :getfn #(let [{id :id} %]
                                               (button-show-dialog "Уд!" dialog-tag-id
                                                                   (url "/files/" (name entity-key) "/" e-id "/delfile/" id "/question-dialog"
                                                                        {:upd-fn (or upd-fn "update") :dialog-tag-id (or dialog-tag-id "del-dialog-1") })))
                                     }))

                          :items (cdbsql/files-for* files-entitys-map files-entity entity-key e-id group)))
     ]))

(defn files-list-as-html [files-entitys-map files-entity entity-key id group dialog-tag-id upd-fn]
  (html (files-list files-entitys-map files-entity entity-key id group dialog-tag-id upd-fn)))


(defn delete-entity-file?-question-dialog [entity-key e-id f-id dialog-tag-id upd-fn]
  (html (str "Удалить фаил  " f-id " ?")
        [:div
         (a-button-dialog-ajax-cl-warning
          "Удалить" {}
          (ajax-ua (url "/files/" entity-key "/" e-id "/delfile/" f-id ) (or upd-fn ""))) " "

         (button-close-modal "Отмена")
         ]))

(defn delete-entity-file [files-entitys-map entity-key e-id f-id]
  (println entity-key " " e-id " " f-id )
  (cdbsql/delete-entity-file-rel files-entitys-map
                                 (keyword entity-key)
                                 (Integer/parseInt e-id)
                                 (Integer/parseInt f-id)))


;;------------------------------------------------------------------------------
;; BEGIN: Fule uploading routes
;; tag: <file upload routes>
;; description: Маршруты для файлового аплоадера
;;------------------------------------------------------------------------------


(defn create-routes-for-file-upload* [files-entitys-map ;; Какта соответствия привязанных чущностей
                                      files-entity buffer-size base-dir prefix-dir suffix-dir max-file-size]
  (defroutes routes-for-file-upload*
    ;; Выгрузка файлов
    (POST "/files/upload" request
          (let [new-file  ((fn-file-upload-and-save files-entity buffer-size base-dir prefix-dir suffix-dir max-file-size) request)
                id (:id new-file)
                path (:path new-file)
                url-path (:urlpath new-file)
                result (cdbsql/save-entity-file-rel files-entitys-map
                                                    (-> request :headers (get "entity-key") keyword)
                                                    (-> request :headers (get "id") (Long/parseLong))
                                                    id)]
            {:status 200
             :headers {"files_id" (str id)
                       "urlpath" url-path "path" path}}
            ))

    (POST "/files/upload/onefile" request
          (let [new-file  ((fn-file-upload-and-save files-entity buffer-size base-dir prefix-dir suffix-dir max-file-size) request)]
            {:status 200
             :headers {"files_id" (-> new-file :id str)
                       "urlpath" (-> new-file :urlpath) "path" (-> new-file :path)}}))

    ;; Получение списка файлов
    (GET "/files/:entity/:id/list/:group" [entity id group dialog-tag-id upd-fn]
         (files-list-as-html files-entitys-map files-entity entity id group dialog-tag-id upd-fn))  ; 0 is files

    ;; Удаление привязки сущьности к файлу
    (GET "/files/:entity/:e-id/delfile/:f-id" [entity e-id f-id]
         (delete-entity-file files-entitys-map entity e-id f-id))

    ;; Содержимое диалога на удаление
    (GET "/files/:entity/:e-id/delfile/:f-id/question-dialog" [entity e-id f-id dialog-tag-id upd-fn]
         (delete-entity-file?-question-dialog entity e-id f-id dialog-tag-id upd-fn))

    ;; Кэшированный источник файлов для картинок
    (GET "/image/*" {{path :*} :params :as request}
         {:status 200
          :headers {"Cache-Control" (str "max-age=" (* 60 60 24 7)) }
          :body (clojure.java.io/file (str base-dir "/" path))})

    ;; Источник файлов
    (GET "/file/*" {{path :*} :params :as request}
         (clojure.java.io/file (str base-dir "/" path)))
    ))


;; END Fule uploading routes
;;..............................................................................

(defn actus-file-upload-one-image [params e-id group & [height width text]]
  (let [file-upload-id (create-sub-e-group-id e-id :up-one-image)
        a-id (create-sub-e-group-id e-id :a)
        img-id (create-sub-e-group-id e-id :img)
        par-id (create-sub-e-group-id e-id :hidden)
        link-id (create-sub-e-group-id e-id :link-id)

        par-url (str "/image" (params e-id))

        height-s (str (or height 64))
        width-s (str (or width 64))  ]
    [:div

     (actus-file-upload file-upload-id (url "/files/upload/onefile" )
                        {:type-group group }
                        (js-text-compressor "
var url=this.getResponseHeader('path');
$(\"#" (name img-id)  "\").attr(\"src\", \"/image\" + url);
$(\"#" (name a-id)  "\").attr(\"href\", \"/image\" + url);
$(\"#" (name e-id)  "\").val(url);
$(\"#" (name link-id) "\").html(\"/image\" + url);
")
                        {:multiple false})

     (hidden-field {} e-id (params e-id))

     [:br]
     [:div {:class "panel panel-default"}
      [:div {:class "panel-heading"} (or text (str "желательный размер изображения " height-s "X" width-s) )]
      [:div {:class "panel-body"}
       [:a {:id a-id :href par-url :target "_blank"}
        [:img {:id img-id  :src par-url :alt "нет изобр." :height height-s :width width-s}]
        [:button {:id link-id :type "button" :class "btn btn-link"} par-url]
        ]]]
     ]))


(defn actus-file-upload-list [e-id entity-key id group]
  (let [update-files-fn-s (name (create-sub-e-group-id e-id :update))
        update-files-fn-call-s (str update-files-fn-s "();")
        del-dialog-id (create-sub-e-group-id e-id :del-dialog)
        entity-key-s (name entity-key)
        files-list-div-id (create-sub-e-group-id e-id :files-list-div-id)
        file-upload-id (create-sub-e-group-id e-id :up-files)]
    [:div
     (actus-file-upload file-upload-id (url "/files/upload" )
                        {:id id :type-group group :entity-key entity-key-s}
                        update-files-fn-call-s {})
     [:div {:id files-list-div-id}]
     (javascript-tag
      (defn-js-fn-and-call
        ajax-fn-udate-div update-files-fn-s
        (url "/files/" entity-key-s "/" id "/list/" group {:upd-fn update-files-fn-call-s
                                                           :dialog-tag-id del-dialog-id})
        files-list-div-id))

     (dialog-ajax del-dialog-id "Удаление записи..." "Подвал")
     ]))

;; END Files table list
;;..............................................................................





;; END INPUT ELEMENTS
;;..................................................................................................


;;**************************************************************************************************
;;* BEGIN LAYOUT ELEMENTS
;;* tag: <layout>
;;*
;;* description: Элементы для разметки
;;*
;;**************************************************************************************************

;;------------------------------------------------------------------------------
;; BEGIN: Some layout macross
;; tag: <layout>
;; description: Макросы для разметки
;;------------------------------------------------------------------------------

(defmacro div-bs-docs-section [& body]
  `[:div {:class "bs-docs-section"}  ~@body] )

(defmacro div-row [& body]
  `[:div {:class "row"} ~@body] )

(defmacro div-col-lg [cols & body]
  `[:div {:class (str "col-lg-" ~cols)}  ~@body] )

(defmacro div-well_bs-component [& body]
  `[:div {:class "well bs-component"}  ~@body] )

(defmacro div-form-horizontal [& body]
  `[:div {:class "form-horizontal"} ~@body] )

;; END Some layout macross
;;..............................................................................

;;------------------------------------------------------------------------------
;; BEGIN: ROW LAYOUT
;; tag: <row>
;; description:
;;------------------------------------------------------------------------------

(defn page-row- [cols & body]
  (into (div-col-lg cols) body))

(defmacro page-row [col-lg & body]
  (apply page-row- (into [col-lg] body)))

;; END ROW LAYOUT
;;..............................................................................


;;------------------------------------------------------------------------------
;; BEGIN: Form layouts
;; tag: <form layouts>
;; description: Элементы разметки для формы
;;------------------------------------------------------------------------------

(defn div-form- [legend & body]
  (div-well_bs-component
   (div-form-horizontal
    (into [:fieldset  [:legend legend]] body) )))

(defmacro div-form-1 [legend & body]
  (apply div-form- (into [legend] body)))

(defmacro page-form-1 [legend col-lg  & body]
  (page-row- col-lg
             (apply div-form- (into [legend] body))))

(defn page-form-1-fn [legend col-lg body]
  (page-row- col-lg (apply div-form- (into [legend] body))))


;; FORM GROUP ------------------------------------------

(defn e-has-? [{actus-has-es actus-has-es-keyword} s id]
  (str s
       (if (nil? actus-has-es) ""
           (let [es (actus-has-es id)]
             (if (nil? es) ""
                 (es {:warning " has-warning"
                      :error " has-error"
                      :success " has-success"}))))))


(defn div-form-group [request label col-lg-label col-lg-input
                      [_ {id :id} :as input]]
  [:div {:class (e-has-? request "form-group" (keyword id)) }
   [:label {:for id :class (str "col-lg-" col-lg-label " control-label")} label]
   [:div {:class (str "col-lg-" col-lg-input)}
    input ;;[:input {:type "text" :class "form-control" :id "inputEmail" :placeholder "Email"}]
    ]])


;; END Form layouts
;;..............................................................................



;;------------------------------------------------------------------------------
;; BEGIN: Message boxes
;; tag: <messagebox>
;; description: Рамки вывода различных сообщений
;;------------------------------------------------------------------------------

(defn alert- [alert-type col-lg message-body]
  (let [a-type  (or (alert-type {:warning "alert-warning"
                                 :danger "alert-danger"
                                 :success "alert-success"
                                 :info "alert-info"}) "alert-info") ]
    (div-col-lg col-lg
                [:div {:class (str "alert alert-dismissable " a-type)}
                 [:button {:type "button" :class "close" :data-dismiss "alert"} "x"]
                 message-body])))

(defn alert-page [alert-type message-body]
  (div-bs-docs-section
   (div-row
    (alert- alert-type 12 message-body)
    )))

;; END Message boxes
;;..............................................................................

;; END LAYOUT ELEMENTS
;;..................................................................................................



;;**************************************************************************************************
;;* BEGIN Entity mapping and convertation
;;* tag: <entyty map>
;;*
;;* description: Функционал для мапирования и конвертации форм
;;*
;;**************************************************************************************************


;; Расскоментировать для тестов и отладки
(comment
  (def test-entity {:id 0
                    :keyname "Keyname entity"
                    :num 10
                    :somevalue ""
                    :description "some description entity...."})

  (def test-form {:ids "1"
                  :keyname "Keyname form"
                  :num "100"
                  :description "some description form ...."})


  (def form-<map>-entity
    [
     {:e :id
      :f :ids
      :f-<-e str
      :f->-e #(Integer/parseInt %)
      :e-fn-rm? empty?
      }

     {:e :keyname
      :f :keyname
      :f-<-e str
      :f->-e str
      }

     {:e :description
      :f :description
      :f-<-e str
      :f->-e str
      }

     ])
  )

(defn fill-form-<map>-entity [fme form direction entity]
  (let [[to-k f-conv-k from-k
         from-e to-e] (cond (= :-<- direction) [:f :f-<-e :e entity form]
                            (= :->- direction) [:e :f->-e :f form entity]
                            :else (throw (Exception. (str "Error direction key '" direction  "' . Mast be only ':-<-' or ':->-' ."))))]
    (->
     ;; Проходимся и пробуем сконвертировать
     (reduce (fn [[a e] {to to-k from from-k f-conv f-conv-k
                         t-e-fn-rm? :e-fn-rm? }]
               (let [value (from from-e)
                     e-fn-rm?  (if (and (= f-conv-k :f->-e)
                                        (not (nil? t-e-fn-rm?)))
                                 t-e-fn-rm?
                                 (fn [_] false))]
                 (try
                   [(if (e-fn-rm? value)
                      (dissoc a to)
                      (assoc a to (f-conv value)) ) e]
                   (catch Exception ex
                     [a (conj e [from (str "Нерпавильный формат поля: " to ) (.getMessage ex)])])
                   )))
             [to-e []] fme)

     ;; Формируеем вывод
     ((fn [[to-e errors]]
        (cond (= :-<- direction) {:form to-e :entity entity :errors errors}
              (= :->- direction) {:form form :entity to-e :errors errors}
              :else (throw (Exception. "Error key"))
              )))
     )))

(defn try-fill-entity [{params :params :as request} fme entity entity-key-in-request]
  (let [{entity :entity errors :errors} (fill-form-<map>-entity fme params :->- entity)]
    (println entity errors)
    (if (empty? errors) (assoc request entity-key-in-request entity)
        (reduce (fn [request [input-id ex-text ex-message]]
                  (-> request
                      (actus-add-alert :danger (str ex-text ": " ex-message))
                      (actus-add-e-has input-id :error)
                      ;;(#(do (println ">>>" %) %))
                      ))
                (add-errors request errors) ;;<----
                errors)
        )))

(defn try-fill-form [{params :params :as request} fme entity]
  (let [{new-params :form errors :errors} (fill-form-<map>-entity fme params :-<- entity)]
    (if (empty? errors) (assoc request :params new-params)
        (reduce (fn [request [input-id ex-text ex-message]]
                  (-> request
                      (actus-add-alert :danger (str ex-text ": " ex-message))
                      ))
                (add-errors request errors)
                errors )
        )))

(defn do-form->- [request functions]
  ;;(println "start do-form:")
  (vector :form
          (loop [step 1 [request error] [request false] do-fn (first functions) functions-list (rest functions)]
            ;;(println step)
            (cond (not (empty? (actus-errors-keyword request))) request ;; если были ошибки от конвертатора
                  (true? error) request
                  (nil? do-fn) request
                  :else (recur (inc step)
                               (try [(do-fn request) false]
                                    (catch Exception ex
                                      [(actus-add-alert request :danger
                                                        (str "Операция не проведена, на шаге (" step ") произошла ошибка: "
                                                             (.getMessage ex)))
                                       true] ))

                               (first functions-list) (rest functions-list))))))


(defmacro do-form-from-request-> [& body]
  (let [rqname (gensym "request")]
    `(fn [~rqname]
       (do-form->- ~rqname [ ~@body ]))))

;; END Entity mapping and convertation
;;..................................................................................................




;;..................................................................................................

















;; end


;;**************************************************************************************************
;;* BEGIN standart-pages
;;* tag: <starndart pages>
;;*
;;* description: Стандартные страници
;;*
;;**************************************************************************************************


;;------------------------------------------------------------------------------
;; BEGIN: Standart page for table and entity with id
;; tag: <standart page table edit entity id>
;; description: Стандартная форма с таблицей для сущности имеющей поле id
;;------------------------------------------------------------------------------

(defn std-page-table-edit-entity-id [path-table
                                     path-edit-dialog
                                     content-template

                                     request

                                     table-describe
                                     & [add-actus-map some-content]]

  (let [t-table-describe (merge {} table-describe)
        t-actus-map (merge {
                            :add #(vector :redirect (go-to-url % path-edit-dialog
                                                               {actus-keyword :add} {}))

                            :edit #(vector :redirect (go-to-url % path-edit-dialog
                                                                {actus-keyword :edit :id (-> % :params :id)}
                                                                {:id (-> % :params :id)}))

                            :del #(vector :redirect (go-to-url % path-edit-dialog
                                                               {actus-keyword :del :id (-> % :params :id)}
                                                               {:id (-> % :params :id)}))

                            :del-dialog #(vector :response (let [{{id :id} :params} %]
                                                             (html
                                                              (str "Запись id: " id)
                                                              [:div
                                                               (actus-button-wapl-warning :del "Удалить!" {:id id}) " "
                                                               (button-close-modal "Отмена")
                                                               ])))

                            :info-dialog #(vector :response (let [{{id :id} :params} %]
                                                              (html (str "Запись id: " id))))
                            } (or add-actus-map {}))]

    (actus-in-form

     request

     t-actus-map

     (fn [request]
       (content-template
        (actus-form-to request
                       :form-1 [:get path-table]
                       [

                        (:before-table some-content)

                        (hidden-field {} :id nil)

                        (when-not (nil? (:add  t-actus-map))
                          (actus-button :add "Добавить" nil))

                        (html-table-with-page-sort
                         request
                         (-> t-table-describe
                             (add-column
                              {:text "Действие"
                               :getfn #(let [{id :id} %]
                                         [:div

                                          (when-not (nil? (:edit  t-actus-map))
                                            (actus-button-wapl :edit "Ред." {:id id} nil)) " "

                                          (when-not (nil? (:del  t-actus-map))
                                            (button-show-dialog "Уд!" :dialog_delete (url path-table {actus-keyword :del-dialog :id id}))) " "

                                          (when-not (nil? (:info-dialog  t-actus-map))
                                            (button-show-dialog "Инф." :dialog_info (url path-table {actus-keyword :info-dialog :id id})))

                                          ])
                               })
                             )
                         :0)

                        (when-not (nil? (:del  t-actus-map))
                          (dialog-ajax :dialog_delete "Удаление записи..." "Подвал"))

                        (when-not (nil? (:info-dialog  t-actus-map))
                          (dialog-ajax :dialog_info "Информация по записи" "Подвал"))

                        (:after-table some-content)

                        ;; (html-table-with-page-sort request table-news-1 :9)
                        ]))))))


;; END Standart page for table and entity with id
;;..............................................................................


;;------------------------------------------------------------------------------
;; BEGIN: Standart page for edit entity with id dialog
;; tag: <standart page table edit entity id dialog>
;; description: Стандартная форма редактирования
;;------------------------------------------------------------------------------


(defn std-page-dialog-edit-entity-id [path-edit-dialog
                                      content-template
                                      request
                                      entity
                                      default-entity
                                      save-entity-request-fn
                                      convertors-and-validators
                                      add-actus-map
                                      form-fields]

  (let [form-<map>-entity (into [;; Связывание полей
                                 {:e :id :f :id
                                  :f-<-e str
                                  :f->-e #(Long/parseLong %) :e-fn-rm? empty? }
                                 ] (or convertors-and-validators [])  )

        actus-map (merge {:close #(vector :redirect (-> % :params :ret-url))

                          :add (do-form-from-request->
                                #(try-fill-form % form-<map>-entity
                                                default-entity)) ;; Значения по умолчанию

                          :edit  (do-form-from-request->
                                  #(let [{{id :id} :params} %]
                                     (try-fill-form % form-<map>-entity
                                                    (cdbsql/common-find entity (Long/parseLong id)) )))

                          :update  (do-form-from-request->
                                    #(try-fill-entity % form-<map>-entity {} :entity)

                                    ;;;#(do (println ">>>>>>>" %) %)

                                    #(let [{e :entity :as request} %]
                                       (try-fill-form request form-<map>-entity (save-entity-request-fn e) ))

                                    ;;#(/ 1 0 %)

                                    #(actus-add-alert % :success "Операция проведена успешно!"))

                          ;;:update-and-close #(vector :redirect (-> % :params :ret-url))

                          :del #(vector :redirect (let [{{id :id ret-url :ret-url} :params} %]
                                                    (cdbsql/common-delete-for-id entity (Long/parseLong id))
                                                    ret-url))

                          ;;:upload #(s/news-upload-and-save (-> % :headers (get "id") (Long/parseLong)) %)

                          } (or add-actus-map {}))
        ]

    (actus-in-form
     request
     actus-map

     (fn [{{id :id :as params} :params :as request}]
       (content-template
        (actus-form-to
         request :form-1 [:post path-edit-dialog]
         [
          (actus-hidden-field params {} :id nil)

          ;;(alert-page :info (str params))
          ;;(println ">>>>>>>>>" request)

          (page-form-1-fn "LEGEND" 12
                          (conj

                           (form-fields request params id)

                           [:p {:class "bs-component"}
                            (actus-button :close "Назад" nil) " "
                            ;;(actus-button :update-and-close "Принять и закрыть") " "
                            (actus-button :update "Сохранить" nil) " " ;; Надо ставить пробелл так как нет переноса
                            ])
                          )
          ]))))))








;; END Standart page for edit entity with id dialog
;;..............................................................................





;; END standart-tables
;;..................................................................................................
