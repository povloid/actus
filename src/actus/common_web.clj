(ns actus.common-web

  (:use hiccup.core)
  (:use hiccup.page)
  (:use hiccup.form)
  (:use hiccup.element)
  (:use hiccup.util)

  (:require [net.cgrand.enlive-html :as h]
            [actus.common-db-sql :as cdbsql]
            ))



(declare alert- alert-page)


;; ************************************************************************************************
;; HTML ELEMENTS

(defn create-sub-e-group-id [e-group-id id]
  (keyword (str (name e-group-id) "_" (name id))))

(defn get-param [{params :params} k default]
  (let [{n k :or {n default}} params] n))


(defn js-text-compressor
  "Сжимает текст в одну строчку. Убирает пробелы и переносы, для коментариев надо применять /*....*/"
  [text]
  (-> text
      (clojure.string/replace #"\n" " ")
      (clojure.string/replace #"\s+" " ")
      ))

(defn js-text-compressor-map-str
  "[ \" alert   ('\" :key  \"')  ;  \" ... ] => [ \"alert('\" :key  \"');\" ... ]"
  [x]
  (map
   #(if (string? %) (js-text-compressor %) %)
   x))

(defn js-text-compressor-coll-as-str
  "(js-text-compressor-coll-as-str [ \"alert('\" :key  \"');\" ... ] {:key \"!!!\", ... } )
=> \"alert('!!!');....\" "
  [c m]
  (->> (replace m c)
       (apply str)))





(defn js-e-set-1 [id]
  (str " this.form.elements['" (name id) "'].value = 1;"))

(defn js-e-inc [id]
  (str " v = this.form.elements['" (name id) "'].value; this.form.elements['" (name id) "'].value = parseInt(v) + 1;"))

(defn js-e-dec [id]
  (str " v = this.form.elements['" (name id) "'].value; if(v > 1) this.form.elements['" (name id) "'].value = parseInt(v) - 1;"))

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



(defn html-table
  "Генерирует HTML таблицу"
  [{e-group-id :e-group-id columns :columns items :items}]
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




(def actus-form-head

  "Заголовок, вынесенный в def для оптимизации по скорости"
  ;;$(function() {alert('!1');});
  ;;$(function() {alert('!2');});
  ;;document.forms['form1'].actus.value=null; /* неработает в ишаке */
  ;;window.onload=function(){document.forms['" (name id) "'].actus.value=null;}

  (js-text-compressor-map-str
   (list "

$(function(){

document.forms['" :form-id "'].actus.value=null;

$('#" :form-id  "').focusin(function(){
  document.forms['" :form-id "'].actus.value=null;
});

$(window).load(function () {
  /* alert('манипуляции с готовой страницей'); */
});

/* alert('!'); */

})

")))


(defn actus-add-alert
  "Добавляет сообщение в массив сообщений по ключу :actus-alerts"
  [{actus-alerts :actus-alerts :or {actus-alerts []} :as request}
   alert-type message]
  (assoc request :actus-alerts (conj actus-alerts [alert-type message])))

(defn actus-add-e-has
  "Подсветка элемента по ключу id"
  [{actus-has-es :actus-has-es :or {actus-has-es {}} :as request}
   id has-es-type]
  (assoc request :actus-has-es (assoc actus-has-es id has-es-type)))


(defn actus-form-to [{{ret-url :ret-url :as params}
                      :params :as request}
                     id [method action] body]
  [:div
   (javascript-tag (js-text-compressor-coll-as-str
                    actus-form-head {:form-id (name id)}))

   (-> (form-to {:id id} [method action]

                (if (nil? ret-url) nil
                    (hidden-field {} :ret-url ret-url))

                (hidden-field {} :actus nil)

                )

       (into (->> request :actus-alerts
                  (map #(let [[k message] %]
                          (alert-page k message)
                          ))
                  ))

       ;;(conj (alert-page :info "!!!!!!!!!!!!!!!"))

       (into body)
       )
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
                                            ))]
          other)))

;; :params
;; :uri "/content"
;; :query-string "actus=action3&id=&table_news_0_page=2&table_news_0_size=5&table_news_0_sortcol_id=id&table_news_0_sorttype_id=DESC&table_news_0_sortcol_cdate=NONE&table_news_0_sorttype_cdate=ASC"

(defn go-to-url [{{actus :actus :as params} :params uri :uri query-string :query-string}
                 url-string add-params add-ret-params]
  (letfn [(actus-str-fn [x] (str "actus=" (name x)))]
    (let [actus-str (actus-str-fn actus)
          new-query (url uri (-> params
                                 (dissoc :actus)
                                 (assoc :last_actus actus)
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





;; INPUT ELEMENTS BEGIN ---------------------------------------------------------------------------

(defn get-parametr [request p-name]
  (-> request :params p-name))

(defn a-hidden-field [request p-name]
  (hidden-field {} p-name (get-parametr request p-name)))

;; INPUT ELEMENTS END -----------------------------------------------------------------------------

(defn actus-hidden-field [params attrs id default-value]
  (let [{value id :or {value default-value}} params]
    (hidden-field attrs id value)))

(defn actus-text-field [params attrs id default-value]
  (let [{value id :or {value default-value}} params]
    (text-field attrs id value)))









;; LAYOUT -----------------------------------------------

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

;; ROW --------------------------------------------------

(defn page-row- [cols & body]
  (into (div-col-lg cols) body))

(defmacro page-row [col-lg & body]
  (apply page-row- (into [col-lg] body)))

;; FORM -------------------------------------------------

(defn div-form- [legend & body]
  (div-well_bs-component
   (div-form-horizontal
    (into [:fieldset  [:legend legend]] body) )))

(defmacro div-form-1 [legend & body]
  (apply div-form- (into [legend] body)))

(defmacro page-form-1 [legend col-lg  & body]
  (page-row- col-lg
             (apply div-form- (into [legend] body))))

;; FORM GROUP ------------------------------------------

(defn e-has-? [{actus-has-es :actus-has-es} s id]
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


;; MESSAGE BOXES ---------------------------------------
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




;; ENTITY MAPPING -------------------------------------

(comment

  (def test-entity {:id 0
                    :keyname "Keyname entity"
                    :num 10
                    :somevalue ""
                    :description "some description entity...."})

  (def test-form {:ids "1werqw"
                  :keyname "Keyname form"
                  :num "100"
                  :description "some description form ...."})


  (def form-<map>-entity
    [
     {:e :id
      :f :ids
      :f-<-e str
      :f->-e #(Integer/parseInt %)
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
                         e-fn-rm? :e-fn-rm? :or {e-fn-rm? (fn [_] false)}}]
               (let [value (from from-e)]
                 (try
                   [(if (e-fn-rm? value) (dissoc a to)
                      (assoc a to (f-conv value))) e]
                   (catch Exception ex
                     [a (conj e [from "Нерпавильный формат поля." (.getMessage ex)])])
                   )))
             [to-e []] fme)

     ;; Формируеем вывод
     ((fn [[to-e errors]]
        (cond (= :-<- direction) {:form to-e :entity entity :errors errors}
              (= :->- direction) {:form form :entity to-e :errors errors}
              :else (throw (Exception. "Error key"))
              )))
     )))


(defn validate-params-and-fill-entity [{params :params :as request} fme entity]
  ;;(println ">>>>" params)
  (let [{entity :entity errors :errors} (fill-form-<map>-entity fme params :->- entity)]
    (if (empty? errors) (vector :form request)
        (vector :form (reduce (fn [request [input-id ex-text ex-message]]
                                ;;(println ">>>>>>>>>>" input-id)
                                (-> request
                                    (actus-add-alert :danger (str ex-text ": " ex-message))
                                    (actus-add-e-has input-id :error)
                                    ;;(#(do (println %) %))
                                    ))
                              request errors
                              ))
        )))




















;; end
