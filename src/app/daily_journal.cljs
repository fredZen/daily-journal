(ns app.daily-journal
  (:require
   [akiroz.re-frame.storage :as storage]
   [cljss.core :as css]
   [cljss.reagent]
   [goog.string :as gstring]
   [goog.string.format]
   [medley.core :refer [assoc-some]]
   [re-frame.core :as rf]
   [taoensso.tempura :as tempura]
   [reagent.core :as r])
  (:require-macros
   [cljss.core :refer [defstyles]]
   [cljss.reagent :refer [defstyled]]))

(def translations
  {:fr {:page/cols "colonnes par page"
        :page/rows "lignes par page"
        :hour/cols "colonne par heure"
        :hour/rows "lignes par heure"
        :hour/font-size "taille police heures"
        :minute/font-size "taille police minutes"
        :day/begin "début de journée"
        :day/end "fin de journée"
        :print "imprimer"}})

(def persistent-db-keys
  [:page/rows :page/cols :hour/rows :hour/cols :day/begin :day/end :font/minutes :font/hours])

(def tr
  (partial tempura/tr {:dict translations} [:fr]))

(defn reg-event-db
  [event-id handler]
  (rf/reg-event-fx
   event-id
   [(storage/persist-db-keys :timetracking persistent-db-keys)]
   (fn [{:keys [db]} event-vec]
     {:db (handler db event-vec)})))

;;; Page
(def page-defaults
  #:page{:rows 1 :cols 6})

(reg-event-db :page/set-dimensions
              (fn [db [_ {:keys [rows cols]}]]
                (assoc-some db :page/rows rows :page/cols cols)))

(rf/reg-sub :page/rows
            (fn [db _]
              (:page/rows db)))

(rf/reg-sub :page/cols
            (fn [db _]
              (:page/cols db)))

(rf/reg-sub :page/dimensions
            (fn [db _]
              {:rows (:page/rows db)
               :cols (:page/cols db)}))

(rf/reg-sub :page/hours
            :<- [:page/dimensions]
            (fn [{:keys [rows cols]} _]
              (* rows cols)))

;;; Hour
(def hour-defaults
  #:hour{:rows 12 :cols 1})

(reg-event-db :hour/set-dimensions
              (fn [db [_ {:keys [rows cols]}]]
                (assoc-some db :hour/rows rows :hour/cols cols)))

(rf/reg-sub :hour/rows
            (fn [db _]
              (:hour/rows db)))

(rf/reg-sub :hour/cols
            (fn [db _]
              (:hour/cols db)))

(rf/reg-sub :hour/dimensions
            (fn [db _]
              {:rows (:hour/rows db)
               :cols (:hour/cols db)}))

(rf/reg-sub :hour/minute-count
            :<- [:hour/dimensions]
            (fn [{:keys [rows cols]} _]
              (* rows cols)))

;;; Day
(def day-defaults
  #:day{:begin 6 :end 24})

(reg-event-db :day/set-boundaries
              (fn [db [_ {:keys [begin end]}]]
                (assoc-some db :day/begin begin :day/end end)))

(rf/reg-sub :day/begin
            (fn [db _]
              (:day/begin db)))

(rf/reg-sub :day/end
            (fn [db _]
              (let [{:day/keys [begin end]} db
                    ; offset to ensure that the end comes strictly after the
                    ; start
                    offset (-> begin (- end) (+ 24) (quot 24) (* 24) (max 0))]
                (+ end offset))))

(rf/reg-sub :day/boundaries
            :<- [:day/begin]
            :<- [:day/end]
            (fn [[begin end] _]
              {:begin begin :end end}))

;;; Font
(def font-defaults
  #:font{:minutes {:size 6}
         :hours   {:size 96}})

(defn font-size-path [kind]
  [(keyword "font" (name kind)) :size])

(reg-event-db :font/set-size
              (fn [db [_ kind size]]
                (assoc-in db (font-size-path kind) size)))

(rf/reg-sub :font/size
            (fn [db [_ kind]]
              (get-in db (font-size-path kind))))

;;; Init
(reg-event-db
 :initialize
 (fn [db _]
   (merge page-defaults hour-defaults day-defaults font-defaults db)))

;;; Css
(defn css-repeat [attr]
  (with-meta
    #(str "repeat(" % ", 1fr)")
    attr))

(defstyled page :div
  {:display "grid"
   :grid-template-columns (css-repeat :cols)
   :grid-template-rows (css-repeat :rows)
   :gap "0.5em"
   :break-after "page"
   :width "100vw"
   :height "100vh"})

(defstyled hour-block :div
  {:display "grid"
   :border ".5mm solid black"})

(defstyled hours :div
  {:grid-area "1/1"
   :font-size :font-size
   :color "#ddd"
   :font-weight "bold"})

(defstyled minutes :div
  {:font-size :font-size
   :padding "1mm"
   :display "grid"
   :grid-area "1/1"
   :grid-template-columns (css-repeat :cols)
   :grid-template-rows (css-repeat :rows)})

(defn hour [h]
  [hour-block
   [hours {:font-size (str @(rf/subscribe [:font/size :hours]) "pt")} h]
   [minutes (assoc @(rf/subscribe [:hour/dimensions])
                   :font-size (str @(rf/subscribe [:font/size :minutes]) "pt"))
    (let [minute-count @(rf/subscribe [:hour/minute-count])]
      (doall
       (for [i (range 0 minute-count)
             :let [m (-> i (* 60) (quot minute-count))]]
         ^{:key i}
         [:div (gstring/format "%02d" m)])))]])

(defn journal []
  (let [{:keys [begin end]} @(rf/subscribe [:day/boundaries])
        hours-per-page @(rf/subscribe [:page/hours])]
    [:<>
     (doall
      (for [h-from (range begin end hours-per-page)
            :let [h-to (min end (+ h-from hours-per-page))]]
        ^{:key h-from}
        [page @(rf/subscribe [:page/dimensions])
         (for [h (range h-from h-to)]
           ^{:key h}
           [hour (mod h 24)])]))]))

(defstyled label :label
  {:display "block"})

(defn int-control [_caption value _on-change]
  (let [input-name (gensym)
        current (r/atom value)]
    (fn [caption value on-change]
      [:span
       [label {:for input-name} caption]
       [:input {:id input-name
                :value @current
                :on-change (fn [e]
                             (let [new-val (-> e .-target .-value)
                                   as-int (js/parseInt new-val)]
                               (reset! current new-val)
                               (when-not (js/isNaN as-int)
                                 (on-change as-int))))
                :on-blur (fn [_]
                           (reset! current value))}]])))

(defstyled form :form
  {::css/media {[:only :print] {:display "none"}}
   :display "flex"})

(defn controls []
  [form
   [int-control (tr [:page/cols]) @(rf/subscribe [:page/cols]) #(rf/dispatch [:page/set-dimensions {:cols %}])]
   [int-control (tr [:page/rows]) @(rf/subscribe [:page/rows]) #(rf/dispatch [:page/set-dimensions {:rows %}])]
   [int-control (tr [:hour/cols]) @(rf/subscribe [:hour/cols]) #(rf/dispatch [:hour/set-dimensions {:cols %}])]
   [int-control (tr [:hour/rows]) @(rf/subscribe [:hour/rows]) #(rf/dispatch [:hour/set-dimensions {:rows %}])]
   [int-control (tr [:day/begin]) @(rf/subscribe [:day/begin]) #(rf/dispatch [:day/set-boundaries {:begin %}])]
   [int-control (tr [:day/end]) @(rf/subscribe [:day/end]) #(rf/dispatch [:day/set-boundaries {:end %}])]
   [int-control (tr [:hour/font-size]) @(rf/subscribe [:font/size :hours]) #(rf/dispatch [:font/set-size :hours %])]
   [int-control (tr [:minute/font-size]) @(rf/subscribe [:font/size :minutes]) #(rf/dispatch [:font/set-size :minutes %])]
   [:input {:type "button"
            :value (tr [:print])
            :on-click #(js/print)}]])

(defn journal-app []
  [:<>
   [controls]
   [journal]])

