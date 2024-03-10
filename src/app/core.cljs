(ns app.core
  "This namespace contains the application and is the entrypoint for 'yarn start'."
  (:require
    ["normalize-css"]
    [reagent.core :as r]
    [re-frame.core :as rf]
    [app.daily-journal :refer [journal-app]]))

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (r/render [journal-app] (.getElementById js/document "app")))

(defn ^:export main
  "Run application startup logic."
  []
  (rf/dispatch-sync [:initialize])
  (render))
