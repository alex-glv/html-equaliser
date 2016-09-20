(ns equaliser.core
  (:require-macros [cljs.core.async.macros :as async-macros])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async]
            [goog.events :as goog-events]))

(enable-console-print!)

(defonce equaliser (atom {:handles [{:range "0-50hz"
                                     :pos 190
                                     :moving nil}
                                    {:range "50hz-200hz"
                                     :pos 190
                                     :moving nil}
                                    {:range "200hz-1Khz"
                                     :pos 190
                                     :moving nil}
                                    {:range "1Khz-5Khz"
                                     :pos 190
                                     :moving nil}
                                    {:range "5Khz-10Khz"
                                     :pos 190
                                     :moving nil}
                                    {:range "10Khz-15Khz"
                                     :pos 190
                                     :moving nil}]}))

(defn get-inner-div [handle owner]
  (reify
    om/IRenderState
    (render-state [this {:keys [activate move]}]      
      (dom/div
       nil
       (dom/div
        #js {:className (str "handle " (if (:moving handle) "moving" "static"))
             :onMouseMove (fn [e]                                   
                            (if (:moving @handle)                              
                              (async/put! move {:client-y (. e -clientY) :handle handle})))}
        (dom/div
         #js {:onMouseDown (fn [e] (async/put! activate handle)) 
              :className "controller"
              :style #js{:top (str (:pos handle) "px")}
              }))
       (dom/div nil (:range handle))))))

(defn deactivate-all [handles]
  (vec (map #(assoc-in % [:moving] false) handles)))

(om/root
 (fn [data owner]
   (reify
     om/IRenderState
     (render-state [this state]
       (apply
        dom/ul #js {:id "eq"
                    :onMouseUp (fn [e] (async/put! (:deactivate state) data))}
        (om/build-all get-inner-div (:handles data) {:init-state state})))
     
     om/IInitState
     (init-state [_] {:move (async/chan)
                      :deactivate (async/chan)
                      :activate (async/chan)})
     om/IWillMount
     (will-mount [_]
       (let [move (om/get-state owner :move)
             deactivate (om/get-state owner :deactivate)
             activate (om/get-state owner :activate)]
         (async-macros/go
           (loop []
             (let [handle (async/<! activate)]
               (om/transact! data :handles #(deactivate-all %))
               (om/transact! handle (fn [x] (assoc-in x [:moving] true))))
             (recur)))
         (async-macros/go
           (loop []
             (let [_ (async/<! deactivate)]               
               (om/transact! data
                             :handles
                             #(deactivate-all %)))
             (recur)))
         (async-macros/go
           (loop []
             (let [{:keys [client-y handle]} (async/<! move)]
               (om/transact! handle (fn [x] (assoc-in x [:pos] (* 190  (/ (- client-y 18) (- 217 18)))))))
             (recur)))))))
 equaliser
 {:target (. js/document (getElementById "app"))})

                                        ; ;
