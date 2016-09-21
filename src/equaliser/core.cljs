(ns equaliser.core
  (:require-macros [cljs.core.async.macros :as async-macros])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async]
            [goog.events :as goog-events]))

(enable-console-print!)

(defonce equaliser
  (atom
   {
    :handles
    (vec (map #(identity {:range ""
                          :pos 100      
                          :cursor-start-pos 0
                          :handle-start-pos 0
                          :index %
                          :moving false}) (range 1 36)))}))

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
         #js {:onMouseDown
              (fn [e]
                (async/put! activate {:handle handle :client-y (. e -clientY)})) 
              :className "controller"
              :style #js{:top (str (:pos handle) "px")}
              }))
       (dom/div nil (:range handle))))))

(defn deactivate-all [handles]
  (vec (map #(assoc-in % [:moving] false) handles)))

(defn pos-bounds [new-pos]
  (cond
    (< new-pos 0) 0
    (> new-pos 190) 190
    :else new-pos))

(defn abs [v]
  (cond
    (> v 0) v
    (< v 0) (* -1 v)
    :else v))

(om/root
 (fn [data owner]
   (reify
     om/IRenderState
     (render-state [this state]
       (let [deact-fn (fn [e] (async/put! (:deactivate state) data))]
         (apply
          dom/ul #js {:id "eq"
                      :onMouseUp deact-fn
                      :onMouseLeave deact-fn}
          (om/build-all get-inner-div (:handles data) {:init-state state}))))
     
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
             (let [{:keys [handle client-y]} (async/<! activate)]               
               (om/transact! data :handles (fn [all] (vec (map #(assoc-in % [:handle-start-pos] (:pos %)) all))))
               (om/transact! handle :cursor-start-pos #(identity client-y))
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
             (let [{:keys [client-y]} (async/<! move)]
               (om/transact!
                data
                :handles
                (fn [all]
                  (let [curr-handle (first (filter #(= (:moving %) true) all))]
                    (vec (map (fn [x]
                                (assoc-in
                                 x [:pos]
                                 (let [i (:index curr-handle)
                                       n (:index x)
                                       weight (- 1 (/ (abs (- n i)) (count all)))                                       
                                       new-pos (+ (:handle-start-pos x)
                                                  (* weight (- client-y (:cursor-start-pos curr-handle))))
                                       ]
                                   (pos-bounds new-pos))))
                              all))))))
             (recur)))))))
 equaliser
 {:target (. js/document (getElementById "app"))})

