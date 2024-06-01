(in-ns 'clojure.core)
(when (System/getProperty "babashka.version") ;; when running with Babashka
  (defn extend-type [t & specs]
    (case (second t)
      java.lang.AssertionError :noop
      java.nio.charset.CoderMalfunctionError :noop
      java.io.IOError :noop
      java.lang.ThreadDeath :noop
      java.lang.Throwable :noop
      java.lang.Object :noop
      nil :noop
      (do (prn "ELSE") (clojure.core/extend-type t)))))

(ns jq.main
  (:require
    [babashka.process :as proc]
    [babashka.json :as json]
    [clojure.test :as test]
    [clojure.tools.trace :as trace]))

(defn -main []
  (println "main: TODO")
  (shutdown-agents)) ;; without this the process will hang when running without Babashka

(trace/deftrace jq [query item]
  (comment prn "ROOT" query item)
  (let [ret (cond
              (vector? item) (map #(jq query %) item)
              :else (cond
                      (test/function? query) (do (comment prn "func" query item)
                                                 (query item))
                      (vector? query) (do (comment prn "vector2" query item)
                                          (let [vector2 (reduce (fn [acc k] (jq k acc)) item query)]
                                            (comment prn "vector2" vector2)
                                            vector2))
                      (map? query) (do (prn "map" query item)
                                       (into {} (map
                                                  (fn [%]
                                                    (trace/trace "KEYVAL" [% (key %) (val %)])
                                                    [(key %) (jq (val %) item)])
                                                  query)))
                      :else (do (comment prn "else" query item) (get-in item [query]))))]
    (comment prn "reduce-return" ret)
    ret))


(assert (test/is (= "value1" (jq :key1 {:key1 "value1"}))))
(assert (test/is (= "value1" (jq [:key1] {:key1 "value1"}))))
(assert (test/is (= "value2" (jq [:key1 :key2] {:key1 {:key2 "value2"}}))))
(assert (test/is (= ["valueA"] (jq [:key1 [:key2]] {:key1 [{:key2 "valueA"}]}))))
(assert (test/is (= ["valueA" "valueB"] (jq [:key1 [:key2]] {:key1 [{:key2 "valueA"} {:key2 "valueB"}]}))))
(assert (test/is (= "value3" (jq [#(:key1 %)] {:key1 "value3"}))))
(assert (test/is (= {:keyA "value3"} (jq [{:keyA [:key1]}] {:key1 "value3"}))))
(assert (test/is (= {:keyA "value3" :keyB "value3"} (jq [{:keyA [:key1] :keyB [:key1]}] {:key1 "value3"}))))
(assert (test/is (= {:key1 "value3" :keyY [{:key4 "valueA"} {:key4 "valueB"}]}
                    (jq
                      [{:key1 [:key1]
                        :keyY [:key3]}]
                      {:key1 "value3"
                       :key3 [{:key4 "valueA"}
                              {:key4 "valueB"}]}))))
;; Parsing JSON
(assert (test/is (= 3.14 (json/read-str (:out (proc/sh "echo 3.14"))))))

;; Parsing JSON and querying
(assert (test/is (= {:key 3.14} ;; want
                    (jq
                      {:key [:a]} ;; query
                      (json/read-str (:out (proc/sh "echo '{\"a\": 3.14}'"))))))) ;; item
