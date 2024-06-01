(ns jq.main
  (:require
    [babashka.process :as proc]
    [babashka.json :as json]
    [clojure.test :as test]))

(if (System/getProperty "babashka.version") ;; when running with Babashka
  (with-redefs [clojure.core/extend-type (fn [t & _] (prn "MOCKED extend-type" (second t)))]
    (require '[clojure.tools.trace :as trace]))
  (require '[clojure.tools.trace :as trace]))

(defn -main []
  (println "main: TODO")
  (shutdown-agents)) ;; without this the process will hang when running without Babashka

(trace/deftrace jq [query item]
  (let [ret (cond
              (vector? item) (map #(jq query %) item)
              :else (cond
                      (test/function? query) (query item)
                      (vector? query) (reduce (fn [acc k] (jq k acc)) item query)
                      (map? query) (into {} (map (fn [%] [(key %) (jq (val %) item)]) query))
                      :else (get-in item [query])))]
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
