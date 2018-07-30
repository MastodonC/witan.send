(ns witan.send.acceptance.validate-model-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :refer [join]]
            [witan.send.validate-model :refer :all]))

(deftest expected-validation-results
  (let [expected-md5s {"validation_results_count.csv" "cf6265c280c5d7d125cc6924bc89f2d8"
                       "validation_results_state.csv" "71c3dc451f8e3e42a75128f78572b57e"}
        files (keys expected-md5s)
        config-path "data/demo/config.edn"
        validation-dir (io/file (.getParent (java.io.File. config-path)) "validation")]
    (run! #(let [file (join "/" [validation-dir %])]
             (when (.exists (io/file file))
               (io/delete-file file))) files)
    (run-validation config-path)
    (is (= expected-md5s
           (into {} (for [f files]
                      [f (-> (io/file validation-dir f) (digest/md5))]))))))
