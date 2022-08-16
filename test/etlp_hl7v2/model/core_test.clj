(ns etlp-hl7v2.model.core-test
  (:require [clojure.test :refer :all]
            [etlp-hl7v2.model.core :refer [parse-yaml schema]]
            [flatland.ordered.map :refer [ordered-map]]))


(deftest file-not-found-test
  (testing "parse file should raise exception if file not found"
    (is (thrown? Exception (parse-yaml "404.yaml")))))


(deftest file-parsed-into-map
  (testing "valid resource should return flatland ordered map "
    (is  (=  (type (ordered-map [])) (type (parse-yaml "seg2.yaml"))))))

(deftest parser-model-map
  (testing "method schema should return a map with keys type, segments, messages"
    (let [model (schema) schme [:types :segments :messages]]
      (is (nil? (assert (every? model schme)))))))
