(ns etlp-hl7v2.model.core
  [:require [clj-yaml.core]
   [clojure.java.io :as io]])

(def primordials
  {:DT true
   :DTM true
   :FT true
   :GTS true
   :ID true
   :IS true
   :NM true
   :NUL true
   :SI true
   :ST true
   :TM true
   :TX true
   :escapeType true
   :varies true})

(def *model (atom nil))

(defn read-yaml [fob]
  (clj-yaml.core/parse-string fob :keywords true))

(defn handle-error [file inp]
  (if (nil? inp)
    (throw (Exception. (str file " not found")))
    inp))

(defn parse-yaml [file]
  (-> (io/resource file)
      ((partial handle-error file))
      (slurp)
      (read-yaml)))


(defn schema []
; TODO: make this integrant component to allow flexible decelartion of parser schema on runtime
  (if-let [shape @*model]
    shape
    (reset! *model
            {:types (parse-yaml "types.yaml")
             :segments (parse-yaml "segments.yaml")
             :messages (parse-yaml "messages.yaml")})))