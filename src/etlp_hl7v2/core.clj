(ns etlp-hl7v2.core
  (:require [clojure.string :as str]
            [etlp-hl7v2.model.core :as model]
            [cheshire.core]
            ; add parse component method 
            )
  (:import [java.util.regex Pattern])
  (:gen-class))




(defn record-start? [log-line]
  (.startsWith log-line "MSH"))

(def invalid-msg? (complement record-start?))

(defn is-valid-hl7? [msg]
  (cond-> []
    (invalid-msg? msg) (conj "Message should start with MSH segment")
    (< (.length msg) 8) (conj "Message is too short (MSH truncated)")))


(defn separators [msg]
  {:segment #"(\r\n|\r|\n)"
   :field (get msg 3)
   :component (get msg 4)
   :subcomponent (get msg 7)
   :repetition (get msg 5)
   :escape (get msg 6)})

(defn split-by [s sep]
  (if (= java.util.regex.Pattern (type sep))
    (str/split s sep)
    (str/split s (re-pattern (Pattern/quote (str sep))))))

(defn indexed-map [coll]
  (loop [[x & xs] coll acc {} idx 1]
    (if (empty? xs)
      (if (nil? x) acc (assoc acc idx x))
      (recur xs
             (if (nil? x) acc (assoc acc idx x))
             (inc idx)))))

(defn not-empty? [v]
  (if (or (map? v) (sequential? v))
    (not (nil? (seq v)))
    (if (string? v)
      (not (str/blank? v))
      (not (nil? v)))))

(defn parse-value [{sch :schema sep :separators :as ctx} type value]
  (if (get model/primordials (keyword (:type type)))
    value
    (if-let [sub-type (get-in sch [:types (keyword (:type type))])]
      (let [sub-components (split-by value (:subcomponent sep))
            sub-types (:components sub-type)]
        (loop [[cmp & cmps] sub-components
               [st & sts] sub-types
               res {}]
          (let [res (if-not (str/blank? cmp)
                      (let [value (parse-value ctx st cmp)]
                        (if (not-empty? value)
                          (assoc res (keyword (:key st)) value)
                          res))
                      res)]
            (if (empty? cmps)
              res
              (recur cmps sts res)))))
      value)))

(defn parse-component [ctx tp v]
  (if (:components tp)
    (let [cmps (split-by v (get-in ctx [:separators :component]))]
      (loop [[c & cs] cmps
             [s & ss] (:components tp)
             res {}]
        (let [res (if-not (str/blank? c)
                    (let [v (parse-value ctx s c)]
                      (if (not-empty? v)
                        (assoc res (keyword (:key s)) v)
                        res))
                    res)]
          (if (empty? cs)
            res
            (recur cs ss res)))))
    v))

(defn logger [line]
  (prn line)
  line)

(defn parse-field [{sch :schema seps :separators :as ctx} {tpn :type c? :coll v :value :as f}]
  (let [tp (get-in sch [:types (keyword tpn)])
        vv (if c?
             (->> (split-by v (:repetition seps))
                  ;; (map logger)
                  (mapv #(parse-component ctx tp %))
                  (filterv not-empty?))
             (parse-component ctx tp v))]
    vv))

(defn parse-segment [{sch :schema seps :separators :as ctx} seg opts]
  (let [fields (split-by seg (:field seps))
        [seg-name & fields] fields
        fields (if (= "MSH" seg-name)
                 (into ["|"] fields)
                 fields)

        seg-sch (get-in sch [:segments (keyword seg-name)])]

    [seg-name
     (loop [[s & ss] seg-sch
            field-idx 0
            acc {}]
       (let [f (nth fields field-idx nil)
             s (merge s (get-in sch [:fields (keyword (:field s))]))]

         (if (str/blank? f)
           (let [next-acc acc #_(if (:req s)
                                  (assoc acc :__errors
                                         (conj (get acc :__errors [])
                                               (str "Field " (:name s) " is required")))
                                  acc)]

             (if (empty? ss)
               next-acc
               (recur ss (inc field-idx) next-acc)))

           (let [v (parse-field ctx (assoc (or s {}) :value f))
                 acc  (if (not-empty? v)
                        (assoc acc (keyword (:key s)) v)
                        acc)]
             (if (empty? ss)
               acc
               (recur ss (inc field-idx) acc))))))]))
