(ns etlp-hl7v2.core
  (:require [cheshire.core] ; add parse component method 
            [clojure.string :as str]
            [etlp-hl7v2.model.core :as model]
            [etlp-hl7v2.model.builder :as model-builder]
            [flatland.ordered.map :refer [ordered-map]])
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
  (clojure.pprint/pprint line)
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

;; FIX: grammar desc is a list?
;; if after is nil = conj
(defn append [after coll x]
  (if after
    (let [pattern (re-pattern (str after ".?"))]
      (flatten (mapv #(if (re-find pattern %) [% x] %) coll)))
    (if (vector? coll)
      (conj coll x)
      (seq (conj (vec coll) x)))))

;; extension proposal
;; [:ADT_A01 :ZBC [[:name "ZBC.1" :type ST :key "zbc1"]]]
;; [:ADT_A01 :ZBC [[:name "ZBC.1" :type ST :key "zbc1"]] {:after #"PID"}]
;; TODO: support "put after", not only append
(defn apply-extension [schema [grammar segment-name segment-desc {after :after quant :quant}]]
  (let [[grammar rule] (if (sequential? grammar) grammar [grammar :msg])
        rule (or rule :msg)

        messages-path (conj [:messages] grammar rule)
        desc (map #(apply ordered-map %) segment-desc)]
    (-> schema
        (update-in messages-path (partial append after) (str (name segment-name) (or quant "?")))
        (assoc-in [:segments segment-name] desc))))


(defn parse-only
  ([msg {extensions :extensions :as opts}]
   (let [errs (is-valid-hl7? msg)]
     (when-not (empty? errs)
       (throw (Exception. (str/join "; " errs))))
     (let [sch (model/schema)
           sch (reduce apply-extension sch extensions)
           seps (separators msg)
           ctx {:separators seps
                :schema sch}

           segments (->> (split-by msg (:segment seps))
                         (mapv str/trim)
                         (filter #(> (.length %) 0))
                         (mapv #(parse-segment ctx % opts)))

           errors (mapcat #(get (second %) :__errors []) segments)]

       (if (and (not (nil? (seq errors))) (get opts :strict? true))
         [:error (pr-str errors)]
         segments)))))

(defn structurize-only [segments options]
  (let [sch (model/schema)
        sch (reduce apply-extension sch (:extensions options))
        {c :code e :event} (get-in segments [0 1 :type])
        msg-key (get-in sch [:messages :idx (keyword c) (keyword e)])
        grammar (get-in sch [:messages (when [msg-key] (keyword msg-key))])]

    (when-not grammar
      (throw (Exception. (str "Do not know how to parse: " c "|" e " " (first segments)))))

    (model-builder/build grammar (mapv #(keyword (first %)) segments) (fn [idx] (get-in segments [idx 1])) options)))

(defn parse
  ([msg] (parse msg {}))
  ([msg opts]
   (let [segments (parse-only msg opts)]
     (if (= :error (first segments))
       segments
       (structurize-only segments opts)))))


(defn get-segments
  ([ctx {extensions :extensions :as opts} msg]
   (let [sch (model/schema)
         sch (reduce apply-extension sch extensions)
         seps (:separators ctx)
         ctx! {:separators seps
               :schema sch}

         segments (->> msg
                       (mapv str/trim)
                       (filter #(> (.length %) 0))
                       (mapv #(parse-segment ctx! % opts)))

         errors (mapcat #(get (second %) :__errors []) segments)]

     (if (and (not (nil? (seq errors))) (get {} :strict? true))
       [:error (pr-str errors)]
       segments))))

(defn next-log-record [ctx hl7-lines]
  (let [head (first hl7-lines)
        body (take-while (complement record-start?) (rest hl7-lines))]
    (remove nil? (conj body head))))


(defn hl7-xform
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.  Returns a stateful
  transducer when no collection is provided."
  ([ctx]
   (fn [rf]
     (let [a (java.util.ArrayList.)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                             ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (.add a input)
          (if (and (> (count a) 1) (= true (record-start? input)))
            (let [v (vec (.toArray a))]
              (.clear a)
              (.add a (last v))
              (rf result (drop-last v)))
            result))))))

  ([ctx log-lines]
   (lazy-seq
    (when-let [s (seq log-lines)]
      (let [record (doall (next-log-record ctx s))]
        (cons record
              (hl7-xform ctx (nthrest s (count record)))))))))

(defn parse-with-opts [opts segmts]
  (structurize-only segmts opts))

(defn transduce-hl7-stream [ctx opts]
  (comp
   (hl7-xform ctx)
   (map (partial get-segments ctx opts))
   (map (partial parse-with-opts opts))))
