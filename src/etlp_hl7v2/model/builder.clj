(ns etlp-hl7v2.model.builder
  (:require [clojure.string :as str]))


(defn cur [inp]
  (get (:msg inp) (:pos inp)))

(defn inext [inp]
  (update inp :pos inc))

(defn seg? [x]
  (let [fl (subs x 0 1)]
    (= fl (str/upper-case fl))))

(defn name-quant [x]
  (if-let [[_ nm q] (re-find #"(.*)(\?|\*|\+)$" x)]
    [(keyword nm) (keyword q)]
    [(keyword x) nil]))


(defn build-grammar [grammar get-value rule input]
  (loop [[seg & seggs :as segments] (get grammar rule)
         input input
         output {}
         repeat true]
    (if (nil? seg)
      [input output]
      (if-let [c (cur input)]
        (let [typ (if (seg? seg) :seg :grp)
              [nm q] (name-quant seg)]
          (cond
            (= typ :seg) (if (= nm c)
                           (if (contains? #{:+ :*} q)
                             (recur segments (inext input)
                                    (update output c (fn [x] (conj (or x []) (get-value (:pos input)))))
                                    true)

                             (recur seggs (inext input)
                                    (assoc output c (get-value (:pos input)))
                                    false))
                           (cond
                             (or (= q :*) (and repeat (= q :+)))
                             (recur seggs input output false)

                             (= q :?)
                             (recur seggs input output false)
                             :else
                             [input [:error (str "Rule " rule " [" (str/join " " (get grammar rule)) "] at " seg  " expected  [" (name nm) "] got [" (name c) "] segment position " (:pos input))]]))
            (= typ :grp) (let [[inp res] (build-grammar grammar get-value nm input)]
                           (if-not (= :error (first res))
                             (if (and (contains? #{:+ :*} q)
                                      (not= (:pos input) (:pos inp)))
                               (recur segments inp (update output nm (fn [x] (conj (or x []) res))) true)
                               (recur seggs inp (assoc output nm res) false))
                             (cond
                               (or (= q :*) (and repeat (= q :+)))
                               (recur seggs input output false)

                               (= q :?)
                               (recur seggs input output false)
                               :else
                               [input res])))))
        (if (or (str/index-of seg "*") (str/index-of seg "?"))
          [input output]

          (if repeat
            [input output]
            [input [:error (str "Rule " rule " [" (str/join " " (get grammar rule)) "] at " seg  " expected " seg " at segment position " (:pos input))]]))))))

(defn build [grammar msg get-value opts]
  (let [[inp res] (build-grammar grammar (or get-value identity) :msg {:msg msg :pos 0})]
    (if (= :error (first res))
      res
      (if (= (:pos inp) (count (:msg inp)))
        res
        (if (get opts :strict? true)
          [:error (str "Extra input [" (name (cur inp)) "] pos: " (:pos inp))]
          res)))))

