(ns markov.core
  (:require [clojure.java.io :as io]))

(def corpus
  (->> (io/resource "nietzsche.txt")
    slurp))

(defn split-with-delim 
  ([delim text]
    (split-with-delim (new java.util.StringTokenizer text delim true)))
  ([tokenizer]
    (if (.hasMoreTokens tokenizer)
         (cons (.nextToken tokenizer)
               (lazy-seq (split-with-delim tokenizer))))))

(defn incr [v]
  (if (number? v) (inc v) 1))

(defn pairs
  ([] '())
  ([[a b & rest]]
    (if (nil? b)
      '()
      (cons [a b] (lazy-seq (pairs (cons b rest)))))))

(defn triplets
  ([] '())
  ([[a b c & rest]]
    (if (nil? c)
      '()
      (cons [[a b] c]
            (lazy-seq (triplets (cons b (cons c rest))))))))

(defn insert-token [model token-pair]
  (let [[token next-token] token-pair]
    (update-in model [token next-token] incr)))

(defn build-model [text]
  (->> text
    (split-with-delim " ,.!?:;\r\n\t-_")
    (remove #(contains? #{" " "\r" "\n" "\t" "_" "-"} %))
    ;(pairs)
    (triplets)
    (reduce insert-token {})))

(defn get-next-tokens [model token]
  (let [branches         (get model token)
        branch-sum       (apply + (vals branches))
        calc-percentage  (fn [[token value]]
                           [token (/ value branch-sum)])]
    (map calc-percentage branches)))

(defn take-next-token [max branch-chances]
  (loop [sum 0
         [entry & rest] branch-chances]
    (let [token (first entry)
          value (or (last entry) 0)
          new-sum (+ sum value)]
      (if (or (nil? rest) (>= new-sum max))
        token
        (recur new-sum rest)))))

(defn find-next-token [model token]
  (->>
    (get-next-tokens model token)
    (take-next-token (rand))))

(defn gen-text [model token length]
  (->>
      (iterate #(find-next-token model %) token)
      (take length)
      (interpose " ")
      (apply str)))

(defn run [firstword]
  (let [model (build-model corpus)]
    (print (gen-text model firstword 100))))

