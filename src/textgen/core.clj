(ns markov.core
  (:require [clojure.java.io :as io]))

(def corpus
  (->> (io/resource "nietzsche.txt")
    slurp))

(defn split-with-delim [delim text]
  (let [tokenizer (new java.util.StringTokenizer text delim true)
        next-token (fn next-token []
                     (if (.hasMoreTokens tokenizer)
                       (cons (.nextToken tokenizer)
                             (lazy-seq (next-token)))))]
      (next-token)))

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

(defn ngrams [n tokens]
  (let [candidates (take n tokens)]
    (if (or
          (not= (count candidates) n)
          (nil? (last candidates)))
      '()
      (let [ntuple (take (- n 1) candidates)
            next-token (last candidates)
            rest (drop n tokens)]
        (cons [ntuple next-token ]
              (lazy-seq (ngrams n (cons (last ntuple) (cons next-token rest)))))))))
      

(defn insert-token [model token-pair]
  (let [[token next-token] token-pair]
    (update-in model [token next-token] incr)))

(defn build-model [text]
  (->> text
    (split-with-delim " ,.!?:;\r\n\t-_")
    (remove #(contains? #{" " "\r" "\n" "\t" "_" "-"} %))
    ;(pairs)
    (triplets)
    ;(ngrams 4)
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
    (take-next-token (rand))
    (vector)
    (concat (rest token))
    (vec)))

(defn fix-sentences [[a b & rest]]
  (cond
    (nil? a)
      '()
    (nil? b)
      (list a)
    (and (clojure.string/blank? a) (re-matches #"[.:,;!?\"']" b))
      (cons b (lazy-seq (fix-sentences rest)))
    :else
      (cons a (lazy-seq (fix-sentences (cons b rest))))))

(defn gen-text [model token length]
  (->>
      (iterate #(find-next-token model %) token)
      (map first)
      (take length)
      (interpose " ")
      (fix-sentences)
      (apply str)))

; (def model (build-model corpus))

(defn run
  ([] (let [model (build-model corpus)]
        (run model)))
  ([model]
    (print (gen-text model (rand-nth (keys model)) 100))))
  

