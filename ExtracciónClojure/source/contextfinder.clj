(ns contextfinder
  (:use [clojure.pprint :only [pprint]]
        [opennlp.nlp]
        [opennlp.tools.filters]))


(def get-sentences (make-sentence-detector "models/en-sent.bin"))
(def tokenize (make-tokenizer "models/en-token.bin"))
(def pos-tag (make-pos-tagger "models/en-pos-maxent.bin"))



(defn- mindist
  [n ns]
  (apply min (map #(Math/abs (- n %)) ns)))


(defn- score-word
  [iword iterms base]
  (let [dist (mindist (first iword) iterms)
        score (if (zero? dist)
                base
                (/ base (* 2 dist)))]
    (if (> dist 2) 0 score)))


(defn score-words
  ([term words] (score-words term words 1))
  ([term words base]
     (let [iwords (map vector (iterate inc 0) words)
           iterms (map first (filter (fn [e] (= (second e) term)) iwords))]
       (if (= 0 (count iterms))
         (map #(vector % 0) words)
         (map #(vector (second %) (score-word % iterms base)) iwords)))))


(defn nv-filter
  [tagged-sentence]
  (filter #(>= (count (first %)) 3) (nouns-and-verbs tagged-sentence)))


(defn contains-token?
  [sentence term]
  (let [tokens (tokenize sentence)]
    (boolean (some #{term} tokens))))


(defn get-matching-sentences
  [sentences term]
  (filter #(contains-token? % term) sentences))


(defn get-tagged-sentences
  [sentences]
  (map #(pos-tag (tokenize %)) sentences))


(defn get-weighted-sentences
  [tagged-sentences term]
  (map #(score-words term (map first (nv-filter %))) tagged-sentences))


(defn get-new-terms
  [weighted-sentences]
  (into {}
        (reduce conj
                (reduce conj
                        (map #(filter (fn [pair] (not= 0 (second pair))) %)
                             weighted-sentences)))))


(defn get-scored-terms
  [text term]
  (let [sentences (get-sentences text)
        matched-sentences (get-matching-sentences sentences term)
        tagged-sentences (get-tagged-sentences matched-sentences)
        weighted-sentences (get-weighted-sentences tagged-sentences term)
        new-terms (get-new-terms weighted-sentences)]
    new-terms))


(defn score-sentence
  [sentence score-words]
  (let [tokens (tokenize sentence)]
    (reduce + (map #(get score-words % 0) tokens))))


(defn score-sentences
  [text score-words]
  (let [sentences (get-sentences text)]
    (for [s sentences]
      [s (score-sentence s score-words)])))


(defn score-text
  [text score-words]
  (let [sentences (get-sentences text)]
    (reduce + (map #(score-sentence % score-words) sentences))))

(def mytext "Politechicos.py")

(def scorewords (get-scored-terms mytext "palabras ofensivas/insultos"))

(pprint (reverse (sort-by second (score-sentences mytext scorewords))))

(println (score-text mytext scorewords))
