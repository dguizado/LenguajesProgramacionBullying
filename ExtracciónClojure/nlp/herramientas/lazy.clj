(ns opennlp.tools.lazy
  (:require [clojure.string :as str]))


(defn lazy-get-sentences
  [text sentence-finder]
  (lazy-seq
   (when-let [s (seq text)]
     (cons (sentence-finder (first text))
           (lazy-get-sentences (rest s) sentence-finder)))))


(defn lazy-tokenize
  [sentences tokenizer]
  (lazy-seq
   (when-let [s (seq sentences)]
     (cons (tokenizer (first s)) (lazy-tokenize (rest s) tokenizer)))))


(defn lazy-tag
  [sentences tokenizer pos-tagger]
  (lazy-seq
   (when-let [s (seq sentences)]
     (cons (pos-tagger (tokenizer (first s)))
           (lazy-tag (rest s) tokenizer pos-tagger)))))


(defn lazy-chunk
  [sentences tokenizer pos-tagger chunker]
  (lazy-seq
   (when-let [s (seq sentences)]
     (cons (chunker (pos-tagger (tokenizer (first s))))
           (lazy-chunk (rest s) tokenizer pos-tagger chunker)))))

(defn sentence-seq
  [^java.io.BufferedReader rdr sentence-finder]
  (.mark rdr 0)
  (let [sb (StringBuilder.)]
    (loop [c (.read rdr)]
      (if-not (= -1 c)
        (do (.append sb (char c))
            (let [sents (sentence-finder (str sb))]
              (if (> (count sents) 1)
                (do (.reset rdr)
                    (cons (first sents)
                          (lazy-seq (sentence-seq rdr sentence-finder))))
                (do (.mark rdr 0)
                    (recur (.read rdr))))))
        [(str/trim (str sb))]))))
