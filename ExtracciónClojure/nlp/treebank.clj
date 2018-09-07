(ns opennlp.treebank
  (:use [clojure.java.io :only [input-stream]])
  (:require [clojure.string :as str]
            [instaparse.core :as insta])
  (:import (java.util List)
           (opennlp.tools.chunker ChunkerModel ChunkerME)
           (opennlp.tools.cmdline.parser ParserTool)
           (opennlp.tools.parser Parse ParserModel
                                 ParserFactory AbstractBottomUpParser)
           (opennlp.tools.parser.chunking Parser)
           (opennlp.tools.util Span)))


(def ^:dynamic *advance-percentage* 0.95)

(def ^:dynamic *beam-size* 3)

(defn- split-chunks
  [chunks]
  (let [seqnum    (atom 0)
        splitfunc (fn
                    [^String item]
                    (if (.startsWith item "B-")
                      (swap! seqnum inc)
                      @seqnum))]
    (partition-by splitfunc (pop chunks))))


(defn- size-chunk
  [tb-chunk]
  (let [chunk-type  (second (re-find #"B-(.*)" (first tb-chunk)))
        chunk-count (count tb-chunk)]
    [chunk-type chunk-count]))


(defn- split-with-size
  [[v & vs] s]
  (if-not v
    (list s)
    (cons (take v s) (split-with-size vs (drop v s)))))


(defn- de-interleave
  [s]
  [(map first s) (map last s)])


(defstruct treebank-phrase :phrase :tag)

(defmulti make-treebank-chunker
  class)

(defmethod make-treebank-chunker :default
  [modelfile]
  (with-open [modelstream (input-stream modelfile)]
    (make-treebank-chunker (ChunkerModel. modelstream))))

(defmethod make-treebank-chunker ChunkerModel
  [^ChunkerModel model]
  (fn treebank-chunker
    [pos-tagged-tokens]
    (let [chunker (ChunkerME. model)
          [tokens tags] (de-interleave pos-tagged-tokens)
          chunks  (into [] (seq (.chunk chunker 
                                  (into-array ^List tokens) 
                                  (into-array ^List tags))))
          sized-chunks (map size-chunk (split-chunks chunks))
          [types sizes] (de-interleave sized-chunks)
          token-chunks (split-with-size sizes tokens)
          probs (seq (.probs chunker))]
      (with-meta
        (map #(struct treebank-phrase (into [] (last %)) (first %))
             (partition 2 (interleave types token-chunks)))
        {:probabilities probs}))))

(defn phrases
  [phrases]
  (map :phrase phrases))

(defn phrase-strings
  [phrase-chunks]
  (map #(apply str (interpose " " %)) (phrases phrase-chunks)))


(defn- strip-parens
  [s]
  (-> s
      (str/replace "\\(" "-LRB-")
      (str/replace "\\)" "-RRB-")
      (str/replace "\\{" "-LCB-")
      (str/replace "\\}" "-RCB-")))


(defn- parse-line
  [line parser]
  (let [line (strip-parens line)
        results (StringBuffer.)
        parse-num 1]
    (.show ^Parse (first (ParserTool/parseLine line parser parse-num)) results)
    (str results)))


(defmulti make-treebank-parser
  class)

(defmethod make-treebank-parser :default
  [modelfile]
  (with-open [modelstream (input-stream modelfile)]
    (make-treebank-parser (ParserModel. modelstream))))

(defmethod make-treebank-parser ParserModel
  [model]
  (fn treebank-parser
    [text]
    (let [parser (ParserFactory/create model
                                       *beam-size*
                                       *advance-percentage*)
          parses (map #(parse-line % parser) text)]
      (vec parses))))

(def ^:private s-parser
  (insta/parser
   "E = <'('> V <WS> (ch | (E <WS?>)+) <')'> <WS?> ; m = #'[^)\\s]+' ; WS = #'\\s+'"))

(defn- tr
  [ptree & [tag-fn]]
  (let [t (or tag-fn symbol)]
    (if (= :E (first ptree))
      {:tag 
      (t (second (second ptree))) :chunk (map #(tr % tag-fn) (drop 2 ptree))}
      (second ptree))))

(defn make-tree
  [tree-text & [tag-fn]]
  (tr (s-parser tree-text) tag-fn))

