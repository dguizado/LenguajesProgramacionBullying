(ns opennlp.tools.filters)

(defmacro pos-filter
  [n r]
  (let [docstring (str "Given a list of pos-tagged elements, "
                       "return only the " n " in a list.")]
    `(defn ~n
       ~docstring
       [elements#]
       (filter (fn [t#] (re-find ~r (second t#))) elements#))))

(defmacro chunk-filter
  [n r]
  (let [docstring (str "Given a list of treebank-chunked elements, "
                       "return only the " n " in a list.")]
    `(defn ~n
       ~docstring
       [elements#]
       (filter (fn [t#] (if (nil? ~r)
                          (nil? (:tag t#))
                          (and (:tag t#)
                               (re-find ~r (:tag t#)))))
               elements#))))

(pos-filter nouns #"^NN")
(pos-filter nouns-and-verbs #"^(NN|VB)")
(pos-filter proper-nouns #"^NNP")
(pos-filter verbs #"^VB")

(chunk-filter verb-phrases #"^VP$")
(chunk-filter noun-phrases #"^NP$")
(chunk-filter adverb-phrases #"^ADVP$")
(chunk-filter adjective-phrases #"^ADJP$")

(chunk-filter nil-phrases nil)
