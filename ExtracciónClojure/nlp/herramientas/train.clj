(ns opennlp.tools.train
  (:use [clojure.java.io :only [reader file input-stream output-stream]])
  (:import (opennlp.tools.util PlainTextByLineStream 
                               TrainingParameters
                               MarkableFileInputStreamFactory)
           (opennlp.tools.util.model BaseModel 
                                     ModelType)
           (opennlp.tools.dictionary Dictionary)
           (opennlp.tools.tokenize TokenizerME
                                   TokenizerModel
                                   TokenSampleStream
                                   TokenizerFactory)
           (opennlp.tools.sentdetect SentenceDetectorME
                                     SentenceModel
                                     SentenceSampleStream
                                     SentenceDetectorFactory)
           (opennlp.tools.namefind NameSampleDataStream
                                   NameFinderME
                                   TokenNameFinderModel
                                   TokenNameFinderFactory
                                   BioCodec)
           (opennlp.tools.chunker ChunkerME 
                                  ChunkSampleStream 
                                  ChunkerModel
                                  ChunkerFactory)
           (opennlp.tools.parser ParseSampleStream 
                                 ParserModel)
           (opennlp.tools.parser.lang.en HeadRules)
           (opennlp.tools.parser.chunking Parser)
           (opennlp.tools.postag POSTaggerME
                                 POSModel
                                 POSDictionary
                                 WordTagSampleStream
                                 POSTaggerFactory)
           (opennlp.tools.doccat DoccatModel
                                 DocumentCategorizerME
                                 DocumentSampleStream
                                 DoccatFactory)))

(defn write-model
  [^BaseModel model out-stream]
  (with-open [out (output-stream out-stream)]
    (.serialize model out)))

(defn build-dictionary
  [in]
  (with-open [rdr (reader in)]
    (Dictionary/parseOneEntryPerLine rdr)))

(defn build-posdictionary
  [in]
  (with-open [is (input-stream in)]
    (POSDictionary/create is)))

(defn ^ChunkerModel train-treebank-chunker
  ([in] (train-treebank-chunker "en" in))
  ([lang in] (train-treebank-chunker lang in 100 5))
  ([lang in iter cut]
    (ChunkerME/train
      lang
      (ChunkSampleStream.
        (PlainTextByLineStream. 
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
      (doto (TrainingParameters.)
        (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
        (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut)))
      (ChunkerFactory.))))

(defn ^ParserModel train-treebank-parser
  ([in headrules] (train-treebank-parser "en" in headrules))
  ([lang in headrules] (train-treebank-parser lang in headrules 100 5))
  ([lang in headrules iter cut]
     (with-open [rdr (reader headrules)]
       (Parser/train
        lang
        (ParseSampleStream.
         (PlainTextByLineStream.
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
        (HeadRules. rdr) 
        (doto (TrainingParameters.)
          (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
          (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut)))))))


(defn ^TokenNameFinderModel train-name-finder
  ([in] (train-name-finder "en" in))
  ([lang in] (train-name-finder lang in 100 5))
  ([lang in iter cut & {:keys [entity-type feature-gen classifier]
                        :or  {entity-type "default" classifier "MAXENT"}}]
    
    (NameFinderME/train
      lang
      entity-type
      (NameSampleDataStream.
        (PlainTextByLineStream.
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
      (doto (TrainingParameters.)
        (.put TrainingParameters/ALGORITHM_PARAM classifier)
        (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
        (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut)))
      (TokenNameFinderFactory. 
        feature-gen {} (BioCodec.)))))

(defn ^TokenizerModel train-tokenizer
  ([in] (train-tokenizer "en" in))
  ([lang in] (train-tokenizer lang in 100 5))
  ([lang in iter cut]
    (TokenizerME/train
      (TokenSampleStream.
        (PlainTextByLineStream.
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
      (TokenizerFactory. 
        lang nil false nil)
      (doto (TrainingParameters.)
        (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
        (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut))))))

(defn ^POSModel train-pos-tagger
  ([in] (train-pos-tagger "en" in))
  ([lang in] (train-pos-tagger lang in nil))
  ([lang in tagdict] (train-pos-tagger lang in tagdict 100 5))
  ([lang in tagdict iter cut]
    (POSTaggerME/train
      lang
      (WordTagSampleStream. 
        (PlainTextByLineStream.
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
      (doto (TrainingParameters.)
        (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
        (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut)))
      (POSTaggerFactory. nil tagdict))))

(defn ^SentenceModel train-sentence-detector
  ([in] (train-sentence-detector "en" in))
  ([lang in]
    (SentenceDetectorME/train 
      lang
      (SentenceSampleStream. 
        (PlainTextByLineStream.
          (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
      (SentenceDetectorFactory. lang true nil nil)
      (TrainingParameters.))))

(defn ^DoccatModel train-document-categorization
  ([in] (train-document-categorization "en" in 1 100))
  ([lang in] (train-document-categorization lang in 1 100))
  ([lang in cut] (train-document-categorization lang in cut 100))
  ([lang in cut iter]
       (DocumentCategorizerME/train 
         lang
         (DocumentSampleStream.
           (PlainTextByLineStream.
             (MarkableFileInputStreamFactory. (file in)) "UTF-8"))
         (doto (TrainingParameters.)
           (.put TrainingParameters/ITERATIONS_PARAM (Integer/toString iter))
           (.put TrainingParameters/CUTOFF_PARAM     (Integer/toString cut)))
         (DoccatFactory.))))
                                   
