(ns regex-berry-sethi.core
  (:require [regex-berry-sethi.regex-tree :as regex-tree]
            [regex-berry-sethi.automaton :as automaton])
  (:gen-class))

(def tree (regex-tree/concat
            (regex-tree/star
              (regex-tree/or (regex-tree/letter \a) (regex-tree/letter \b)))
            (regex-tree/concat
              (regex-tree/letter \a)
              (regex-tree/or (regex-tree/letter \a) (regex-tree/letter \b)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (automaton/accepts? tree "abab")))