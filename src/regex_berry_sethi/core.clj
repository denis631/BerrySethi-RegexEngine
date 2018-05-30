(ns regex-berry-sethi.core
  (:require [regex-berry-sethi.regex-parser :as parser]
            [regex-berry-sethi.automaton :as automaton])
  (:gen-class))

(defn -main  [& args]
  (when-let [tree (parser/parse (parser/scan "(a|b)*a(a|b)"))]
    (do
      (println (automaton/accepts? tree "abab"))
      (println (automaton/graphviz tree "ababb")))))
