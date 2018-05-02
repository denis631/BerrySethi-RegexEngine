(ns regex-berry-sethi.automaton
  (:require [regex-berry-sethi.regex-tree :as regex-tree])
  (:gen-class))

(defmulti states ::regex-tree/regex-tree)
(defmethod states ::regex-tree/epsilon [_] [])
(defmethod states ::regex-tree/letter [l] [l])
(defmethod states ::regex-tree/or [o] (concat (states (:left o)) (states (:right o))))
(defmethod states ::regex-tree/star [s] (states (:r s)))
(defmethod states ::regex-tree/concat [c] (concat (states (:left c)) (states (:right c))))

(defn accepting-states
  [automaton]
  (let [accepting-states (into [] (regex-tree/last automaton))]
    (if (regex-tree/empty? automaton)
      (conj accepting-states :init)
      accepting-states)))

(defn transition
  [automaton from-state action]
  (if (= from-state :init)
    (filter (fn [to-state] (= (:letter to-state) action)) (regex-tree/first automaton))
    (filter (fn [to-state] (= (:letter to-state) action)) (regex-tree/next automaton from-state))))

(defn accepts?
  [automaton word]
  (let [word-finite-states (reduce (fn [states char]
                                     (mapcat #(transition automaton % char) states))
                                   [:init]
                                   word)]

    ;; some? (some #{%} coll) syntax is crazy, but does what contains? would do
    (some true? (map #(and (some? (some #{%} (accepting-states automaton)))
                           ;; since I have no IDs, I look also for next to be empty
                           (empty? (regex-tree/next automaton %))) word-finite-states))))