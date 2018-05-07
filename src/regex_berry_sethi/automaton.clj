(ns regex-berry-sethi.automaton
  (:require [regex-berry-sethi.regex-tree :as regex-tree]
            [clojure.string :as str])
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

(defn not-empty?
  [coll]
  (not (empty? coll)))

(defn accepts?
  "returns whether or not the given word is accepted by the automaton"
  [automaton word]
  (let [word-final-states (reduce (fn [states char] (mapcat #(transition automaton % char) states))
                                  [:init]
                                  word)]

    ;; check if any state we landed is an accepting state of this automaton
    (not-empty? (some (into #{} word-final-states)
                      (accepting-states automaton)))))

;; Graphviz
;;---------
(def init-state-id -1)

(defn state-ids
  "maps states to ids"
  [automaton]
  (zipmap (concat '(:init) (states automaton)) (iterate inc' init-state-id)))

(defn stringify
  [coll leaf-ids]
  (str "\"{" (str/join "," (sort (map (fn [x] (leaf-ids x)) coll))) "}\""))

(defn graphviz
  "generates graphviz representation of the dfa constructed while consuming given word"
  [automaton word]
  ;; init work
  (let [leaf-ids (state-ids automaton)
        traversed-states (->> (reduce (fn [states-till-char char]
                                        (cons (mapcat #(transition automaton % char)
                                                      (first states-till-char))
                                              states-till-char))
                                      '((:init))
                                      word)
                               reverse)
        ;; check if any element from traversed-states element is in accepting states, then the whole transition-state is
        power-set-accept-states (->> (concat traversed-states)
                                     (filter (fn [transition-state]
                                               (not-empty? (filter (fn [state] (some? (some #{state} (accepting-states automaton)))) transition-state))))
                                     ;; sort inner power-set tuple by ids
                                     (map (fn [coll] (sort-by (fn [x] (leaf-ids x)) coll)))
                                     ;; uniques only
                                     (into #{}))
        final-states (str "node [shape=doublecircle];\n"
                          (->> power-set-accept-states
                               (map (fn [states] (stringify states leaf-ids)))
                               (str/join ";"))
                          ";\n")
        transitions (str "node [shape=circle];\n"
                         ;; [i [from to]] pairs
                         (->> (map vector (range) (partition 2 1 traversed-states))
                              (mapcat #(let [i (first %)
                                             [[a b]] (rest %)
                                             from (stringify a leaf-ids)
                                             to   (stringify b leaf-ids)]
                                         (str from " -> " to "[label=" (nth word i) "];" "\n")))
                              (apply str)))
        initial-state (str "start -> \"{" init-state-id "}\";\n")]
    (str "digraph dfa {\nrankdir=LR;\nsize=\"8,5\"\nnode [shape=none,width=0,height=0,margin=0]; start [label=\"\"];\n"
         final-states
         transitions
         initial-state
        "}")))