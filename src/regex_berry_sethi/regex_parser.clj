(ns regex-berry-sethi.regex-parser
  (:require [regex-berry-sethi.regex-tree :as regex-tree])
  (:gen-class))

;; Recursive descent for regex
;; ---------------------------
(declare parse-regex)

(defn scan
  [s]
  (seq s))

(defn parse
  [tokens]
  (parse-regex tokens))

;; Grammar for regex. With operator precedence
;; <regex> ::= <term> '|' <regex>
;;          |  <term>
;;
;;<term> ::= <factor> <term> | eps
;;
;;<factor> ::= <base> | <base> '*'
;;
;;<base> ::= <char>
;;         | '(' <regex> ')'

(defn index-of-next-close-bracket
  [tokens]
  (loop [tokens tokens i 0 bracket-count -1]
    (when-not (empty? tokens)
      (case (first tokens)
        \) (if (= bracket-count 0)
             (inc i)
             (recur (rest tokens) (inc i) (dec bracket-count)))
        \( (recur (rest tokens) (inc i) (inc bracket-count))
        (recur (rest tokens) (inc i) bracket-count)))))

(defn- parse-base
  [tokens]
  (case (first tokens)
    \( (let [idx (index-of-next-close-bracket tokens)
             [regex-tokens rest-tokens] (split-at idx tokens)
             regex-tokens-without-brackets (rest (drop-last regex-tokens))]
         [rest-tokens (parse-regex regex-tokens-without-brackets)])
    \ε [(rest tokens) (regex-tree/epsilon)]
    (when (and (not (nil? (first tokens)))
               (Character/isLetter ^char (first tokens)))
      [(rest tokens) (regex-tree/letter (first tokens))])))

(defn- parse-factor
  [tokens]
  (when-let [[rest-tokens tree] (parse-base tokens)]
    (if (= \* (first rest-tokens))
      [(next rest-tokens) (regex-tree/star tree)]
      [rest-tokens tree])))

(defn- parse-term
  [tokens]
  (when-let [[rest-tokens tree] (parse-factor tokens)]
    (if-let [[rest-tokens rest-tree] (parse-term rest-tokens)]
      [rest-tokens (regex-tree/concat tree rest-tree)]
      [rest-tokens tree])))

(defn- parse-regex
  [tokens]
  (when-let [[rest-tokens tree] (parse-term tokens)]
    (case (first rest-tokens)
      \| (regex-tree/or tree (parse-regex (rest rest-tokens)))
      nil tree
      nil)))