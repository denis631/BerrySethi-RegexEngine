(ns regex-berry-sethi.regex-tree
  (:gen-class))

;; empty
(defmulti empty? ::regex-tree)
(defmethod empty? ::epsilon [_] true)
(defmethod empty? ::letter [_] false)
(defmethod empty? ::or [o] (or (empty? (:left o)) (empty? (:right o))))
(defmethod empty? ::star [_] true)
(defmethod empty? ::concat [c] (and (empty? (:left c)) (empty? (:right c))))

;; first
(defmulti first ::regex-tree)
(defmethod first ::epsilon [_] [])
(defmethod first ::letter [l] [l])
(defmethod first ::or [o] (concat (first (:left o)) (first (:right o))))
(defmethod first ::star [s] (first (:r s)))
(defmethod first ::concat [c] (if (empty? (:left c))
                                (concat (first (:left c)) (first (:right c)))
                                (first (:left c))))

;; next
(defn next
  ([root node] (next root node []))
  ([root node acc]
   (if (identical? root node)
     acc
     (case (::regex-tree root)
       ::or (concat (next (:left root) node acc)
                    (next (:right root) node acc))
       ::star (next (:r root) node (concat acc (first (:r root))))
       ::concat (concat (next (:left root) node (if (empty? (:right root))
                                                  (concat acc (first (:right root)))
                                                  (first (:right root))))
                        (next (:right root) node acc))
       ;; empty vector on no match
       []))))

;; last
(defmulti last ::regex-tree)
(defmethod last ::epsilon [_] [])
(defmethod last ::letter [l] [l])
(defmethod last ::or [o] (concat (last (:left o)) (last (:right o))))
(defmethod last ::star [s] (last (:r s)))
(defmethod last ::concat [c] (if (empty? (:right c))
                                (concat (last (:left c)) (last (:right c)))
                                (last (:right c))))

(defn uuid [] (str (java.util.UUID/randomUUID)))

;; constructors
(defn epsilon [] {::regex-tree ::epsilon ::id (uuid)})
(defn letter [l] {::regex-tree ::letter ::id (uuid) :letter l})
(defn or [l r] {::regex-tree ::or ::id (uuid) :left l :right r})
(defn star [s] {::regex-tree ::star ::id (uuid) :r s})
(defn concat [l r] {::regex-tree ::concat ::id (uuid) :left l :right r})