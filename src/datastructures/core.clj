(ns datastructures.core)

;;;;;;;; Singly linked list ;;;;;;;;;;
(deftype SCons [x xs])

(def SNil (SCons. nil nil))

(defn scons
  "Concat a new element to a singly linked list"
  [e l]
  (SCons. e l))

(defn shead
  "Get the first element of a singly linked list"
  [l]
  (.x l))

(defn stail
  "Get the remaining elements in the sequence"
  [l]
  (.xs l))

(defn stoclj
  "Convert a singly linked list into a vector"
  ([l]
     (stoclj l []))

  ([l z]
     (if (= l SNil)
       z
       (recur (stail l) (conj z (shead l))))))

(defn sreverse
  "Reverse a singly linked list"
  ([l] (sreverse l SNil))
  ([l s]
     (if (= l SNil)
       s
       (sreverse (stail l) (scons (shead l) s)))))

(defn sinsert
  "Insert into value v into a linked list at index i"
  ([l v i] (sinsert l v i SNil))
  ([l v i x]
     (cond
      (= l SNil)
      (sreverse x)

      (= i 0)
      (recur (stail l) v (- i 1) (scons (shead l) (scons v x)))

      :else
      (recur (stail l) v (- i 1) (scons (shead l) x)))))

(defn sdelete
  "Delete a value at index i from the linked list"
  ([l i] (sdelete l i SNil))
  ([l i x]
     (cond
      (= SNil l)
      (sreverse x)

      (= i 0)
      (recur (stail l) (- i 1)  x)

      :else
      (recur (stail l) (- i 1) (scons (shead l) x)))))
