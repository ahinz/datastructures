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

(defn supdate
  "Update a index i to value v"
  ([l v i] (supdate l v i SNil))
  ([l v i x]
     (cond
      (= l SNil)
      (sreverse x)

      (= i 0)
      (recur (stail l) v (- i 1) (scons v x))

      :else
      (recur (stail l) v (- i 1) (scons (shead l) x)))))

(defn sindex
  "Get the index of a value or -1 if it doesn't exist"
  ([l v] (sindex l v identity))
  ([l v f] (sindex l v 0 f))
  ([l v i f]
     (cond
      (or (= nil l) (= SNil l))
      -1

      (= (f (shead l)) v)
      i

      :else
      (recur (stail l) v (+ i 1) f))))

(defn sget
  "Get the value at a given index"
  [l i]
  (cond
   (or (= l nil) (= l SNil) (< i 0))
   nil

   (= i 0)
   (shead l)

   :else
   (sget (stail l) (- i 1))))

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
