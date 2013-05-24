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

;;;;;;;; Open Addressing Hash Table ;;;;;;;;;;

(defn ohmake
  "Create a new (mutable) open-address hash table"
  ([] (ohmake 10))
  ([n] (object-array n)))

(defn ohinsert
  "Insert a k,v pair into the hashmap"
  [h k v]
  (let [hashed-k (mod k (alength h))
        existing (aget h hashed-k)]
    (if (= nil existing)
      (aset h hashed-k (scons (scons k v) SNil))
      (let [i (sindex existing k shead)]
        (aset h hashed-k
              (if (= -1 i)
                (scons (scons k v) existing)
                (supdate existing (scons k v) i))))))
  h)

(defn ohcontains
  "Determine if a given key is in a hashmap"
  [h k]
  (let [hashed-k (mod k (alength h))
        lst (aget h hashed-k)]
    (not= -1 (sindex lst k shead))))

(defn ohget
  "Get a key from the hash. If the key doesn't exist return nil. Note that
   it is allowable to store nils so if you do store nils in the hash table
   you should use ohcontains to determine if the hash contains this key"
  [h k]
  (let [hashed-k (mod k (alength h))
        lst (aget h hashed-k)
        i (sindex lst k shead)]
    (if (= -1 i)
      nil
      (stail (sget lst i)))))

(defn ohtoclj
  "Convert open-addressing hash to clojure map"
  [h]
  (reduce
   (fn [hh slist]
     (loop [l slist
            hh hh]
       (if (= l SNil)
         hh
         (recur (stail l) (assoc hh (shead (shead l)) (stail (shead l)))))))
   {} (filter #(not (nil? %)) h)))

;;;;;;;; Graphs ;;;;;;;;;;

(definterface INode
  (getVisited [])
  (setVisited [r]))

(deftype Node
    [value ^:volatile-mutable visited]
  INode
  (getVisited [_] visited)
  (setVisited [this v] (set! visited v)))

(defn node [n] (Node. n false))

(deftype Graph [node edges])

(defn connected
  "Get a list of nodes that are contected to the given node"
  [g node]
  (map
   second
   (filter (fn [[n1 _]] (= n1 node)) (.edges g))))

(defn bfs
  "Breadth first search of the given graph"
  ([g] (bfs g [(.node g)] (list)))
  ([g qu rs]
     (if (empty? qu)
       (reverse (map #(.value %) rs))
       (recur
        g
        (concat (rest qu) (connected g (first qu)))
        (conj rs (first qu))))))

(defn dfs
  "Depth first search of the given graph"
  ([g] (dfs g [(.node g)] (list)))
  ([g qu rs]
     (if (empty? qu)
       (reverse (map #(.value %) rs))
       (recur
        g
        (concat (connected g (first qu)) (rest qu))
        (conj rs (first qu))))))

(def n1 (node 1))
(def n2 (node 2))
(def n3 (node 3))
(def n4 (node 4))
(def n5 (node 5))
(def n6 (node 6))
(def n7 (node 7))

(def g1 (Graph. n1 [[n1 n2] [n1 n3] [n2 n4] [n4 n5] [n2 n6] [n5 n7]]))
