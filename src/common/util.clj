(ns common.util)


(defn mapm
  "Like map, maps map-fn over the values of input and returns a map from original keys to new values."
  [map-fn coll]
  (into {} (map (fn [[k v]] [k (map-fn v)])) coll))


(defn mapping
  "Constructs a map from (key-fn x) => (val-fn x) for each x in coll.
  Duplicate keys will overwrite previous entries."
  [key-fn val-fn coll]
  (persistent!
    (reduce (fn [m x]
              (assoc! m (key-fn x) (val-fn x)))
            (transient {})
            coll)))


(defn map-to
  "Construct a map from key => (val-fn key)."
  [val-fn keys]
  (persistent!
    (reduce (fn [m k]
              (assoc! m k (val-fn k)))
            (transient {})
            keys)))
