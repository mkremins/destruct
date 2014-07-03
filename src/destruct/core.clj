(ns destruct.core)

(defprotocol IBindingForm
  (-bindings [this expr]))

(defn- map-bindings [m expr]
  (let [as-sym (or (:as m) (gensym))
        or-map (or (:or m) {})
        keys (for [s (:keys m)] [(symbol (name s)) (keyword (namespace s) (name s))])
        strs (for [s (:strs m)] [(symbol s) (str s)])
        syms (for [s (:syms m)] [(symbol (name s)) s])
        bpairs (concat (dissoc m :as :or :keys :strs :syms) keys strs syms)]
    (concat (-bindings as-sym expr)
            (apply concat
              (for [[k v] bpairs] (-bindings k `(get ~as-sym ~v))))
            (for [[k v] or-map] [k `(or ~k ~v)]))))

(defn- vec-bindings [v expr]
  (let [[nths specials] (split-with (complement #{'& :as}) v)
        specials (apply hash-map specials)
        as-sym (or (:as specials) (gensym))
        rest-form ('& specials)]
    (concat (-bindings as-sym expr)
            (->> nths
              (map-indexed (fn [n bform] (-bindings bform `(nth ~as-sym ~n))))
              (apply concat))
            (when rest-form
              (-bindings rest-form `(nthnext ~as-sym ~(count nths)))))))

(extend-protocol IBindingForm
  clojure.lang.Symbol
  (-bindings [this expr]
    [[this expr]])

  clojure.lang.PersistentArrayMap
  (-bindings [this expr]
    (map-bindings this expr))

  clojure.lang.PersistentHashMap
  (-bindings [this expr]
    (map-bindings this expr))

  clojure.lang.PersistentVector
  (-bindings [this expr]
    (vec-bindings this expr)))

(defmacro let' [bvec & body]
  (let [bpairs (mapcat #(apply -bindings %) (partition 2 bvec))]
    `(let* [~@(apply concat bpairs)]
       ~@body)))
