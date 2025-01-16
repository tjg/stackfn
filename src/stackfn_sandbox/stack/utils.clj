(ns stackfn-sandbox.stack.utils
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.zip :as z]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defn trim-assign-from-varname [s]
  (when-not (and s (str/ends-with? s "+"))
    (throw (ex-info "Malformed varname assignment"
                    {:form s
                     :validation-type :syntax})))
  (subs s 0 (dec (count s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stacks

(defn peek-multiple [n coll]
  (loop [counter n
         coll coll
         acc []]
    (if (>= 0 counter)
      acc
      (recur (dec counter)
             (pop coll)
             (conj acc (peek coll))))))

(defn pop-multiple [n coll]
  (nth (iterate pop coll) n))

(defn unforgiving-peek
  "Like peek, but results in a semantic error if `coll` is empty."
  [coll]
  (if (empty? coll)
    (throw (ex-info "Can't peek empty stack."
                    {:form coll
                     :validation-type :semantics}))
    (peek coll)))

(defn forgiving-pop
  "Like pop, but returns `coll` unmodified if empty."
  [coll]
  (if (empty? coll)
    coll
    (pop coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Zippers

(defn alter-loc
  "Apply loc-fn to each loc in the tree."
  [loc-fn loc]
  (loop [loc loc]
    (if (z/end? loc)
      loc
      (-> loc loc-fn z/next recur))))

(defn iter-zip
  "Sequence of locs in the tree."
  [loc]
  (->> loc
       (iterate z/next)
       (take-while (complement z/end?))))

(defn down-rightmost
  "Like clojure.zip/down, but gets the rightmost child (not the leftmost)."
  [loc]
  ;; z/down can lead to nil; z/rightmost won't, unless z/down does.
  ;;
  ;; z/rightmost won't lead to an error when receiving nil, but I'm
  ;; unsure whether this is implementation-dependent.
  (some-> loc z/down z/rightmost))

(defn first-descendant
  "First leaf node under loc. (Or loc itself.)"
  [loc]
  (->> loc
       (iterate z/down)
       (take-while (complement nil?))
       last))

(defn last-descendant
  "The last leaf node under loc, or self."
  [loc]
  (->> loc
       (iterate down-rightmost)
       (take-while (complement nil?))
       last))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(defn normalize-decl [paramlist-and-body+]
  (if (vector? (first paramlist-and-body+))
    (list paramlist-and-body+)
    paramlist-and-body+))
