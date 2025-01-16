(ns stackfn-sandbox.stack.utils-test
  (:require
   [clojure.test :as t]
   [clojure.zip :as z]
   [expectations.clojure.test
    :refer [approximately
            between
            between'
            defexpect
            expect
            expecting
            functionally
            side-effects]]
   [stackfn-sandbox.stack.utils :as sut]))

(defexpect trim-assign-from-varname-test
  (expect Exception (sut/trim-assign-from-varname ""))
  (expect Exception (sut/trim-assign-from-varname "!abc"))
  (expect "!abc" (sut/trim-assign-from-varname "!abc+")))

(defexpect iteration-test
  (expect '([])
          (->> '[]
               z/vector-zip
               sut/iter-zip
               (map z/node)))

  (expect [[1 2 [3 [4 5 6]]]
           1
           2
           [3 [4 5 6]]
           3
           [4 5 6]
           4
           5
           6]
          (->> '[1 2 [3 [4 5 6]]]
               z/vector-zip
               sut/iter-zip
               (map z/node))))

(defexpect descendants-test
  (expect []
          (-> '[]
              z/vector-zip
              sut/first-descendant
              z/node))

  (expect 1
          (-> '[1 2 [3 [4 5 [6]]]]
              z/vector-zip
              sut/first-descendant
              z/node))
  (expect 3
          (-> '[1 2 [[3] [4 5 [6]]]]
              z/vector-zip
              z/next z/next z/next z/down z/right ;; Navigated to [4 5 [6]]
              z/up ;; Navigated up to [[3] [4 5 [6]]]
              sut/first-descendant
              z/node))

  (expect []
          (-> '[]
              z/vector-zip
              sut/last-descendant
              z/node))

  (expect 6
          (-> '[1 2 [3 [[4] 5 [6]]]]
              z/vector-zip
              sut/last-descendant
              z/node)))
