(ns damp.qwal.test.qwal
  (:use [damp.qwal])
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [is]] :reload)
  (:use [clojure.test]))



(defn has-info [current info]
  (project [current]
           (all
            (== current info))))

(defn current-has-info [info]
  (qcurrent [current]
            (has-info current info)))

(defn
  ^{:doc "succeeds when to is the list of nodes that are direct successors of node" }
  to-node [node to]
  (conde [(== node 1)
          (== to '(2 3))]
         [(== node 2)
          (== to '(4 5))]
         [(== node 3)
          (== to '(6 7))]
         [(membero node '(4 5))
          (== to '(8))]
         [(membero node '(6 7))
          (== to '(9))]
         [(membero node '(8 9))
          (== to '(10))]))

(defn
  from-node [node from]
  (conde [(== node 10)
          (membero from '(8 9))]
         [(== node 9)
          (membero from '(6 7))]
         [(== node 8)
          (membero from '(4 5))]
         [(membero node '(6 7))
          (== from '(3))]
         [(membero node '(4 5))
          (== from '(2))]
         [(membero node '(2 3))
          (== from 1)]))

(defn
  to-node-cyclic [node to]
  (conde [(to-node node to)]
         [(== node 10)
          (== to '(1))]))

(defn
  from-node-cyclic [node from]
  (conde [(from-node node from)]
         [(== node 1)
          (== from '(10))]))

(def graph
  (let [nodes (range 1 11)]
    {:nodes nodes
     :successors to-node
     :predecessors from-node}))


(def cyclic-graph
  (let [nodes (range 1 11)]
    {:nodes nodes
     :successors to-node-cyclic
     :predecessors from-node-cyclic}))


(defn basic-walk [graph]
  (run* [endstate]
        (qwal graph 1 endstate []
              (current-has-info 1)
              q=>
              (current-has-info 2)
              q=>
              (current-has-info 4)
              q=>
              (current-has-info 8)
              q=>
              (current-has-info 10))))

(defn logic-odd? [nr]
  (all (== true (odd? nr))))

(defn logic-even? [nr]
  (all (== true (even? nr))))



(defn greedy-many [graph]
  (run* [endstate]
        (qwal graph 1 endstate []
              (q=>* (qcurrent [curr]
                              (logic-odd? curr))))))



(defn reluctant-many [graph]
  (run* [endstate]
        (qwal graph 1 endstate []
              (q=>*? (qcurrent [curr]
                               (logic-odd? curr))))))


(defn while-odd [graph]
  (run* [endstate]
        (qwal graph 1 endstate []
              (qwhile curr [(logic-odd? curr)] q=>))))


(defn until-even [graph]
  (run* [endstate]
        (qwal graph 1 endstate []
              (quntil curr [(logic-even? curr)] q=>))))
              

(deftest basic-queries
  (is (= (basic-walk graph) '(10)))
  (is (= (greedy-many graph) (reverse '(1 2 3 6 7 9 10))) "for reasons unknown greedy behaves as reluctant")
  (is (= (greedy-many cyclic-graph) (greedy-many graph)))
  (is (= (reluctant-many graph) '(1 2 3 6 7 9 10)))
  (is (= (reluctant-many cyclic-graph) (reluctant-many graph)))
  (is (= (while-odd graph) '(2 6 10)))
  (is (= (until-even graph) '(2 6 10))))



