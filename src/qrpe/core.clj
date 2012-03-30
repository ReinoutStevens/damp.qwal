(ns
    ^{:doc "(Quantified) regular path expressions over graphlike structures"
      :author "Reinout Stevens"}
  qrpe.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.walk]))

(in-ns 'qrpe.core)


(comment
(defstruct directed-graph
  :nodes
  :neighbors)

(defn make-graph [nodes neighborfn]
  (struct-map directed-graph :nodes nodes :neighbors neighborfn))

(defn get-neighbors [graph node]
  ((:neighbors graph) node))

(def graph
  (let [baz (ref {})
        bar (ref {:info :bar, :to (list baz)})
        foo (ref {:info :foo, :to (list bar)})
        quux (ref {:info :quux, :to (list foo )})]
    (dosync
     (ref-set baz {:info :baz, :to (list quux)}))
    (make-graph (list foo bar baz quux)
                #(:to %1))))
)


(def graph
  {:nodes (list :foo :bar :baz :quux)})




(defn has-info [current info]
  (conde [(project [current]
                   (== current info))]))
    

(defn
  ^{:doc "succeeds when to is the list of nodes that are direct successors of node" }
  to-node [node to]
  (conda [(== node :foo)
          (== to '(:bar))]
         [(== node :bar)
          (== to '(:baz))]
         [(== node :baz)
          (== to '(:quux))]
         [(== node :quux)
          (== to '(:foo))]))

;;;; QRPE
          

(defn
  ^{:doc "succeeds when next is a direct successor of node" }
  trans [node next]
  (fresh [to]
         (to-node node to)
         (membero next to)))



(defn
  ^{:doc "calls goal with curr and next. curr is a grounded node. goal should ground next"}
  solve-goal [goal curr next]
  (goal curr next))


(defn
  ^{:doc "goals is a list of goals. Each goal is a rule with two arguments, current and next world.
Each goal is called, passing the next version of the previous goal as the
current version of the current goal" }
  solve-goals [goals curr end]
  (conde [(emptyo goals)
          (== curr end)]
         [(fresh [h t next]
                 (conso h t goals)
                 (project [ h t ]
                          (solve-goal h curr next)
                          (solve-goals t next end)))]))

(defn =>
  ^{:doc "fancier syntax for trans"}
  [current next]
  (trans current next))


(defn <=
  ^{:doc "reverse transition"}
  [current previous]
  (trans previous current))




(defn
  ^{:doc "goals may succeed zero to multiple times.
Should detect loops by using tabled/slg resolution"}
  q* [& goals]
  (def q*loop
    (tabled [gs current end]
            (conde
             [(fresh [next neext]
                     (solve-goals gs current next)
                     (q*loop gs neext end))]
             [(== current end)])))
  (fn [current next]
    (q*loop goals current next)))


(defn
  ^{:doc "see q* but also calls => at the end of goals"}
  q*=> [& goals]
  (apply q* (conj goals =>))) 
     

(defn
  ^{:doc "same as q*, except goals should succeed at least once"}
  q+ [& goals]
  (fn [current end]
    (fresh [next]
           (solve-goals goals current next)
           ((apply q* goals) next end))))

(defn
  ^{:doc "same as q+ but also calls => at the end of goals"}
  q+=> [& goals]
  (apply q+ (conj goals =>)))


(defn
  ^{:doc "goals may succeed or not"}
  q? [& goals]
  (fn [curr next]
    (conde [(solve-goals goals current next)]
           [(== curr next)])))


(defn
  ^{:doc "main rule that solves a qrpe"}
  solve-qrpe [start end & goals ]
  (conda  [(fresh [h t next]
                  (conso h t goals)
                  (project [h t]
                           (solve-goal h start next)
                           (apply solve-qrpe next end t)))]
           [(== nil goals) ;; (emptyo goals) doesnt work for reasons unknown to the author
            (== start end)]))



;;Macros for nicer syntax, because sugar is good for you
(defmacro
  ^{:doc "A macro on top of solve-qrpe that allows for nicer syntax.
Graph holds the graph, and should at least understand :nodes
Start and end are unified with nodes in graph.
Bindings are the new introduced variables that are kept throughout the pathexpression.
Exps are the actual goals that should hold on the path through the graph.
Each goal should be a rule that takes 2 variables.
First variable is the current world, and will be ground.
Second variable is the next world, and goal must ground this." }
  qrpe [graph start end bindings & exps ]
  (let [genstart (gensym "start")
        graphvar (gensym "graph")]
    `(let [~graphvar ~graph]
       (fresh  ~bindings
              (fresh [~genstart]
                     (== ~start ~genstart)
                     (membero ~genstart (:nodes ~graphvar))
                     (solve-qrpe
                      ~genstart
                      ~end
                      ~@exps))
              (membero ~end (:nodes ~graphvar))))))


(defmacro
  ^{:doc "macro to evaluate a series of goals in the same world"}
  in-current [& goals]
  (let [world (gensym "world")]
    `(with-current [~world]
       ~@goals)))
                    

(defmacro
  ^{:doc "macro that evaluates a series of goals in the current world. current is bound to the current world"}
  with-current [[current] & goals]
  (let [next (gensym "next")]
    `(fn [~current ~next]
       (all
        ~@goals
        (== ~current ~next)))))



(comment
  "example usage"
  (run* [end]
        (qrpe graph (first (:nodes graph)) end
              []
              (q* (with-current [curr] (has-info curr :foo)))
              (q*=> (with-current [curr] (fresh [info] (has-info curr info))))
              (with-current [curr] (has-info curr :foo))
              =>
              (with-current [curr] (has-info curr :bar))
              =>
              (with-current [curr] (has-info curr :baz))))
)
