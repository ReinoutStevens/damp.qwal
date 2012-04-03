(ns
    ^{:doc "(Quantified) regular path expressions over graphlike structures"
      :author "Reinout Stevens"}
  damp.qwal.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(in-ns 'damp.qwal.core)

(comment
(defstruct directed-graph
  :nodes
  :neighbors)

(defn make-graph [nodes neighborfn]
  (struct-map directed-graph :nodes nodes :neighbors neighborfn))
)




(defn has-info [current info]
  (project [current]
           (all
            (== current info))))
    

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


(def graph
  {:nodes (list :foo :bar :baz :quux)
   :neighbors to-node})


(defn get-neighbors [graph node next]
  ((:neighbors graph) node next))

;;;; QRPE
;; Atm I am not sure whether adding quantifiers actually adds anything.
;; The universal and existential quantifier are dual.
;; Saying that an expression has to hold on all the paths between A and B
;; is the same as finding a path between A and B where the exps does not hold.
          

(defn
  ^{:doc "succeeds when next is a direct successor of node" }
  trans [graph node next]
  (fresh [nodes]
         (project [node]
                  (get-neighbors graph node nodes)
                  (membero next nodes))))


(defn
  ^{:doc "solves goal in the current world.
Arguments to the goal are goal, current and next.
Goal should ground next."}
  solve-goal [graph current next goal]
  (all
   (goal graph current next)))


(defn
  ^{:doc "goals is a list of goals.
Each goal is called, passing the next version of the previous goal as the
current version of the current goal" }
  solve-goals [graph curr end goals]
  (conde [(emptyo goals)
          (== curr end)]
         [(fresh [h t next]
                 (conso h t goals)
                 (project [ h t ]
                          (solve-goal graph curr next h)
                          (solve-goals graph next end t)))]))

(defn q=>
  ^{:doc "fancier syntax for trans"}
  [graph current next]
  (trans graph current next))


(defn q<=
  ^{:doc "reverse transition"}
  [graph current previous]
  (conde [(membero previous (:nodes graph))
          (trans graph previous current)]))




(defn
  ^{:doc "goals may succeed zero to multiple times.
Should detect loops by using tabled/slg resolution"}
  q* [& goals]
  (def q*loop
    (tabled [graph current end goals]
            (conde
             [(fresh [next neext]
                     (solve-goals graph current next goals)
                     (q*loop graph neext end goals))] ;;goals may succeed an arbitrary nr of times
             [(== current end)])))
  (fn [graph current next]
    (q*loop graph current next goals)))


(defn
  ^{:doc "see q* but also calls => at the end of goals"}
  q*=> [& goals]
  (apply q* (conj goals q=>)))
     

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
  (apply q+ (conj goals q=>)))


(defn
  ^{:doc "goals may succeed or not"}
  q? [& goals]
  (fn [graph curr next]
    (conde [(solve-goals graph curr next goals)]
           [(== curr next)])))


(defn
  ^{:doc "main rule that solves a qrpe"}
  solve-qrpe [graph start end & goals ]
  (conda  [(fresh [h t next]
                  (conso h t goals)
                  (project [h t]
                           (solve-goal graph start next h)
                           (apply solve-qrpe  graph next end t)))]
           [(== nil goals) ;; (emptyo goals) doesnt work for reasons unknown to the author
            (== start end)]))



;;Macros for nicer syntax, because sugar is good for you
(defmacro
  ^{:doc "A macro on top of solve-qrpe that allows for nicer syntax.
Graph holds the graph, and should at least understand :nodes and :neighbors
Start and end are unified with nodes in graph.
Bindings are the new introduced variables that are kept throughout the pathexpression.
Exps are the actual goals that should hold on the path through the graph.
Each goal should be a rule that takes 2 variables.
First variable is the current world, and will be ground.
Second variable is the next world, and goal must ground this." }
  qrpe [graph start end bindings & exps ]
  (let [genstart (gensym "start")
        genend (gensym "end")
        graphvar (gensym "graph")]
    `(let [~graphvar ~graph]
       (fresh  ~bindings
               (fresh [~genstart ~genend]
                      (== ~start ~genstart)
                      (== ~end ~genend)
                      (membero ~genstart (:nodes ~graphvar))
                      (solve-qrpe
                       ~graphvar
                       ~genstart
                       ~end
                       ~@exps)
                      (membero ~genend (:nodes ~graphvar)))))))


(defmacro
  ^{:doc "macro to evaluate a series of goals in the same world"}
  in-current [& goals]
  (let [world (gensym "world")]
    `(with-current [~world]
       ~@goals)))
                    

(defmacro
  ^{:doc "macro that evaluates a series of goals in the current world. current is bound to the current world"}
  with-current [[current] & goals]
  (let [next (gensym "next")
        graph (gensym "graph")]
    `(fn [~graph ~current ~next]
       (all
        ~@goals
        (== ~current ~next)))))



(comment
  "example usage"
  (run* [end]
        (qrpe graph (first (:nodes graph)) end
              [info]
              (q* (with-current [curr] (has-info curr info)))
              (q*=> (with-current [curr] (fresh [info] (has-info curr info))))
              (with-current [curr] (has-info curr :foo))
              q=>
              (with-current [curr] (has-info curr :bar))
              q=>
              (q? (with-current [curr] (has-info curr :foo)) q=>)
              (with-current [curr] (has-info curr :baz))
              q=> q=>
              (with-current [curr] (has-info curr info))))
)
