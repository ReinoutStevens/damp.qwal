(ns
    ^{:doc "(Quantified) regular path expressions over graphlike structures"
      :author "Reinout Stevens"}
  damp.qwal
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(in-ns 'damp.qwal)

(comment
  "constructing example graph with a loop"
  (defn has-info [current info]
    (project [current]
             (all
              (== current info))))
  
  (defn
    ^{:doc "succeeds when to is the list of nodes that are direct successors of node" }
    to-node [node to]
    (conde [(== node :foo)
            (== to '(:bar))]
           [(== node :bar)
            (== to '(:baz))]
           [(== node :baz)
            (== to '(:quux))]
           [(== node :quux)
            (== to '(:foo))]))

  (defn
    from-node [node from]
    (conde [(== node :foo)
            (== from '(:quux))]
           [(== node :bar)
            (== from '(:foo))]
           [(== node :baz)
            (== from '(:bar))]
           [(== node :quux)
            (== from '(:baz))]))
  
  (def graph
    (let [nodes (list :foo :bar :baz :quux)]
      {:nodes nodes
       :successors to-node
       :predecessors from-node})))


(defn get-successors [graph node next]
  ((:successors graph) node next))

(defn get-predecessors [graph node pred]
  ((:predecessors graph) node pred))

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
                  (get-successors graph node nodes)
                  (membero next nodes))))

(defn
  ^{:doc "succeeds when previous is a direct predecessor of node" }
  rev-trans [graph node previous]
  (fresh [nodes]
         (project [node]
                  (get-predecessors graph node nodes)
                  (membero previous nodes))))


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
  (all
   (trans graph current next)))


(defn q<=
  ^{:doc "reverse transition"}
  [graph current previous]
  (all
   (rev-trans graph current previous)))



(defn
  ^{:doc "goals may succeed zero to multiple times.
Should detect loops by using tabled/slg resolution"}
  q* [& goals]
  (def q*loop
    (tabled
     [graph current end goals]
     (conde
      [(fresh [next]
              (solve-goals graph current next goals)
              (q*loop graph next end goals))] ;;goals may succeed an arbitrary nr of times
      [(== current end)])))
  (fn [graph current next]
    (all
     (q*loop graph current next goals))))


(defn
  ^{:doc "see q* but also calls => at the end of goals"}
  q*=> [& goals]
  (apply q* (concat goals [q=>])))
     

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
  (apply q+ (concat goals [q=>])))


(defn
  ^{:doc "goals may succeed or not"}
  q? [& goals]
  (fn [graph curr next]
    (conde [(solve-goals graph curr next goals)]
           [(== curr next)])))


(defn
  ^{:doc "main rule that solves a qrpe"}
  solve-qrpe [graph start end & goals ]
  (conde  [(fresh [h t next]
                  (!= nil goals)
                  (conso h t goals)
                  (project [h t]
                           (solve-goal graph start next h)
                           (apply solve-qrpe  graph next end t)))]
           [(== nil goals) ;; (emptyo goals) doesnt work for reasons unknown to the author
            (== start end)]))



;;Macros for nicer syntax, because sugar is good for you
(defmacro
  ^{:doc "A macro on top of solve-qrpe that allows for nicer syntax.
Graph holds the graph, and should at least understand :nodes, :successors and :predecessors.
Start node must be a member of the graph.
End node is assumed to be a member of the graph.
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
                       ~genend
                       ~@exps))))))


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
              (q*=>)
              (q*=> (with-current [curr] succeed))
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
