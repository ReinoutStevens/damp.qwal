(ns
    ^{:doc "(Quantified) regular path expressions over graphlike structures"
      :author "Reinout Stevens"}
  damp.qwal
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

(in-ns 'damp.qwal)



(defn get-successors [graph node next]
  ((:successors graph) node next))

(defn get-predecessors [graph node pred]
  ((:predecessors graph) node pred))

;;;; Regular Path Expressions

(defn default-track-path [graph next end-of-path new-end-of-path]
  (all
   (conso next new-end-of-path end-of-path)))


(defn track-path [graph next end-of-path new-end-of-path]
  (all
   (default-track-path graph next end-of-path new-end-of-path)))



(defn
  ^{:doc "succeeds when next is a direct successor of node" }
  trans [graph node next end-of-path new-end-of-path]
  (fresh [nodes]
         (project [node]
                  (get-successors graph node nodes)
                  (membero next nodes)
                  (track-path graph next end-of-path new-end-of-path))))

(defn
  ^{:doc "succeeds when previous is a direct predecessor of node" }
  rev-trans [graph node previous end-of-path new-end-of-path]
  (fresh [nodes]
         (project [node]
                  (get-predecessors graph node nodes)
                  (membero previous nodes)
                  (track-path graph previous end-of-path new-end-of-path))))


(defn
  default-solve-goal [graph current next goal end-of-path new-end-of-path]
  (all
   (goal graph current next end-of-path new-end-of-path)))
   



(defn
  ^{:doc "solves goal in the current world.
Arguments to the goal are goal, current and next.
Goal should ground next."}
  solve-goal [graph current next end-of-path new-end-of-path goal]
  (let [goal-solver (or (:goal-solver graph) default-solve-goal)]
    (goal-solver graph current next end-of-path new-end-of-path goal)))



(defn
  ^{:doc "goals is a list of goals.
Each goal is called, passing the next version of the previous goal as the
current version of the current goal."}
  solve-goals [graph curr end end-of-path new-end-of-path goals]
  (conde [(emptyo goals)
          (== curr end)
          (== end-of-path new-end-of-path)]
         [(fresh [h t next newpathvar]
                 (conso h t goals)
                 (project [ curr h t ]
                          (solve-goal graph curr next h end-of-path newpathvar)
                          (solve-goals graph next end newpathvar new-end-of-path t)))]))

(defn q=>
  ^{:doc "fancier syntax for trans"}
  [graph current next end-of-path new-end-of-path]
  (all
   (trans graph current next end-of-path new-end-of-path)))


(defn q<=
  ^{:doc "reverse transition"}
  [graph current previous end-of-path new-end-of-path]
  (all
   (rev-trans graph current previous end-of-path new-end-of-path)))



(defn
  ^{:doc "goals may succeed zero to multiple times.
Should detect loops by using tabled/slg resolution.
q* is greedy, meaning it tries the longest path for which goals holds.
see q*? for the reluctant variant
BUG: currently greedy behaves just as the reluctant version,
as conde interleaves between its choices." }
  q* [& goals]
  (def q*loop
    (tabled
     [graph current end goals end-of-path new-end-of-path]
     (all
      (trace-lvars "q*loop" current end-of-path new-end-of-path)
     (conde
      [(fresh [next fresh-end-of-path]
              (solve-goals graph current next end-of-path fresh-end-of-path goals)
              (q*loop graph next end goals fresh-end-of-path new-end-of-path))] ;;goals may succeed an arbitrary nr of times
      [(== current end)
       (== end-of-path new-end-of-path)]))))
  (fn [graph current next end-of-path new-end-of-path]
    (q*loop graph current next goals end-of-path new-end-of-path)))


(defn
  ^{:doc "reluctant/non-greedy version of q*"}
  q*? [& goals]
  (def q*?loop
    (tabled
     [graph current end goals end-of-path new-end-of-path ]
     (conde
      [(== current end)
       (== end-of-path new-end-of-path)]
      [(fresh [next fresh-end-of-path]
              (solve-goals graph current next end-of-path fresh-end-of-path goals)
              (q*?loop graph next goals fresh-end-of-path new-end-of-path))])))
  (fn [graph current next end-of-path new-end-of-path]
    (all
     (q*?loop graph current next goals end-of-path new-end-of-path))))


(defn
  ^{:doc "see q*, but also calls q=> at the end of goals"}
  q=>* [& goals]
  (apply q* (concat goals [q=>])))


(defn
  ^{:doc "see q*?, but also calls q=> at the end of goals"}
  q=>*? [& goals]
  (apply q*? (concat goals [q=>])))


(defn
  ^{:doc "see q*, but also calls q<= at the end of goals"}
  q<=* [& goals]
  (apply q* (concat goals [q<=])))

(defn
  ^{:doc "see q*?, but also calls q<= at the end of goals"}
  q<=? [& goals]
  (apply q*? (concat goals [q<=])))


(defn
  ^{:doc "same as q*, except goals should succeed at least once"}
  q+ [& goals]
  (fn [graph current end end-of-path new-end-of-path]
    (fresh [next fresh-end-of-path]
           (solve-goals graph current next end-of-path fresh-end-of-path goals)
           ((apply q* goals) graph next end fresh-end-of-path new-end-of-path))))


(defn
  ^{:doc "same as q*?, except goals should succeed at least once"}
  q+? [& goals]
  (fn [graph current end end-of-path new-end-of-path]
    (fresh [next fresh-end-of-path]
           (solve-goals graph current next end-of-path fresh-end-of-path goals)
           ((apply q*? goals) graph next end fresh-end-of-path new-end-of-path))))

(defn
  ^{:doc "see q+, but also calls q=> at the end of goals"}
  q=>+ [& goals]
  (apply q+ (concat goals [q=>])))

(defn
  ^{:doc "see q+?, but also calls q=> at the end of goals"}
  q=>+? [& goals]
  (apply q+? (concat goals [q=>])))


(defn
  ^{:doc "see q+, but also calls q<= at the end of goals"}
  q<=+ [& goals]
  (apply q+ (concat goals [q<=])))

(defn
  ^{:doc "see q+?, but also calls q<= at the end of goals"}
  q<=+? [& goals]
  (apply q+? (concat goals [q<=])))


(defn
  ^{:doc "goals may succeed or not"}
  q? [& goals]
  (fn [graph curr next end-of-path new-end-of-path]
    (conde [(solve-goals graph curr next end-of-path new-end-of-path goals)]
           [(== curr next)
            (== end-of-path new-end-of-path)])))


;;one may argue about tabling this or not
(defn
  ^{:doc "goals has to succeed times times"}
  qtimes [times & goals]
  (defn times-bound-loop [graph curr next end-of-path new-end-of-path number]
    (fresh [neext fresh-end-of-path]
           (conde [(== number times)
                   (== curr next)
                   (== end-of-path new-end-of-path)]
                  [(== true (< number times))
                   (solve-goals graph curr neext end-of-path fresh-end-of-path goals)
                   (times-bound-loop graph neext next fresh-end-of-path new-end-of-path (inc number))])))
  (defn times-unbound-loop [graph curr next end-of-path new-end-of-path number]
    (fresh [neext fresh-end-of-path]
           (conde [(== number times)
                   (== curr next)
                   (== end-of-path new-end-of-path)]
                  [(solve-goals graph curr neext end-of-path fresh-end-of-path goals)
                   (times-unbound-loop graph neext next fresh-end-of-path new-end-of-path (inc number))])))
  (fn [graph curr next end-of-path new-end-of-path]
    (project [times]
             (if (lvar? times) ;;unbound
               (times-unbound-loop graph curr next end-of-path new-end-of-path 0)
               (times-bound-loop graph curr next end-of-path new-end-of-path 0)))))

(defn
  ^{:doc "see qtimes, but also calls q=> at the end of goals"}
  qtimes=> [times & goals]
  (apply qtimes times
         (concat goals [q=>])))

(defn
^{:doc "see qtimes, but also calls q=> at the end of goals"}
  qtimes<= [times & goals]
  (apply qtimes times
         (concat goals [q<=])))
  
              
    



(defn
  ^{:doc "implementing naf using conda"}
  qfail [& goals]
  (fn [graph current next end-of-path new-end-of-path]
    (conda
     [(solve-goals graph current next end-of-path new-end-of-path goals)
      fail]
     [(== current next)
      (== end-of-path new-end-of-path)])))


(defmacro
  ^{:doc "reverse of qwhile.
Goals are executed until conditions hold in current."}
  quntil [current [ & conditions ] & goals]
  `(qwhile ~current [ (qfail (all ~@conditions)) ] ~@goals))



(defmacro
  ^{:doc "calls goals as long as conditions holds.
Current is bound to the current world and can thus be used in conditions.
Note that when & goals doesn't go to a successor zero results are found."}
  qwhile [current [& conditions] & goals]
  (let [realgoals (if (nil? goals) '() goals)]
    `(fn [graphvar# current# endvar# end-of-path# new-end-of-path#]
       (def loopvar#
         (tabled [ graphvar# current# endvar# end-of-path# new-end-of-path# ]
                 (project [current#]
                          (conda [~@conditions
                                  (fresh [nextvar# fresh-path#]
                                         ;;for reasons unknown this doesnt work when you just use ~realgoals
                                         (solve-goals graphvar# current# nextvar# end-of-path# fresh-path# (list ~@realgoals))
                                         (loopvar# graphvar# nextvar# endvar# fresh-path# new-end-of-path#))]
                                 [(== current# endvar#)
                                  (== end-of-path# new-end-of-path#)]))))
       (loopvar# graphvar# current# endvar# end-of-path# new-end-of-path#))))

(defn solve-qrpe [graph start end end-of-path new-end-of-path & goals]
  (all
   (solve-goals graph start end end-of-path new-end-of-path goals)))
   
       

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
  qwal [graph start end path bindings & exps ]
  (let [genstart (gensym "start")
        genend (gensym "end")
        graphvar (gensym "graph")
        startpath (gensym "startpath")
        endpath (gensym "endpath")]
    `(let [~graphvar ~graph] 
       (project [~graphvar]
                (fresh  ~bindings
                        (fresh [~genstart ~genend ~startpath ~endpath]
                               (== ~start ~genstart)
                               (== ~end ~genend)
                               (solve-qrpe
                                ~graphvar
                                ~genstart
                                ~genend
                                ~path
                                '()
                                ~@exps)))))))


(defmacro
  ^{:doc "macro to evaluate a series of conditions in the same world"}
  qin-current [& conditions]
  (let [world (gensym "world")]
    `(qcurrent [~world]
               ~@conditions)))


(defmacro
  ^{:doc "macro that evaluates a series of conditions in the current world. current is bound to the current world"}
  qcurrent [[current] & conditions]
  `(fn [graph# ~current next# end-of-path# new-end-of-path#]
       (project [~current]
                ~@conditions
                (== ~current next#)
                (== end-of-path# new-end-of-path#))))

(defmacro qcurrento [[current] & conditions]
  ^{:doc "macro that evaluated a series of conditions in the current worls. current is unified with the current world, and wrapped inside a project"}
  `(fn [graph# curr# next#]
    (all
     (== ~current curr#)
     (project [~current]
              ~@conditions
              (== curr# next#)))))


(defn
  ^{:doc "Helper function that creates a goal that solves all goals passed as argument"}
  all-goals [& goals]
  (fn [graph current next]
    (solve-goals graph current next goals)))


(defn
  ^{:doc "Poorly named function that can be used inside a goal to solve more goals"}
  solve-all-goals [graph current next & goals]
  (solve-goals graph current next goals))
      
    
(comment
  "example usage"

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
           [(== node :rein)
            (== from '(:baz))]
           [(== node :quux)
            (== from '(:baz))]))
  
  (def graph
    (let [nodes (list :foo :bar :baz :quux :rein)]
      {:nodes nodes
       :successors to-node
       :predecessors from-node}))

  (run* [end]
        (qwal graph (first (:nodes graph)) end
              [info curro]
              (q=>*)
              (q=>* (qcurrent [curr] succeed))
              (q=>* (qcurrent [curr] (fresh [info] (has-info curr info))))
              (qcurrent [curr] (has-info curr :foo))
              q=>
              (qcurrent [curr] (has-info curr :bar))
              q=>
              (q? (qcurrent [curr] (has-info curr :foo)) q=>)
              (qcurrent [curr] (has-info curr :baz))
              q=> q=>
              (qcurrento [curro] (has-info curro info))))
  )
