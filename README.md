# damp.qwal

QWAL is a path expression library to allow querying graphlike structures using core.logic in Clojure.

## Regular Path Expressions
Regular Path Expressions are a way to express paths throughout a graph. A path expression can be seen as a highlevel description of a valid
path throughout a graph. They are similar to regular expressions, as regexps provide a way to describe multiple valid strings, without having to
enumerate them all.

A regular path expression has similar constructs, such as repetition (`*` and `+`), choice (`|`, `?`), grouping and backreferences.
The main difference is that a regular expression only reasons over a single path (namely a string of characters), while a regular path expression
reasons over multiple paths in a graph.

## Usage
The main function is qrpe, which stands for quantified regular path expression (the name is a bit silly since we dont have quantifiers at the moment).
The function has the following signature:
    (qrpe graph start end bindings & goals)

* `graph` is the graph we are querying, and should at least understand the method `:nodes`, `:successors` and `:predecessors`.
    * `(:nodes graph)` returns a collection of all the nodes in the graph
    * `(:successors graph)` returns a rule that takes two variables. It binds the second variable to the list of nodes connected with the first variable.
    * `(:predecessors graph)` returns a rule that takes two variables. It binds
      the second variable to the list of nodes that can directly reach the
      first variable.
* `start` is the start node of the path, and must be a member of the nodes of the graph.
* `end` is the end node of the path, and must be a member of the nodes of the graph.
* `bindings` is a vector with bindings that need to be available throughout the whole expression.
* `goals` is an arbitrary amount of goals. We explain later what a goal is.


### Goals
A goal is a function that takes three arguments: the graph, the current node and the next node.
The first two arguments will always be grounded, the third one can either be grounded or ungrounded.

It is the responsability of the goal to unify the next node with the next node of the path expression.
Goals that want to transition to one of the successors can do this by unifying next with one of the successors of the current node.
Goals that want to stay in the current world can just unify next with current.


The following goals are predefined:

* `q=>` is a goal that transitions to one of the successors of the current node. 
* `q<=` is a goal that transitions to one of the predecessors of the current node.

The following macros that generate goals are predefined:

* `(qin-current & conditions)` returns a goal that succeeds when all the conditions succeed. It stays in the same world.
* `(qcurrent [curr] & conditions)` is similar to `qin-current`, except that `curr` is bound to the current world.
* `(qwhile current [& conditions ] & goals)` executes goals as long as conditions hold in current.

The following functions are predefined that return goals:

* `(q* & goals)` is a function that takes an arbitrary number of goals. These goals may succeed zero or multiple times.
* `(q=>* & goqls)` is similar to `q*`, except that after calling goals `q=>` is called as well.
* `(q<=* & goals)` is similar to `q*`, except that after calling goals `q<=` is called as well.
* `(q+ & goals)` is similar to `q*`, except that the goals must succeed at least once.
* `(q=>+ & goals)` is similar to `q=>*`, except that the goals must succeed at least once.
* `(q<=+ & goals)` is similar to `q<=*`, except that the goals must succeed at least once.
* `(q? & goals)` tries to succeed goals, or stays in the current world. 


### Example
We have the following graph:

    foo -> bar -> baz -> quux -> foo -> ...


We can define it as follows:

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
        :predecessors from-node}))

    "helper function to verify that a node unifies with a symbol"
    (defn has-info [current info]
      (project [current]
        (all
          (== current info))))


We begin by simply describing the path `foo->bar->baz->quux` through
the graph.


    (let [start (first (:nodes graph))] ;;we start in foo
      (run* [end] ;;we end in :quux
        (qwal graph start end 
          []
          (qcurrent [curr] (has-info curr :foo))
          q=>
          (qcurrent [curr] (has-info curr :bar))
          q=>
          (qcurrent [curr] (has-info curr :baz))
          q=>
          (qcurrent [curr] (has-info curr :quux)))))


This query illustrates the basic usage of qwal, but is not that
interesting. A more interesting one is finding whether there is a loop
in the graph.


    (let [start (first (:nodes graph))]
      (run* [end]
        (qwal graph start end 
          [info] ;;a variable that is shared across worlds/nodes
          (q=>*) ;;skip (possibly 0) arbitrary number of worlds
          (qcurrent [curr] (has-info curr info))
          (q=>+) ;;skip (at least 1) arbitrary number of worlds
          (qcurrent [curr] (has-info curr info)))))


In this query we describe a path that starts in `:foo`. It skips an
arbitrary number of worlds by using `(q=>*)`, and then binds `info` to
the current node. This means that `info` will take all the values of
reachable nodes from `:foo`. We have a loop if there is a possible
path from the current world to the same world again. We specify this
by taking one or more transitions from the current world, which is
done using `(q=>+)`. Note that `(q=>*)` is incorrect here, as we could
stay in the current world. Finally, we specify once again that the
current world has the same info object, ensuring a loop.

One of the important things to note from this example is that qwal
detects loops, and prevents from going in an infinite computation that
would not yield more results. This is done using the tabled resolution
of core.logic.




## Licence
Copyright (C) 2012 Reinout Stevens

Distributed under the Eclipse Public License, the same as Clojure.
