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

* `graph` is the graph we are querying, and should at least understand the method `:nodes` and `:neighbors`.
    * `(:nodes graph)` returns a collection of all the nodes in the graph
    * `(:neighbors graph)` returns a rule that takes two variables. It binds the second variable to the list of nodes connected with the first variable.
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

* `(in-current & conditions)` returns a goal that succeeds when all the conditions succeed. It stays in the same world.
* `(with-current [curr] & conditions)` is similar to `in-current`, except that `curr` is bound to the current world.

The following functions are predefined that return goals:

* `(q* & goals)` is a function that takes an arbitrary number of goals. These goals may succeed zero or multiple times.
* `(q=>* & goqls)` is similar to `q*`, except that after calling goals `q=>` is called as well.
* `(q+ & goals)` is similar to `q*`, except that the goals must succeed at least once.
* `(q=>+ & goals)` is similar to `q=>*`, except that the goals must succeed at least once.
* `(q? & goals)` tries to succeed goals, or stays in the current world. 


### Example
We define the following graph:

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

    (def graph
     {:nodes (list :foo :bar :baz :quux)
     :neighbors to-node})

We can describe the following path through the graph:

    (run* [end]
     (qrpe graph (first (:nodes graph)) end
      [info]
      (q* (with-current [curr] (has-info curr info)))
      (q*=> (with-current [curr] (fresh [info] (has-info curr info))))
      (with-current [curr] (has-info curr :foo))
      q=>
      (with-current [curr] (has-info curr :bar))
      q=>
      (q? (with-current [curr] (has-info curr :foo)) =>)
      (with-current [curr] (has-info curr :baz))
      q=> q=>
      (with-current [curr] (has-info curr info))))

Note that there are many redundant goals in this expression.

* The first goal tries to constantly prove that the current node has some info. QWAL detects the loop and stops executing this goal as it will not lead to more answers.
* The second goal tries to match the longest path that contains the same info as the first node. The difference is the use of `q*=>` versus `q*`, so a transition will happen.
* The third goal states that the current node must contain `:foo`. This means that the second goal consumes zero versions.
* The 4th goal transitions to the next version.
* We then say there is a node that contains `:bar` and transition to the next version.
* We then state that there may be a goal that contains `:foo`, and if the goals is present we transition to the next version.
* We then state that there is a goal called `:baz`, and transition twice. We are now back in the start node (as there is a loop in the graph).
* Finally we say that this node must contain the same info as the node we started in.

## Licence
GPL v3
