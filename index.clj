{:namespaces
 ({:source-url nil,
   :wiki-url "damp.qwal.core-api.html",
   :name "damp.qwal.core",
   :author "Reinout Stevens",
   :doc
   "(Quantified) regular path expressions over graphlike structures"}),
 :vars
 ({:arglists ([& goals]),
   :name "in-current",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/in-current",
   :doc "macro to evaluate a series of goals in the same world",
   :var-type "macro",
   :line 186,
   :file "src/qwal/core.clj"}
  {:arglists ([& goals]),
   :name "q*",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/q*",
   :doc
   "goals may succeed zero to multiple times.\nShould detect loops by using tabled/slg resolution",
   :var-type "function",
   :line 102,
   :file "src/qwal/core.clj"}
  {:arglists ([& goals]),
   :name "q*=>",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/q*=>",
   :doc "see q* but also calls => at the end of goals",
   :var-type "function",
   :line 117,
   :file "src/qwal/core.clj"}
  {:arglists ([& goals]),
   :name "q+",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/q+",
   :doc "same as q*, except goals should succeed at least once",
   :var-type "function",
   :line 123,
   :file "src/qwal/core.clj"}
  {:arglists ([& goals]),
   :name "q+=>",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/q+=>",
   :doc "same as q+ but also calls => at the end of goals",
   :var-type "function",
   :line 131,
   :file "src/qwal/core.clj"}
  {:arglists ([& goals]),
   :name "q?",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/q?",
   :doc "goals may succeed or not",
   :var-type "function",
   :line 137,
   :file "src/qwal/core.clj"}
  {:arglists ([graph start end bindings & exps]),
   :name "qrpe",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/qrpe",
   :doc
   "A macro on top of solve-qrpe that allows for nicer syntax.\nGraph holds the graph, and should at least understand :nodes and :neighbors\nStart and end are unified with nodes in graph.\nBindings are the new introduced variables that are kept throughout the pathexpression.\nExps are the actual goals that should hold on the path through the graph.\nEach goal should be a rule that takes 2 variables.\nFirst variable is the current world, and will be ground.\nSecond variable is the next world, and goal must ground this.",
   :var-type "macro",
   :line 159,
   :file "src/qwal/core.clj"}
  {:arglists ([graph current next goal]),
   :name "solve-goal",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/solve-goal",
   :doc
   "solves goal in the current world.\nArguments to the goal are goal, current and next.\nGoal should ground next.",
   :var-type "function",
   :line 65,
   :file "src/qwal/core.clj"}
  {:arglists ([graph curr end goals]),
   :name "solve-goals",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/solve-goals",
   :doc
   "goals is a list of goals.\nEach goal is called, passing the next version of the previous goal as the\ncurrent version of the current goal",
   :var-type "function",
   :line 74,
   :file "src/qwal/core.clj"}
  {:arglists ([graph start end & goals]),
   :name "solve-qrpe",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/solve-qrpe",
   :doc "main rule that solves a qrpe",
   :var-type "function",
   :line 145,
   :file "src/qwal/core.clj"}
  {:arglists ([node to]),
   :name "to-node",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/to-node",
   :doc
   "succeeds when to is the list of nodes that are direct successors of node",
   :var-type "function",
   :line 28,
   :file "src/qwal/core.clj"}
  {:arglists ([graph node next]),
   :name "trans",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/trans",
   :doc "succeeds when next is a direct successor of node",
   :var-type "function",
   :line 56,
   :file "src/qwal/core.clj"}
  {:arglists ([[current] & goals]),
   :name "with-current",
   :namespace "damp.qwal.core",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal.core-api.html#damp.qwal.core/with-current",
   :doc
   "macro that evaluates a series of goals in the current world. current is bound to the current world",
   :var-type "macro",
   :line 194,
   :file "src/qwal/core.clj"})}
