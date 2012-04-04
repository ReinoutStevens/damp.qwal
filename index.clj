{:namespaces
 ({:source-url nil,
   :wiki-url "damp.qwal-api.html",
   :name "damp.qwal",
   :author "Reinout Stevens",
   :doc
   "(Quantified) regular path expressions over graphlike structures"}),
 :vars
 ({:arglists ([& goals]),
   :name "in-current",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/in-current",
   :doc "macro to evaluate a series of goals in the same world",
   :var-type "macro",
   :line 201,
   :file "src/damp/qwal.clj"}
  {:arglists ([& goals]),
   :name "q*",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/q*",
   :doc
   "goals may succeed zero to multiple times.\nShould detect loops by using tabled/slg resolution",
   :var-type "function",
   :line 114,
   :file "src/damp/qwal.clj"}
  {:arglists ([& goals]),
   :name "q*=>",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/q*=>",
   :doc "see q* but also calls => at the end of goals",
   :var-type "function",
   :line 131,
   :file "src/damp/qwal.clj"}
  {:arglists ([& goals]),
   :name "q+",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/q+",
   :doc "same as q*, except goals should succeed at least once",
   :var-type "function",
   :line 137,
   :file "src/damp/qwal.clj"}
  {:arglists ([& goals]),
   :name "q+=>",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/q+=>",
   :doc "same as q+ but also calls => at the end of goals",
   :var-type "function",
   :line 145,
   :file "src/damp/qwal.clj"}
  {:arglists ([& goals]),
   :name "q?",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/q?",
   :doc "goals may succeed or not",
   :var-type "function",
   :line 151,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph start end bindings & exps]),
   :name "qrpe",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/qrpe",
   :doc
   "A macro on top of solve-qrpe that allows for nicer syntax.\nGraph holds the graph, and should at least understand :nodes, :successors and :predecessors.\nStart node must be a member of the graph.\nEnd node is assumed to be a member of the graph.\nBindings are the new introduced variables that are kept throughout the pathexpression.\nExps are the actual goals that should hold on the path through the graph.\nEach goal should be a rule that takes 2 variables.\nFirst variable is the current world, and will be ground.\nSecond variable is the next world, and goal must ground this.",
   :var-type "macro",
   :line 174,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph node previous]),
   :name "rev-trans",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/rev-trans",
   :doc "succeeds when previous is a direct predecessor of node",
   :var-type "function",
   :line 68,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph current next goal]),
   :name "solve-goal",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/solve-goal",
   :doc
   "solves goal in the current world.\nArguments to the goal are goal, current and next.\nGoal should ground next.",
   :var-type "function",
   :line 77,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph curr end goals]),
   :name "solve-goals",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/solve-goals",
   :doc
   "goals is a list of goals.\nEach goal is called, passing the next version of the previous goal as the\ncurrent version of the current goal",
   :var-type "function",
   :line 86,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph start end & goals]),
   :name "solve-qrpe",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/solve-qrpe",
   :doc "main rule that solves a qrpe",
   :var-type "function",
   :line 159,
   :file "src/damp/qwal.clj"}
  {:arglists ([graph node next]),
   :name "trans",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/trans",
   :doc "succeeds when next is a direct successor of node",
   :var-type "function",
   :line 60,
   :file "src/damp/qwal.clj"}
  {:arglists ([[current] & goals]),
   :name "with-current",
   :namespace "damp.qwal",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url "/damp.qwal-api.html#damp.qwal/with-current",
   :doc
   "macro that evaluates a series of goals in the current world. current is bound to the current world",
   :var-type "macro",
   :line 209,
   :file "src/damp/qwal.clj"})}
