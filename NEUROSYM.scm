;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; NEUROSYM.scm — Neuro-Symbolic Patterns for STATE.scm
;;;
;;; Defines the integration between neural (LLM) and symbolic
;;; (Scheme/minikanren) reasoning for STATE.scm operations.

(define-module (state.scm neurosym)
  #:export (neurosym-patterns))

(define neurosym-patterns
  '((identity
     (name . "STATE.scm Neuro-Symbolic Patterns")
     (version . "1.0.0")
     (purpose . "Neural-symbolic integration for AI project management"))

    ;;; ========================================================
    ;;; ARCHITECTURE OVERVIEW
    ;;; ========================================================
    (architecture
     (description . "STATE.scm combines neural (LLM) pattern recognition
                     with symbolic (Scheme/minikanren) logical reasoning
                     for robust project state management.")

     (layers
      ((layer . "neural")
       (role . "Natural language understanding, context summarization")
       (examples . ("Parse user intent"
                    "Generate human-readable reports"
                    "Identify implicit blockers from conversation")))

      ((layer . "symbolic")
       (role . "Formal reasoning, query execution, constraint checking")
       (examples . ("minikanren dependency queries"
                    "Topological sort for critical path"
                    "Schema validation")))

      ((layer . "grounding")
       (role . "Connect neural understanding to symbolic structures")
       (examples . ("Map 'blocked by X' to (statuso q 'blocked' state)"
                    "Convert completion estimates to snapshot data"
                    "Translate project names to identifiers")))))

    ;;; ========================================================
    ;;; NEURAL → SYMBOLIC MAPPINGS
    ;;; ========================================================
    (neural-to-symbolic

     ((pattern . "What projects are blocked?")
      (intent . "query-blocked")
      (symbolic . "(run* (q) (statuso q \"blocked\" state))")
      (output-transform . "List project names"))

     ((pattern . "What depends on X?")
      (intent . "query-dependents")
      (symbolic . "(run* (q) (dependso q \"X\" state))")
      (output-transform . "List dependent projects"))

     ((pattern . "Show me the critical path to X")
      (intent . "critical-path")
      (symbolic . "(find-critical-path \"X\" state)")
      (output-transform . "Ordered path list"))

     ((pattern . "When will X be done?")
      (intent . "estimate-completion")
      (symbolic . "(estimate-completion-date \"X\" state)")
      (output-transform . "ISO date or 'unknown'"))

     ((pattern . "How fast are we progressing?")
      (intent . "velocity-report")
      (symbolic . "(velocity-report state)")
      (output-transform . "Formatted velocity table"))

     ((pattern . "Generate a dependency graph")
      (intent . "visualize-dependencies")
      (symbolic . "(generate-dot state)")
      (output-transform . "DOT source or rendered image")))

    ;;; ========================================================
    ;;; SYMBOLIC → NEURAL MAPPINGS
    ;;; ========================================================
    (symbolic-to-neural

     ((symbol . "blocked")
      (neural-interpretation . "Cannot proceed due to external factors")
      (suggested-actions . ("Identify blocking factor"
                            "Find alternative approach"
                            "Escalate if persistent")))

     ((symbol . "in-progress")
      (neural-interpretation . "Actively being worked on")
      (suggested-actions . ("Continue work"
                            "Update completion percentage"
                            "Check for new blockers")))

     ((symbol . "velocity-declining")
      (neural-interpretation . "Progress rate is slowing")
      (suggested-actions . ("Review recent blockers"
                            "Check for scope creep"
                            "Consider breaking into smaller tasks")))

     ((symbol . "critical-path")
      (neural-interpretation . "Sequence that determines minimum completion time")
      (suggested-actions . ("Prioritize items on critical path"
                            "Identify parallelizable work"
                            "Add resources to bottleneck"))))

    ;;; ========================================================
    ;;; HYBRID REASONING PATTERNS
    ;;; ========================================================
    (hybrid-patterns

     ((name . "intelligent-prioritization")
      (description . "Combine symbolic dependency analysis with
                      neural understanding of urgency and impact")
      (steps
       ((step . "symbolic")
        (action . "Calculate topological order of projects"))
       ((step . "neural")
        (action . "Assess business impact of each project"))
       ((step . "hybrid")
        (action . "Merge to produce priority-ordered list"))))

     ((name . "blocker-inference")
      (description . "Use neural understanding to identify implicit
                      blockers, then validate symbolically")
      (steps
       ((step . "neural")
        (action . "Parse conversation for blocker mentions"))
       ((step . "symbolic")
        (action . "Verify blocker exists in project dependencies"))
       ((step . "hybrid")
        (action . "Add validated blockers to STATE"))))

     ((name . "context-compression")
      (description . "Use symbolic structure to efficiently compress
                      context for neural processing")
      (steps
       ((step . "symbolic")
        (action . "Extract relevant projects and dependencies"))
       ((step . "neural")
        (action . "Summarize into concise context prompt"))
       ((step . "hybrid")
        (action . "Maintain structured data + natural summary")))))

    ;;; ========================================================
    ;;; MINIKANREN INTEGRATION
    ;;; ========================================================
    (minikanren-integration
     (description . "minikanren provides declarative relational queries
                     that complement neural pattern recognition")

     (advantages
      ("Guaranteed complete enumeration of solutions"
       "Bidirectional queries (find X given Y or Y given X)"
       "Composable goal expressions"
       "Verifiable reasoning chains"))

     (example-goals
      ((goal . "(statuso q status state)")
       (mode . "bidirectional")
       (description . "Query projects by status or find status of project"))

      ((goal . "(dependso child parent state)")
       (mode . "bidirectional")
       (description . "Query dependencies in either direction"))

      ((goal . "(dependency-chain-o start end chain state)")
       (mode . "generative")
       (description . "Generate dependency chains between projects"))))

    ;;; ========================================================
    ;;; VERIFICATION AND GROUNDING
    ;;; ========================================================
    (verification
     (description . "Symbolic layer provides verification of neural outputs")

     (checks
      ((check . "schema-validation")
       (symbolic . "Validate STATE structure against schema")
       (catches . "Malformed or missing required fields"))

      ((check . "dependency-cycle-detection")
       (symbolic . "Topological sort fails on cycles")
       (catches . "Circular dependencies"))

      ((check . "completion-bounds")
       (symbolic . "Verify 0 <= completion <= 100")
       (catches . "Invalid percentage values"))

      ((check . "reference-integrity")
       (symbolic . "All dependency names exist as projects")
       (catches . "Dangling references"))))))

;;; END NEUROSYM.scm
