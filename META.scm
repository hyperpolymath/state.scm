;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; META.scm — state.scm Project Metadata & Component Status
;;;
;;; Final Project Status: 32/32 Components Complete
;;; Language: Guile Scheme 3.0+

(define-module (state.scm meta)
  #:export (component-status
            architecture-decisions
            development-practices
            design-rationale
            language-stack
            project-status))

;;; ============================================================
;;; COMPONENT STATUS — 32/32 Complete
;;; ============================================================

(define component-status
  '((summary
     (total-components . 32)
     (complete . 32)
     (in-progress . 0)
     (planned . 0)
     (completion-percentage . 100))

    ;; ----------------------------------------------------------
    ;; 1. Core Engine (5/5)
    ;; ----------------------------------------------------------
    (core-engine
     (status . "complete")
     (count . "5/5")
     (components
      ((name . "state-core.scm")
       (location . "lib/state-core.scm")
       (purpose . "Core accessors and predicates"))
      ((name . "state-kanren.scm")
       (location . "lib/state-kanren.scm")
       (purpose . "minikanren relational queries with fallback"))
      ((name . "state-graph.scm")
       (location . "lib/state-graph.scm")
       (purpose . "GraphViz DOT and Mermaid generation"))
      ((name . "state-history.scm")
       (location . "lib/state-history.scm")
       (purpose . "Velocity tracking and estimation"))
      ((name . "state.scm")
       (location . "lib/state.scm")
       (purpose . "Main entry point and re-exports"))))

    ;; ----------------------------------------------------------
    ;; 2. Build System (5/5)
    ;; ----------------------------------------------------------
    (build-system
     (status . "complete")
     (count . "5/5")
     (components
      ((name . "Justfile")
       (location . "justfile")
       (purpose . "Task automation with just"))
      ((name . "Guix Package")
       (location . "guix.scm")
       (purpose . "Primary package definition"))
      ((name . "Nix Flake")
       (location . "flake.nix")
       (purpose . "Fallback reproducible builds"))
      ((name . "Guix Manifest")
       (location . "manifest.scm")
       (purpose . "Development environment"))
      ((name . "Containerfile")
       (location . "Containerfile")
       (purpose . "OCI container image"))))

    ;; ----------------------------------------------------------
    ;; 3. Query System (4/4)
    ;; ----------------------------------------------------------
    (query-system
     (status . "complete")
     (count . "4/4")
     (components
      ((name . "minikanren Integration")
       (location . "lib/state-kanren.scm")
       (purpose . "Relational logic queries"))
      ((name . "minikanren Fallback")
       (location . "lib/state-kanren.scm")
       (purpose . "Simplified implementation when unavailable"))
      ((name . "Core Queries")
       (location . "lib/state-core.scm")
       (purpose . "Basic accessors and predicates"))
      ((name . "Graph Analysis")
       (location . "lib/state-graph.scm")
       (purpose . "Topological sort, critical path"))))

    ;; ----------------------------------------------------------
    ;; 4. Visualization (4/4)
    ;; ----------------------------------------------------------
    (visualization
     (status . "complete")
     (count . "4/4")
     (components
      ((name . "GraphViz DOT")
       (location . "lib/state-graph.scm")
       (purpose . "Dependency graph generation"))
      ((name . "Mermaid Diagrams")
       (location . "lib/state-graph.scm")
       (purpose . "Markdown-embedded diagrams"))
      ((name . "Status Graphs")
       (location . "lib/state-graph.scm")
       (purpose . "Project status visualization"))
      ((name . "Critical Path")
       (location . "lib/state-graph.scm")
       (purpose . "Critical path highlighting"))))

    ;; ----------------------------------------------------------
    ;; 5. History & Estimation (4/4)
    ;; ----------------------------------------------------------
    (history-estimation
     (status . "complete")
     (count . "4/4")
     (components
      ((name . "Snapshot Management")
       (location . "lib/state-history.scm")
       (purpose . "Create and prune snapshots"))
      ((name . "Velocity Calculation")
       (location . "lib/state-history.scm")
       (purpose . "Progress rate tracking"))
      ((name . "Completion Estimation")
       (location . "lib/state-history.scm")
       (purpose . "Predict completion dates"))
      ((name . "Burndown Data")
       (location . "lib/state-history.scm")
       (purpose . "Generate chart data"))))

    ;; ----------------------------------------------------------
    ;; 6. Specification (3/3)
    ;; ----------------------------------------------------------
    (specification
     (status . "complete")
     (count . "3/3")
     (components
      ((name . "Format Specification")
       (location . "spec/STATE-FORMAT-SPEC.adoc")
       (purpose . "IETF-style format spec"))
      ((name . "ABNF Grammar")
       (location . "spec/abnf/state.abnf")
       (purpose . "Formal syntax definition"))
      ((name . "JSON Schema")
       (location . "spec/schema/state.schema.json")
       (purpose . "Machine-readable schema"))))

    ;; ----------------------------------------------------------
    ;; 7. Documentation (7/7)
    ;; ----------------------------------------------------------
    (documentation
     (status . "complete")
     (count . "7/7")
     (components
      ((name . "README")
       (location . "README.adoc")
       (purpose . "Project overview"))
      ((name . "Usage Guide")
       (location . "USAGE.adoc")
       (purpose . "Comprehensive usage docs"))
      ((name . "Changelog")
       (location . "CHANGELOG.adoc")
       (purpose . "Version history"))
      ((name . "Contributing")
       (location . "CONTRIBUTING.adoc")
       (purpose . "Contribution guidelines"))
      ((name . "Security Policy")
       (location . "SECURITY.md")
       (purpose . "Vulnerability reporting"))
      ((name . "Governance")
       (location . "GOVERNANCE.adoc")
       (purpose . "Project governance"))
      ((name . "Code of Conduct")
       (location . "CODE_OF_CONDUCT.adoc")
       (purpose . "Community standards"))))

    ;; ----------------------------------------------------------
    ;; 8. Examples (2/2)
    ;; ----------------------------------------------------------
    (examples
     (status . "complete")
     (count . "2/2")
     (components
      ((name . "Template STATE")
       (location . "STATE.scm")
       (purpose . "Base template file"))
      ((name . "Example STATE")
       (location . "examples/example-state.scm")
       (purpose . "Realistic example"))))

    ;; ----------------------------------------------------------
    ;; 9. CI/CD (4/4)
    ;; ----------------------------------------------------------
    (ci-cd
     (status . "complete")
     (count . "4/4")
     (components
      ((name . "Quality Gates")
       (location . ".github/workflows/comprehensive-quality.yml")
       (purpose . "Multi-dimensional quality checks"))
      ((name . "CodeQL Analysis")
       (location . ".github/workflows/codeql.yml")
       (purpose . "Security scanning"))
      ((name . "OSSF Scorecard")
       (location . ".github/workflows/scorecard.yml")
       (purpose . "Supply chain security"))
      ((name . "RSR Compliance")
       (location . ".github/workflows/rsr-antipattern.yml")
       (purpose . "Rhodium Standard validation"))))))

;;; ============================================================
;;; ARCHITECTURE DECISIONS
;;; ============================================================

(define architecture-decisions
  '((adr-001
     (title . "RSR Compliance")
     (status . "accepted")
     (date . "2025-12-15")
     (context . "Project in the hyperpolymath ecosystem")
     (decision . "Follow Rhodium Standard Repository guidelines")
     (consequences . ("RSR Gold target"
                      "SHA-pinned actions"
                      "SPDX headers"
                      "Multi-platform CI")))

    (adr-002
     (title . "Guile Scheme as Implementation Language")
     (status . "accepted")
     (date . "2025-12-01")
     (context . "Need human-readable, homoiconic format")
     (decision . "Use Guile Scheme 3.0+ for library and format")
     (consequences . ("S-expression format"
                      "Native evaluation possible"
                      "GNU standard tooling"
                      "minikanren available")))

    (adr-003
     (title . "Dual License MIT/AGPL")
     (status . "accepted")
     (date . "2025-12-20")
     (context . "Balance permissive and copyleft options")
     (decision . "MIT OR AGPL-3.0-or-later (user choice)")
     (consequences . ("Maximum flexibility"
                      "SaaS protection via AGPL option"
                      "Palimpsest principles encouraged")))

    (adr-004
     (title . "minikanren Fallback")
     (status . "accepted")
     (date . "2025-12-06")
     (context . "minikanren may not be installed")
     (decision . "Include simplified fallback implementation")
     (consequences . ("Works without full minikanren"
                      "Limited unification in fallback"
                      "Graceful degradation")))))

;;; ============================================================
;;; DEVELOPMENT PRACTICES
;;; ============================================================

(define development-practices
  '((code-style
     (language . "Guile Scheme")
     (version . "3.0+")
     (formatter . "None standard")
     (linter . "guile -c"))

    (security
     (sast . "CodeQL")
     (supply-chain . "OSSF Scorecard")
     (secrets . "trufflehog")
     (credentials . "env vars only"))

    (testing
     (framework . "guile load tests")
     (coverage-target . 80)
     (integration . "just test"))

    (versioning
     (scheme . "SemVer 2.0.0")
     (current . "2.0.0"))

    (package-management
     (primary . "Guix")
     (fallback . "Nix flake")
     (forbidden . ("npm" "bun" "pip")))))

;;; ============================================================
;;; DESIGN RATIONALE
;;; ============================================================

(define design-rationale
  '((why-scheme
     "S-expressions provide minimal syntax, code-as-data philosophy,
      and direct evaluation capability. Guile is the GNU standard
      and provides excellent stability.")

    (why-minikanren
     "Relational logic programming enables declarative queries over
      project dependencies and status. 'Find all blocked projects
      depending on X' becomes a single goal expression.")

    (why-graphviz-mermaid
     "GraphViz provides publication-quality graphs. Mermaid enables
      embedding diagrams directly in Markdown documentation for
      GitHub/GitLab rendering.")

    (why-rsr
     "RSR ensures consistency, security, and maintainability across
      all hyperpolymath projects. Gold-tier compliance demonstrates
      commitment to quality.")))

;;; ============================================================
;;; LANGUAGE STACK
;;; ============================================================

(define language-stack
  '((primary
     (language . "Guile Scheme")
     (version . "3.0+")
     (purpose . "Core library and format"))

    (build
     (tools . ("just" "guix" "nix" "podman"))
     (automation . "justfile"))

    (documentation
     (format . "AsciiDoc")
     (processor . "asciidoctor"))

    (ci-cd
     (platform . "GitHub Actions")
     (containers . "Podman/Wolfi"))))

;;; ============================================================
;;; PROJECT STATUS SUMMARY
;;; ============================================================

(define project-status
  '((version . "2.0.0")
    (stability . "beta")
    (rsr-tier . "gold")
    (phase . "Phase 2 Complete, Phase 3 Planning")

    (phases
     ((phase-1 . "Foundation")
      (status . "complete")
      (completion . 100))
     ((phase-2 . "Smart Queries")
      (status . "complete")
      (completion . 100))
     ((phase-3 . "Automation")
      (status . "planned")
      (completion . 0))
     ((phase-4 . "Integration")
      (status . "future")
      (completion . 0)))))

;;; END META.scm
