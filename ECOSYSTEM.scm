;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; ECOSYSTEM.scm — state.scm Ecosystem Positioning
;;;
;;; Defines how STATE.scm fits within the hyperpolymath ecosystem
;;; and its relationships with other projects.

(define-module (state.scm ecosystem)
  #:export (ecosystem-definition))

(define ecosystem-definition
  '((identity
     (name . "STATE.scm")
     (version . "2.0.0")
     (type . "infrastructure")
     (tier . "foundation")
     (language . "Guile Scheme"))

    ;;; --------------------------------------------------------
    ;;; POSITION IN ECOSYSTEM
    ;;; --------------------------------------------------------
    (position-in-ecosystem
     (description . "AI conversation persistence layer for hyperpolymath projects")
     (layer . "infrastructure")
     (dependencies . ())  ; STATE is a foundation - depends on nothing else
     (dependents . ("UPM" "Echomesh" "Claude-workflows")))

    ;;; --------------------------------------------------------
    ;;; ECOSYSTEM RELATIONSHIPS
    ;;; --------------------------------------------------------
    (relationships

     ;; RSR - The standard we follow
     ((project . "rhodium-standard-repositories")
      (url . "https://github.com/hyperpolymath/rhodium-standard-repositories")
      (relationship . "standard")
      (direction . "follows")
      (notes . "STATE.scm targets RSR Gold tier compliance"))

     ;; Echomesh - Future integration
     ((project . "Echomesh")
      (url . "https://github.com/hyperpolymath/echomesh")
      (relationship . "integration")
      (direction . "provides-to")
      (notes . "Echomesh will use STATE for automatic conversation persistence"))

     ;; UPM - Future integration
     ((project . "UPM")
      (url . "https://github.com/hyperpolymath/upm")
      (relationship . "integration")
      (direction . "provides-to")
      (notes . "Universal Project Manager will sync with STATE.scm"))

     ;; Palimpsest - Philosophy and license
     ((project . "palimpsest-license")
      (url . "https://github.com/hyperpolymath/palimpsest-license")
      (relationship . "philosophy")
      (direction . "follows")
      (notes . "Palimpsest principles encouraged but not required")))

    ;;; --------------------------------------------------------
    ;;; WHAT THIS IS
    ;;; --------------------------------------------------------
    (what-this-is
     ("A checkpoint/restore system for AI conversations"
      "A Guile Scheme library for parsing and querying project state"
      "A format specification for declarative project context"
      "A foundation for AI-assisted project management"
      "An RSR Gold-tier compliant project"))

    ;;; --------------------------------------------------------
    ;;; WHAT THIS IS NOT
    ;;; --------------------------------------------------------
    (what-this-is-not
     ("NOT a general-purpose database"
      "NOT a real-time sync system (yet)"
      "NOT specific to any single AI provider"
      "NOT a replacement for proper version control"
      "NOT exempt from RSR compliance"))

    ;;; --------------------------------------------------------
    ;;; INTEGRATION POINTS
    ;;; --------------------------------------------------------
    (integration-points
     ((type . "file-format")
      (format . "S-expression")
      (extension . ".scm")
      (mime-type . "application/x-state+scheme"))

     ((type . "guile-module")
      (module . "(state)")
      (load-path . "lib/"))

     ((type . "json-binding")
      (spec . "spec/schema/state.schema.json")
      (purpose . "Web/API integration"))

     ((type . "container")
      (image . "state:latest")
      (registry . "OCI-compatible")))

    ;;; --------------------------------------------------------
    ;;; ECOSYSTEM COMMANDS
    ;;; --------------------------------------------------------
    (commands
     ((just . ("test" "repl" "dot" "mermaid" "velocity" "progress"))
      (guix . ("shell -m manifest.scm" "build -f guix.scm"))
      (nix . ("develop" "build"))
      (container . ("podman build" "podman run"))))))

;;; END ECOSYSTEM.scm
