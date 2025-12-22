;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; PLAYBOOK.scm — state.scm Development Playbook
;;;
;;; Operational procedures, workflows, and decision trees
;;; for developing and maintaining STATE.scm

(define-module (state.scm playbook)
  #:export (playbook-definition))

(define playbook-definition
  '((identity
     (name . "STATE.scm Playbook")
     (version . "1.0.0")
     (purpose . "Operational procedures for STATE development"))

    ;;; ========================================================
    ;;; DAILY OPERATIONS
    ;;; ========================================================
    (daily-operations

     ((name . "development-setup")
      (when . "Starting work session")
      (steps
       ("Enter development environment: guix shell -m manifest.scm"
        "Verify tests pass: just test"
        "Check for updates: git pull"
        "Review open issues if any")))

     ((name . "end-of-session")
      (when . "Finishing work session")
      (steps
       ("Run full test suite: just test"
        "Commit changes with descriptive message"
        "Update STATE.scm if significant work done"
        "Push to remote")))

     ((name . "pre-commit")
      (when . "Before each commit")
      (steps
       ("Run linter: just lint"
        "Run tests: just test"
        "Check for secrets: git secrets --scan (if installed)"
        "Review diff for accidental inclusions"))))

    ;;; ========================================================
    ;;; RELEASE PROCEDURES
    ;;; ========================================================
    (release-procedures

     ((name . "version-bump")
      (when . "Preparing new release")
      (steps
       ("Update version in lib/state.scm"
        "Update version in guix.scm"
        "Update version in flake.nix"
        "Update CHANGELOG.adoc"
        "Update PROJECT-STATUS files"
        "Run: just test"
        "Commit: 'chore(release): bump to vX.Y.Z'"
        "Tag: git tag -a vX.Y.Z -m 'Release X.Y.Z'")))

     ((name . "release-checklist")
      (when . "Before publishing release")
      (checklist
       ("[ ] All tests pass"
        "[ ] CHANGELOG updated"
        "[ ] Version numbers consistent"
        "[ ] SECURITY.md current"
        "[ ] No TODOs in critical paths"
        "[ ] Container builds successfully"
        "[ ] Documentation reviewed"))))

    ;;; ========================================================
    ;;; DECISION TREES
    ;;; ========================================================
    (decision-trees

     ((name . "new-feature")
      (question . "Adding a new feature?")
      (branches
       ((condition . "Affects core query API")
        (action . "Requires ADR and version bump"))
       ((condition . "Adds new visualization")
        (action . "Add to state-graph.scm"))
       ((condition . "Changes file format")
        (action . "Update spec/, bump major version"))
       ((condition . "New utility function")
        (action . "Add to appropriate lib/*.scm"))))

     ((name . "bug-fix")
      (question . "Fixing a bug?")
      (branches
       ((condition . "Security issue")
        (action . "Follow SECURITY.md disclosure process"))
       ((condition . "Data corruption possible")
        (action . "Patch release immediately"))
       ((condition . "Cosmetic/minor")
        (action . "Include in next regular release"))))

     ((name . "dependency-choice")
      (question . "Need external dependency?")
      (branches
       ((condition . "Guile module available")
        (action . "Add to manifest.scm"))
       ((condition . "Can implement fallback")
        (action . "Implement fallback like minikanren"))
       ((condition . "Must use external tool")
        (action . "Make optional, document in README")))))

    ;;; ========================================================
    ;;; TROUBLESHOOTING
    ;;; ========================================================
    (troubleshooting

     ((problem . "Guile module not found")
      (symptoms . ("Module not found error" "Use-modules fails"))
      (solutions
       ("Check GUILE_LOAD_PATH includes lib/"
        "Run: export GUILE_LOAD_PATH=$PWD/lib:$GUILE_LOAD_PATH"
        "Use: guile -L lib")))

     ((problem . "minikanren not working")
      (symptoms . ("minikanren-available? returns #f"))
      (solutions
       ("Install guile-minikanren package"
        "Or use fallback (automatically enabled)"
        "Check: just check-minikanren")))

     ((problem . "Container build fails")
      (symptoms . ("Podman/nerdctl build error"))
      (solutions
       ("Check Containerfile syntax"
        "Ensure base image available"
        "Check network access for apt"))))

    ;;; ========================================================
    ;;; COMMAND QUICK REFERENCE
    ;;; ========================================================
    (command-reference
     ((category . "development")
      (commands
       (("just test" . "Run all tests")
        ("just repl" . "Start interactive REPL")
        ("just lint" . "Check syntax"))))

     ((category . "visualization")
      (commands
       (("just dot" . "Generate GraphViz")
        ("just mermaid" . "Generate Mermaid")
        ("just velocity" . "Show velocity report")
        ("just progress" . "Show progress report"))))

     ((category . "environment")
      (commands
       (("guix shell -m manifest.scm" . "Enter Guix shell")
        ("nix develop" . "Enter Nix shell")
        ("just container" . "Build container")
        ("just container-run" . "Run in container")))))))

;;; END PLAYBOOK.scm
