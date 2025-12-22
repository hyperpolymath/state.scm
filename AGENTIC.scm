;; SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
;; SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell
;;; AGENTIC.scm — Agentic AI Patterns for STATE.scm
;;;
;;; Defines patterns for AI agents working with STATE files
;;; to enable autonomous project management capabilities.

(define-module (state.scm agentic)
  #:export (agentic-patterns))

(define agentic-patterns
  '((identity
     (name . "STATE.scm Agentic Patterns")
     (version . "1.0.0")
     (purpose . "AI agent interaction patterns"))

    ;;; ========================================================
    ;;; AGENT CAPABILITIES
    ;;; ========================================================
    (capabilities

     ((name . "context-restoration")
      (description . "Agent can restore project context from STATE.scm")
      (inputs . ("STATE.scm file"))
      (outputs . ("Reconstructed project knowledge"))
      (reliability . "high"))

     ((name . "progress-tracking")
      (description . "Agent can track and update project progress")
      (inputs . ("Current STATE" "Work completed"))
      (outputs . ("Updated STATE.scm"))
      (reliability . "high"))

     ((name . "blocker-detection")
      (description . "Agent can identify blocked projects and dependencies")
      (inputs . ("Project list with dependencies"))
      (outputs . ("List of blockers and critical path"))
      (reliability . "high"))

     ((name . "velocity-estimation")
      (description . "Agent can estimate completion dates from history")
      (inputs . ("Historical snapshots"))
      (outputs . ("Completion date estimates"))
      (reliability . "medium")))

    ;;; ========================================================
    ;;; AGENT WORKFLOWS
    ;;; ========================================================
    (workflows

     ((name . "session-start")
      (trigger . "User uploads STATE.scm")
      (steps
       ((step . 1)
        (action . "Parse STATE.scm file")
        (tool . "guile read"))
       ((step . 2)
        (action . "Extract current focus")
        (tool . "get-current-focus"))
       ((step . 3)
        (action . "Identify blocked projects")
        (tool . "get-blocked-projects"))
       ((step . 4)
        (action . "Load critical-next actions")
        (tool . "get-critical-next"))
       ((step . 5)
        (action . "Summarize context to user")
        (tool . "natural language"))))

     ((name . "session-end")
      (trigger . "User requests handover")
      (steps
       ((step . 1)
        (action . "Update completion percentages")
        (tool . "user confirmation"))
       ((step . 2)
        (action . "Record blockers discovered")
        (tool . "append to blockers"))
       ((step . 3)
        (action . "Update next actions")
        (tool . "reprioritize"))
       ((step . 4)
        (action . "Create history snapshot")
        (tool . "create-snapshot"))
       ((step . 5)
        (action . "Export STATE.scm")
        (tool . "serialize"))
       ((step . 6)
        (action . "Remind user to download")
        (tool . "natural language"))))

     ((name . "project-analysis")
      (trigger . "User asks about project status")
      (steps
       ((step . 1)
        (action . "Generate dependency graph")
        (tool . "generate-dot or generate-mermaid"))
       ((step . 2)
        (action . "Calculate velocity")
        (tool . "project-velocity"))
       ((step . 3)
        (action . "Estimate completion")
        (tool . "estimate-completion-date"))
       ((step . 4)
        (action . "Identify critical path")
        (tool . "find-critical-path"))
       ((step . 5)
        (action . "Present analysis")
        (tool . "natural language")))))

    ;;; ========================================================
    ;;; AGENT PROMPTS
    ;;; ========================================================
    (prompts

     ((name . "session-start-prompt")
      (template . "
I have loaded your STATE.scm file. Here's your current context:

**Current Focus**: {{current-project}} ({{current-phase}})
**Completion**: {{completion}}%

**Critical Next Actions**:
{{#each critical-next}}
- {{this}}
{{/each}}

**Blocked Projects**: {{blocked-count}}

What would you like to work on?"))

     ((name . "session-end-prompt")
      (template . "
I've prepared your STATE.scm handover.

**Updates This Session**:
- {{completion-delta}} progress on {{project-name}}
- {{blockers-added}} new blockers identified
- {{actions-completed}} actions completed

**IMPORTANT**: Download STATE.scm now. Upload it at the start of your next session.

Would you like me to generate the file?"))

     ((name . "velocity-report-prompt")
      (template . "
**Velocity Report for {{project-name}}**

Current Progress: {{completion}}%
Velocity: {{velocity}}%/day
Estimated Completion: {{estimated-date}}

{{#if velocity-declining}}
⚠️ Velocity is declining. Consider reviewing blockers.
{{/if}}")))

    ;;; ========================================================
    ;;; SAFETY CONSTRAINTS
    ;;; ========================================================
    (safety-constraints

     ((name . "no-autonomous-writes")
      (description . "Agent should not write to filesystem without user confirmation")
      (enforcement . "Always ask before saving"))

     ((name . "no-secret-exposure")
      (description . "Agent should not include secrets in STATE.scm")
      (enforcement . "Scan for secret patterns before serializing"))

     ((name . "user-confirms-progress")
      (description . "Completion updates require user confirmation")
      (enforcement . "Always verify percentages with user"))

     ((name . "preserve-history")
      (description . "Never delete history snapshots without explicit request")
      (enforcement . "History is append-only by default")))

    ;;; ========================================================
    ;;; HOOKS FOR AI TOOLS
    ;;; ========================================================
    (hooks

     ((event . "file-uploaded")
      (action . "Parse and validate STATE.scm")
      (on-success . "Load context")
      (on-failure . "Report parse error"))

     ((event . "session-timeout-warning")
      (action . "Prompt user for handover")
      (threshold . "10 messages remaining"))

     ((event . "blocker-detected")
      (action . "Add to blockers list")
      (notify . "Alert user to new blocker"))

     ((event . "milestone-reached")
      (action . "Create snapshot")
      (notify . "Celebrate with user")))))

;;; END AGENTIC.scm
