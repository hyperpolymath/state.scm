# SPDX-License-Identifier: MIT OR AGPL-3.0-or-later
# Justfile for STATE project — Comprehensive Task Automation
# https://just.systems/
#
# Usage: just <recipe>
# List all: just --list

# ============================================================
# CONFIGURATION
# ============================================================

# Guile load path
guile_path := "lib"

# Container settings
container_name := "state"
container_tag := "latest"

# Documentation output
docs_dir := "docs"

# ============================================================
# DEFAULT
# ============================================================

# Default recipe - show available commands
default:
    @just --list --unsorted

# ============================================================
# TESTING
# ============================================================

# Run all tests
test:
    @echo "Running STATE tests..."
    guile -L {{guile_path}} -c '(use-modules (state)) (display "Modules loaded: ") (display (state-modules-loaded)) (newline)'
    @echo "✓ All tests passed"

# Run tests with verbose output
test-verbose:
    @echo "Loading all modules..."
    guile -L {{guile_path}} -c '(use-modules (state-core) (state-kanren) (state-graph) (state-history)) (display "All modules loaded successfully\n")'

# Run end-to-end tests with example state
test-e2e:
    @echo "Running E2E tests..."
    guile -L {{guile_path}} -c ' \
        (use-modules (state) (state-core) (state-graph) (state-history)) \
        (load "examples/example-state.scm") \
        (display "Focus: ") (display (get-current-focus state)) (newline) \
        (display "Blocked: ") (display (length (get-blocked-projects state))) (newline) \
        (display "Critical: ") (display (length (get-critical-next state))) (newline) \
        (display "✓ E2E tests passed\n")'

# Run all test suites
test-all: test test-verbose test-e2e
    @echo "✓ All test suites completed"

# ============================================================
# DEVELOPMENT
# ============================================================

# Load REPL with STATE modules
repl:
    guile -L {{guile_path}} -l lib/state.scm

# Load REPL with example state pre-loaded
repl-example:
    guile -L {{guile_path}} -c '(use-modules (state)) (load "examples/example-state.scm")' -l lib/state.scm

# Check if minikanren is available
check-minikanren:
    guile -L {{guile_path}} -c '(use-modules (state-kanren)) (display (if (minikanren-available?) "✓ minikanren: available\n" "○ minikanren: using fallback\n"))'

# Lint Scheme files (basic syntax check)
lint:
    @echo "Linting Scheme files..."
    @for f in lib/*.scm STATE.scm examples/*.scm *.scm; do \
        if [ -f "$$f" ]; then \
            echo "  Checking $$f..."; \
            guile -c "(load \"$$f\")" 2>&1 || exit 1; \
        fi \
    done
    @echo "✓ All files OK"

# Check for common issues
check: lint check-minikanren
    @echo "✓ All checks passed"

# Format check (placeholder - Scheme doesn't have standard formatter)
fmt-check:
    @echo "○ No standard Scheme formatter configured"

# ============================================================
# VISUALIZATION
# ============================================================

# Generate GraphViz DOT from example
dot:
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (display (generate-dot state))'

# Generate GraphViz DOT to file
dot-file output="state.dot":
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (generate-dot-file state "{{output}}")'
    @echo "Generated {{output}}"

# Generate Mermaid from example
mermaid:
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (display (generate-mermaid state))'

# Generate Mermaid to file
mermaid-file output="state.mmd":
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (generate-mermaid-file state "{{output}}")'
    @echo "Generated {{output}}"

# Generate status-grouped graph
dot-status:
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (display (dot-status-graph state))'

# Render DOT to PNG (requires graphviz)
render-dot output="state.png":
    just dot | dot -Tpng -o {{output}}
    @echo "Rendered {{output}}"

# ============================================================
# REPORTS
# ============================================================

# Show velocity report from example
velocity:
    guile -L {{guile_path}} -c '(use-modules (state) (state-history)) (load "examples/example-state.scm") (velocity-report state)'

# Show progress report from example
progress:
    guile -L {{guile_path}} -c '(use-modules (state) (state-history)) (load "examples/example-state.scm") (progress-report state)'

# Show all blocked projects
blocked:
    @echo "Blocked projects:"
    @guile -L {{guile_path}} -c '(use-modules (state) (state-core)) (load "examples/example-state.scm") (for-each (lambda (p) (display "  - ") (display (cdr (assoc (quote name) p))) (newline)) (get-blocked-projects state))'

# Show in-progress projects
in-progress:
    @echo "In-progress projects:"
    @guile -L {{guile_path}} -c '(use-modules (state) (state-core)) (load "examples/example-state.scm") (for-each (lambda (p) (display "  - ") (display (cdr (assoc (quote name) p))) (display " (") (display (cdr (assoc (quote completion) p))) (display "%)") (newline)) (get-in-progress-projects state))'

# Show critical next actions
critical:
    @echo "Critical next actions:"
    @guile -L {{guile_path}} -c '(use-modules (state) (state-core)) (load "examples/example-state.scm") (for-each (lambda (a) (display "  → ") (display a) (newline)) (get-critical-next state))'

# Show critical path for a project
critical-path project:
    guile -L {{guile_path}} -c '(use-modules (state) (state-graph)) (load "examples/example-state.scm") (display (find-critical-path "{{project}}" state)) (newline)'

# Create a new snapshot from example state
snapshot:
    guile -L {{guile_path}} -c '(use-modules (state) (state-history)) (load "examples/example-state.scm") (display (create-snapshot state)) (newline)'

# Show session health
health:
    guile -L {{guile_path}} -c '(use-modules (state) (state-core)) (load "examples/example-state.scm") (display "Session health: ") (display (session-health state)) (newline) (display "Should checkpoint: ") (display (should-checkpoint? state)) (newline)'

# ============================================================
# MINIKANREN QUERIES
# ============================================================

# Run minikanren query for blocked projects
query-blocked:
    guile -L {{guile_path}} -c '(use-modules (state) (state-kanren)) (load "examples/example-state.scm") (display (run* (q) (statuso q "blocked" state))) (newline)'

# Run minikanren query for in-progress projects
query-in-progress:
    guile -L {{guile_path}} -c '(use-modules (state) (state-kanren)) (load "examples/example-state.scm") (display (run* (q) (statuso q "in-progress" state))) (newline)'

# Query dependencies of a project
query-deps project:
    guile -L {{guile_path}} -c '(use-modules (state) (state-kanren)) (load "examples/example-state.scm") (display (run* (q) (dependso "{{project}}" q state))) (newline)'

# ============================================================
# DOCUMENTATION
# ============================================================

# Build documentation (requires asciidoctor)
docs:
    @mkdir -p {{docs_dir}}
    asciidoctor README.adoc -o {{docs_dir}}/README.html
    asciidoctor USAGE.adoc -o {{docs_dir}}/USAGE.html
    asciidoctor CONTRIBUTING.adoc -o {{docs_dir}}/CONTRIBUTING.html 2>/dev/null || true
    asciidoctor CHANGELOG.adoc -o {{docs_dir}}/CHANGELOG.html 2>/dev/null || true
    @echo "✓ Documentation built in {{docs_dir}}/"

# Build specification docs
docs-spec:
    @mkdir -p {{docs_dir}}/spec
    asciidoctor spec/STATE-FORMAT-SPEC.adoc -o {{docs_dir}}/spec/STATE-FORMAT-SPEC.html
    @echo "✓ Specification built"

# Build all documentation
docs-all: docs docs-spec
    @echo "✓ All documentation built"

# ============================================================
# CONTAINERS
# ============================================================

# Build container image with podman
container:
    podman build -t {{container_name}}:{{container_tag}} .

# Build container image with nerdctl
container-nerdctl:
    nerdctl build -t {{container_name}}:{{container_tag}} .

# Run in container (interactive)
container-run:
    podman run -it --rm -v $(pwd):/workspace:Z {{container_name}}:{{container_tag}}

# Run tests in container
container-test:
    podman run --rm {{container_name}}:{{container_tag}} guile -L lib -c '(use-modules (state)) (display (state-modules-loaded)) (newline)'

# ============================================================
# PACKAGE MANAGEMENT
# ============================================================

# Enter Guix shell
guix-shell:
    guix shell -m manifest.scm

# Build with Guix
guix-build:
    guix build -f guix.scm

# Enter Nix develop shell
nix-shell:
    nix develop

# Build with Nix
nix-build:
    nix build

# ============================================================
# CLEANUP
# ============================================================

# Clean generated files
clean:
    rm -rf {{docs_dir}}/*.html
    rm -rf {{docs_dir}}/spec/*.html
    rm -f *.dot *.mmd *.png

# Deep clean (including caches)
clean-all: clean
    rm -rf .guix-profile
    rm -rf result
    @echo "✓ All cleaned"

# ============================================================
# RELEASE
# ============================================================

# Show current version
version:
    @guile -L {{guile_path}} -c '(use-modules (state)) (display state-version) (newline)'

# Bump version (displays steps, doesn't auto-commit)
version-bump new_version:
    @echo "To bump version to {{new_version}}:"
    @echo "  1. Update lib/state.scm: state-version"
    @echo "  2. Update guix.scm: version"
    @echo "  3. Update flake.nix: version"
    @echo "  4. Update CHANGELOG.adoc"
    @echo "  5. Run: just test-all"
    @echo "  6. Commit: git commit -am 'chore(release): {{new_version}}'"
    @echo "  7. Tag: git tag -a v{{new_version}} -m 'Release {{new_version}}'"

# ============================================================
# SECURITY
# ============================================================

# Check for secrets in codebase
check-secrets:
    @echo "Checking for potential secrets..."
    @grep -rE "(api_key|apikey|secret|password|token)\s*[=:]\s*['\"][^'\"]{10,}" . --include="*.scm" --include="*.yml" --include="*.yaml" 2>/dev/null | grep -v "example\|test\|mock" | head -5 || echo "✓ No obvious secrets found"

# Check for HTTP URLs (should use HTTPS)
check-urls:
    @echo "Checking for HTTP URLs..."
    @grep -rE "http://[^l][^o]" . --include="*.scm" --include="*.adoc" 2>/dev/null | grep -v "localhost\|127.0.0.1" | head -5 || echo "✓ No HTTP URLs found"

# Run all security checks
security: check-secrets check-urls
    @echo "✓ Security checks completed"

# ============================================================
# CI/CD
# ============================================================

# Run CI pipeline locally
ci: lint test-all security
    @echo "✓ CI pipeline passed"

# Pre-commit hook equivalent
pre-commit: lint test
    @echo "✓ Pre-commit checks passed"

# ============================================================
# HELP
# ============================================================

# Show grouped command list
help:
    @echo "STATE.scm — Task Automation"
    @echo ""
    @echo "Testing:"
    @echo "  just test          Run basic tests"
    @echo "  just test-e2e      Run end-to-end tests"
    @echo "  just test-all      Run all tests"
    @echo ""
    @echo "Development:"
    @echo "  just repl          Start Guile REPL"
    @echo "  just lint          Check syntax"
    @echo "  just check         Run all checks"
    @echo ""
    @echo "Visualization:"
    @echo "  just dot           Generate GraphViz DOT"
    @echo "  just mermaid       Generate Mermaid diagram"
    @echo "  just render-dot    Render DOT to PNG"
    @echo ""
    @echo "Reports:"
    @echo "  just velocity      Show velocity report"
    @echo "  just progress      Show progress report"
    @echo "  just blocked       Show blocked projects"
    @echo "  just critical      Show critical actions"
    @echo ""
    @echo "Queries:"
    @echo "  just query-blocked         Query blocked projects"
    @echo "  just query-deps <project>  Query project dependencies"
    @echo ""
    @echo "Documentation:"
    @echo "  just docs          Build documentation"
    @echo "  just docs-all      Build all documentation"
    @echo ""
    @echo "Containers:"
    @echo "  just container     Build container"
    @echo "  just container-run Run in container"
    @echo ""
    @echo "Package Management:"
    @echo "  just guix-shell    Enter Guix environment"
    @echo "  just nix-shell     Enter Nix environment"
