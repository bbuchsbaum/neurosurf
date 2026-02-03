# Repository Guidelines

## Project Structure & Module Organization

- Core R sources live in `R/` (e.g., `neuro_surface.R`, `Searchlight.R`,
  `IO.R`).
- Tests are in `tests/testthat/`; integration entry point is
  `tests/testthat.R`.
- Documentation lives in `man/` and `vignettes/`; built site in `docs/`.
- Package metadata and config are at the repo root (`DESCRIPTION`,
  `NAMESPACE`, `_pkgdown.yml`).

## Build, Test, and Development Commands

- Install in development mode from the project root:
  [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html)
  or
  [`devtools::install()`](https://devtools.r-lib.org/reference/install.html).
- Run tests:
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html)
  or `testthat::test_dir("tests/testthat")`.
- Build docs and site:
  [`devtools::document()`](https://devtools.r-lib.org/reference/document.html)
  then
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).
- Check package before PRs:
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
  (aim for 0 errors, warnings, notes).

## Coding Style & Naming Conventions

- Use base R style: 2 spaces, no tabs; limit lines to ~80 characters.
- Name exported functions in `snake_case`; S3 methods follow
  `generic.class`.
- Keep one main concept per file; update `Collate` in `DESCRIPTION` when
  adding R files.
- Prefer explicit imports (see `DESCRIPTION`) and avoid `:::` access.

## Testing Guidelines

- Place unit tests in `tests/testthat/` with names like
  `test_<topic>.R`.
- Follow `testthat` expectations and keep tests fast and deterministic.
- Add tests for new features and bug fixes; maintain or improve
  coverage.

## Commit & Pull Request Guidelines

- Write clear, imperative commit messages (e.g., “Add searchlight
  smoothing helper”).
- Reference related issues in commits/PRs when applicable
  (`#<issue-number>`).
- For PRs, describe motivation, key changes, and testing performed;
  include screenshots for UI/plot changes where helpful.

## Issue Tracking with Beads

This project uses **beads** (`bd`) for git-backed issue tracking. See
<https://github.com/steveyegge/beads>

### Essential Commands

| Command | Purpose |
|----|----|
| `bd ready` | List tasks without blockers (your next work) |
| `bd create "title" -p 1` | Create task (P0=critical, P1=high, P2=medium, P3=low) |
| `bd show <id>` | View issue details and history |
| `bd update <id> --status in_progress` | Mark task as in progress |
| `bd close <id> --reason "text"` | Close completed task |
| `bd dep add <child> <parent>` | Add dependency |
| `bd list --json` | List all open issues |
| `bd sync` | Force sync to git |

### Critical Rules for Agents

1.  **NEVER use `bd edit`** - it opens an interactive editor. Use
    flag-based updates:

    ``` bash
    bd update <id> --description "new description"
    bd update <id> --title "new title"
    ```

2.  **Always use `--json` flag** for programmatic access

3.  **Run `bd sync` after changes** to ensure immediate git sync

### Finding Work

``` bash
bd ready --json          # Tasks without blockers
bd list --status open    # All open tasks
bd stale --days 7        # Neglected tasks
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work
is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1.  **File issues for remaining work** - Create issues for anything that
    needs follow-up

2.  **Run quality gates** (if code changed) - Tests, linters, builds

3.  **Update issue status** - Close finished work, update in-progress
    items

4.  **PUSH TO REMOTE** - This is MANDATORY:

    ``` bash
    git pull --rebase
    bd sync
    git push
    git status  # MUST show "up to date with origin"
    ```

5.  **Clean up** - Clear stashes, prune remote branches

6.  **Verify** - All changes committed AND pushed

7.  **Hand off** - Provide context for next session

**CRITICAL RULES:** - Work is NOT complete until `git push` succeeds -
NEVER stop before pushing - that leaves work stranded locally - NEVER
say “ready to push when you are” - YOU must push - If push fails,
resolve and retry until it succeeds

## MCP Agent Mail: coordination for multi-agent workflows

What it is - A mail-like layer that lets coding agents coordinate
asynchronously via MCP tools and resources. - Provides identities,
inbox/outbox, searchable threads, and advisory file reservations, with
human-auditable artifacts in Git.

Why it’s useful - Prevents agents from stepping on each other with
explicit file reservations (leases) for files/globs. - Keeps
communication out of your token budget by storing messages in a
per-project archive. - Offers quick reads (`resource://inbox/...`,
`resource://thread/...`) and macros that bundle common flows.

How to use effectively 1) Same repository - Register an identity: call
`ensure_project`, then `register_agent` using this repo’s absolute path
as `project_key`. - Reserve files before you edit:
`file_reservation_paths(project_key, agent_name, ["src/**"], ttl_seconds=3600, exclusive=true)`
to signal intent and avoid conflict. - Communicate with threads: use
`send_message(..., thread_id="FEAT-123")`; check inbox with
`fetch_inbox` and acknowledge with `acknowledge_message`. - Read fast:
`resource://inbox/{Agent}?project=<abs-path>&limit=20` or
`resource://thread/{id}?project=<abs-path>&include_bodies=true`. - Tip:
set `AGENT_NAME` in your environment so the pre-commit guard can block
commits that conflict with others’ active exclusive file reservations.

2.  Across different repos in one project (e.g., Next.js frontend +
    FastAPI backend)
    - Option A (single project bus): register both sides under the same
      `project_key` (shared key/path). Keep reservation patterns
      specific (e.g., `frontend/**` vs `backend/**`).
    - Option B (separate projects): each repo has its own `project_key`;
      use `macro_contact_handshake` or
      `request_contact`/`respond_contact` to link agents, then message
      directly. Keep a shared `thread_id` (e.g., ticket key) across
      repos for clean summaries/audits.

Macros vs granular tools - Prefer macros when you want speed or are on a
smaller model: `macro_start_session`, `macro_prepare_thread`,
`macro_file_reservation_cycle`, `macro_contact_handshake`. - Use
granular tools when you need control: `register_agent`,
`file_reservation_paths`, `send_message`, `fetch_inbox`,
`acknowledge_message`.

Common pitfalls - “from_agent not registered”: always `register_agent`
in the correct `project_key` first. - “FILE_RESERVATION_CONFLICT”:
adjust patterns, wait for expiry, or use a non-exclusive reservation
when appropriate. - Auth errors: if JWT+JWKS is enabled, include a
bearer token with a `kid` that matches server JWKS; static bearer is
used only when JWT is disabled.
