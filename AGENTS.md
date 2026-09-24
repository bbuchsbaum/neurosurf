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
  `devtools::load_all()` or `devtools::install()`.
- Run tests: `devtools::test()` or
  `testthat::test_dir("tests/testthat")`.
- Build docs and site: `devtools::document()` then
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html).
- Check package before PRs: `devtools::check()` (aim for 0 errors,
  warnings, notes).

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

## Issue Tracking with Mote

This project uses **mote** for issue tracking. The store lives in
`.mote/`, which is machine-local and not committed. Beads/`bd` is
retired; `mote new` still mints `bd-*` ids, but those are mote issues.

| Command | Purpose |
|----|----|
| `mote ready` | List open issues with no open blockers (your next work) |
| `mote ls` | List open issues (`--all` includes closed) |
| `mote new "title" -p 1` | Create an issue (0 = highest priority, 3 = lowest) |
| `mote show <id>` | View issue details |
| `mote begin <id>` | Claim the issue, reserve paths, and mark it in progress |
| `mote note <id> --kind note "text"` | Append durable context (kinds: note, progress, decision, handoff, blocker) |
| `mote done <id>` | Add a completion note, close, and release the claim |
| `mote dep ...` | Manage blocking dependencies |
| `mote doctor` | Check the store if anything looks inconsistent |

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
    mote doctor
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
