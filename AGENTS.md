# Repository Guidelines

## Project Structure & Module Organization
- Core R sources live in `R/` (e.g., `neuro_surface.R`, `Searchlight.R`, `IO.R`).
- Tests are in `tests/testthat/`; integration entry point is `tests/testthat.R`.
- Documentation lives in `man/` and `vignettes/`; built site in `docs/`.
- Package metadata and config are at the repo root (`DESCRIPTION`, `NAMESPACE`, `_pkgdown.yml`).

## Build, Test, and Development Commands
- Install in development mode from the project root: `devtools::load_all()` or `devtools::install()`.
- Run tests: `devtools::test()` or `testthat::test_dir("tests/testthat")`.
- Build docs and site: `devtools::document()` then `pkgdown::build_site()`.
- Check package before PRs: `devtools::check()` (aim for 0 errors, warnings, notes).

## Coding Style & Naming Conventions
- Use base R style: 2 spaces, no tabs; limit lines to ~80 characters.
- Name exported functions in `snake_case`; S3 methods follow `generic.class`.
- Keep one main concept per file; update `Collate` in `DESCRIPTION` when adding R files.
- Prefer explicit imports (see `DESCRIPTION`) and avoid `:::` access.

## Testing Guidelines
- Place unit tests in `tests/testthat/` with names like `test_<topic>.R`.
- Follow `testthat` expectations and keep tests fast and deterministic.
- Add tests for new features and bug fixes; maintain or improve coverage.

## Commit & Pull Request Guidelines
- Write clear, imperative commit messages (e.g., “Add searchlight smoothing helper”).
- Reference related issues in commits/PRs when applicable (`#<issue-number>`).
- For PRs, describe motivation, key changes, and testing performed; include screenshots for UI/plot changes where helpful.

