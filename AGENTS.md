# Repository Guidelines

This document is a concise contributor guide for this repository. It explains how the project is organized, how to build and run it, and the conventions to follow when adding or changing code.

## Project Structure & Module Organization
- Backend (Haskell): `src/` and entry `app/Main.hs`.
  - Web/API: `MoeWeb/*` (routes, server, errors, types).
  - Core/env: `Moe/*` (environment, logging, Effectful effects like `ThirdParty`).
  - Third‑party clients: `Mikan/*`, `BgmTV/*`.
  - Utilities: `Effectful/*`, `Servant/*`, `Data/Types/*`.
- Frontend (web): `web/` (Vite/React + Bun).
- Build config: `bangumi-hs.cabal`, `fourmolu.yaml`.
- Schema/ops: `schema.lt.hcl`, `docs/`.

## Build, Test, and Development Commands
- Backend
  - `cabal build` — build the Haskell library and executable.
  - `cabal run moe-server` — run the API server locally.
  - `cabal test` — run tests (if present under `test/`).
  - `fourmolu --mode check src/ app/` (or `--mode inplace`) — format/check Haskell.
- Frontend
  - `cd web && bun install` — install dependencies.
  - `bun dev` — start dev server. `bun run build` — production build.

## Coding Style & Naming Conventions
- Haskell: GHC2024, `-Wall -Werror`, Fourmolu config in `fourmolu.yaml`.
- Modules use `TitleCase` and explicit exports; files mirror module paths.
- Types/constructors `TitleCase`, functions/values `camelCase`.
- Keep changes minimal and focused; avoid unrelated refactors.

## Testing Guidelines
- Prefer pure, testable functions; isolate effects behind interpreters.
- Use `tasty`/`tasty-hunit` under `test/` with modules named `*Spec.hs` mirroring `src/`.
- Run with `cabal test`. Add targeted tests for new logic.

## Commit & Pull Request Guidelines
- Commit subject: imperative mood, concise (<72 chars). Body explains motivation and approach; link issues.
- Include API changes, migrations, and screenshots for UI changes when relevant.
- Before opening a PR: format code, ensure `cabal build` and (if changed) `bun build` pass.

## Architecture Notes & Agent Tips
- Backend: Servant + Effectful + Warp. External calls go through `Moe.ThirdParty` (don’t call `runClientM` in handlers). Register new effects in `RouteEffects` and wire interpreters in `naturalTransform` in correct order.
- Config: read via `Moe.Environment.Config`/`Moe.Environment.Env`; avoid hardcoded ports/URLs.
- When adding modules, update `bangumi-hs.cabal` accordingly.
