# Project Structure

This document defines the intended root-level structure for local development and repository hygiene.

## Root directories

- `.mvn/`: Maven wrapper internals.
- `bridge/`: Optional local device bridge implementations (printer/cash drawer integration).
- `docs/`: Project documentation and non-production design/reference materials.
- `logs/`: Local runtime/debug output. Not committed.
- `node_modules/`: Frontend dependencies (generated). Not committed.
- `scripts/`: Reusable automation scripts for setup and development workflows (`scripts/dev`, `scripts/tools`).
- `src/`: Application source code and resources.
- `target/`: Maven build outputs (generated). Not committed.

## Root files

- `mvnw`, `mvnw.cmd`: Maven wrapper entry points.
- `pom.xml`: Backend build configuration.
- `package.json`, `package-lock.json`, `tailwind.config.js`, `postcss.config.js`: Frontend tooling configuration.
- `.gitignore`, `.gitattributes`: Repository behavior and ignore rules.
- `README.md`: Main onboarding and run instructions.

## Placement rules

- Keep the repository root limited to build/configuration entry points.
- Put ad-hoc HTML prototypes and UI snapshots under `docs/` (for example `docs/prototypes/`), not in root.
- Write runtime output (`stdout`, debug dumps, temporary logs) under `logs/`.
- Do not commit generated outputs (`target/`, `node_modules/`, runtime uploads, logs).
- Keep reusable automation in `scripts/`; place maintenance helpers in `scripts/tools/`.

## Docs conventions

- `docs/architecture/`: structure docs, design proposals, migration plans.
- `docs/features/`: feature-level technical specifications.
- `docs/operations/`: setup, runbooks, and troubleshooting.
- `docs/prototypes/`: non-production HTML/UI experiments.
