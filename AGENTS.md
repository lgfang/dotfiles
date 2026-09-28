# AGENTS.md

This repository is my dotfiles repository using literate configuration.
Configurations are written in Org files (e.g. emacs.org, ai.org) and tangled
into actual configuration files.

- Modify Org files, not the tangled outputs, unless instructed otherwise.
- We run `setup-fireconnect.sh` to connect Claude to Fireworks AI LLMs. Each
  tangle overwrites the corresponding files, so it should be re-run to re-apply
  Fireworks settings to Claude.
- Commit messages: short lowercase prefix by topic (e.g. `emacs:`, `ai:`),
  imperative summary.
- Don't commit `.agent-shell/transcripts/` or other scratch state.
