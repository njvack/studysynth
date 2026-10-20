# studysynth 0.1.0

* First release.
* Reads a REDCap project's structure from the metadata-only project XML or the dictionary CSVs, and refuses files that contain records.
* `synthesize()` builds a fake study from a YAML spec: measurement models in lavaan syntax (including subscales and factor correlations), lme4-style latent trajectories, screening, withdrawal, missed visits, abandoned forms, skipped items, experience sampling, and open text.
* `check_spec()` checks a spec against the project; `check_spec_cli()` returns an exit status for git and Claude Code hooks.
* `edit_spec()` opens a Shiny editor with per-block previews.
* `score_scales()` and `write_scorify_scoresheet()` score scales and subscales the same way.
