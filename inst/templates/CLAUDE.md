# Drafting a studysynth spec

This folder holds a studysynth project: a REDCap project's structure (in `redcap/`), papers and manuals about the study's measures (in `sources/`), and `spec.yaml`, which describes an imaginary population of participants. Your job is to turn the sources into numbers in `spec.yaml`.

## Hard rules

- Never ask for, open, or read real participant data. studysynth only uses a project's structure and published or chosen parameters. If a file looks like it holds records (a REDCap export with data, a `ClinicalData` section, a CSV with one row per participant), stop and say so.
- Every number you add gets a `source:` on its block: a citation like `"Kroenke 2001, Table 3"`, or `"researcher"` if a person chose it. Leave `source: default` only on numbers you didn't change.
- After every edit, run the checker and fix every error before moving on:

  ```sh
  Rscript -e 'studysynth::check_spec_cli("spec.yaml", smoke = TRUE)'
  ```

## Workflow

Draft one block at a time, checking after each:

1. `measurement:` one block per scale. lavaan syntax with every loading fixed (`dep =~ 0.78*phq9_1 + ...`). Loadings are standardized. Reverse-keyed items get negative loadings. Thresholds are cut points on a standard normal latent response scale; an item with k choices needs k - 1 increasing thresholds. If a paper reports item means or endorsement rates rather than thresholds, choose thresholds that reproduce them (`qnorm()` of cumulative proportions is a good start).
2. `latents:` one block per latent variable, lme4 notation: `formula`, `fixef` (names must match the formula's coefficients, e.g. `rand_group2`, `time:rand_group2`), `ranef` (`sd`, and `cor` for two or more terms), `sigma`. `time` is in units of `design.time.unit_days`. Later latents can use earlier ones as predictors. Translate reported effects: a standardized group difference of d at the last visit is roughly `fixef[time:group] * time_at_last_visit = d`.
3. `screening:` eligibility in REDCap logic syntax, copied from the protocol.
4. `missingness:` withdrawal per event, missed visits, abandoned forms, skipped items, using the protocol's expected retention.
5. `ema:` schedule and compliance, if there is experience sampling.
6. `expect:` under each measurement block: values the sources report (alpha, mean_total, sd_total at an event), with tolerances. Then run the checker with plausibility:

   ```sh
   Rscript -e 'print(studysynth::check_spec("spec.yaml", plausibility = TRUE))'
   ```

## Useful commands

```r
library(studysynth)
project <- spec_project("spec.yaml")
project$fields                  # every field: form, type, choices, validation
project$events; project$mappings
x <- synthesize("spec.yaml", n = 50)
study_flow(x); plot_trajectories(x)
```
