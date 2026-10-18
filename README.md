# studysynth

Make fake participants for your REDCap study, so you can write and test your analysis code (or let an LLM help you write it) before anyone real has enrolled, and without ever showing a model a real record.

You give studysynth your REDCap project's *structure* and a YAML spec saying how your measures behave. It gives you as many imaginary participants as you want, in REDCap import format: screen failures, dropouts, half-finished surveys, experience sampling bursts, and all.

**The one rule:** studysynth never reads real subject data. It reads the metadata-only project XML or the data dictionary CSVs, and refuses a project XML that contains records. Every number in a spec comes from a paper, a manual, or a person.

## Install

```r
# install.packages("pak")
pak::pak("njvack/studysynth")
```

## A one-minute tour

The package ships with Calm Minds, an invented mindfulness trial for depression: PHQ-9 screening, baseline, a two-week experience sampling burst, and three follow-ups.

```r
library(studysynth)

spec <- example_path("calm_minds.yaml")
check_spec(spec)
#> v Spec looks good. Go make some imaginary people.

fake <- synthesize(spec)
#> i Inventing 120 participants for Calm Minds. None of them are real.

study_flow(fake)
#> Screened          286
#> |- Ineligible     142
#> |- Declined        24
#> `- Enrolled       120
#>    |- Withdrew     29  (ema_arm_1: 11, followup_arm_1: 5, week4_arm_1: 8, week8_arm_1: 5)
#>    `- Completed    91

plot_trajectories(fake, "phq9")
write_redcap_csv(fake, "calm_minds_fake.csv")
```

Things to notice in the fake data, because they happen in real data too: baseline PHQ-9 scores drop at week 4 in *both* groups (people were screened in for being high, so they regress toward the mean), baseline alpha is lower than published alphas (screening restricts the range), and people with more depressed moods drop out and skip prompts more often (because the spec says so).

## Your own study

```r
use_studysynth("my-fake-study", xml = "MyStudy_metadata.xml")
```

In REDCap, the XML is under **Other Functions > Download metadata only (XML)**. You can also pass the data dictionary, events, instrument designations, and repeating instruments CSVs instead.

That makes a folder with your REDCap file, a deliberately boring `spec.yaml` drafted from the dictionary (every number tagged `source: default`), a `sources/` folder for your scales' papers, and a `CLAUDE.md` telling an LLM how to fill in the spec from those papers. Then:

1. Put the papers for your measures in `sources/`.
2. Ask an LLM (Claude Code works well here) to fill in `spec.yaml` from them. It sees your project's structure and the papers, never your data. Or edit the spec yourself.
3. Tune it by eye: `edit_spec("spec.yaml")` opens a Shiny editor. Pick a block from the list on the left (searchable, with error and warning counts, so 30 measures stay manageable), edit its YAML, and see that block's issues and a preview aimed at it: item response bars, alpha and means by event for a measure, true trajectories for a latent, compliance for experience sampling, retention for missingness. Comments in the file survive editing.
4. `synthesize("spec.yaml")`, write your analysis code against the result, and run the same code on real data later.

## The spec

One YAML file. Each block uses a format you've probably seen before:

| Block | What it says | Format |
|---|---|---|
| `redcap` | Where the project files are | File paths |
| `design` | How many people, groups, start date, time units | Plain values |
| `covariates` | Person-level variables like age | `{dist: normal, mean: 31, sd: 11}` |
| `measurement` | How items relate to latent variables | lavaan syntax with fixed loadings, plus thresholds |
| `latents` | How people differ and change over time | lme4 formula, `fixef`, `ranef` (sd, cor), `sigma` |
| `ema` | Experience sampling schedule and compliance | Days, prompts per day, window, response rates |
| `screening` | Who gets in | REDCap logic, e.g. `[age] >= 18 and [phq9_total] >= 10` |
| `missingness` | Dropout, missed visits, abandoned forms, skipped items | Probabilities |
| `open_text` | Free-text answers | Canned phrases, or an LLM via ellmer |

See [`calm_minds.yaml`](inst/examples/calm_minds/calm_minds.yaml) for a commented example of every block.

A scale with subscales, like the PANAS, is one measurement block with one factor per subscale. Factor correlations like `pa ~~ -0.2*na` correlate people's typical levels (their random intercepts) on those latents. `score_scales()`, `plot_trajectories()`, the editor, and `expect:` all work per subscale, scoring each item toward the factor it loads on most, reversed if that loading is negative.

`write_scorify_scoresheet(spec, "scoresheet.csv")` writes a [scorify](https://github.com/njvack/scorify) scoresheet that scores a REDCap export the same way `score_scales()` does, so the scoring you test on fake data is the scoring you run on real data.

How it fits together: each latent variable (say, `dep`) gets a value at every occasion from its mixed model. Item responses at a visit come from the latent values through the measurement model, cut at thresholds. Fields the spec doesn't mention get plausible defaults from their type and validation; calculated fields and branching logic are evaluated like REDCap would.

Variables your `latents` formulas can use: `time` (days since day 0, in `design.time.unit_days` units), `day`, `hour`, `sin_tod` and `cos_tod` (time of day), `weekend`, `ema` (1 at prompts), covariates, the group field (as a factor, so coefficients look like `rand_group2`), and any latent listed above the current one.

Missing data is applied in this order: abandoned forms and skipped items, calculated fields, screening (so a half-finished screening form can make someone ineligible), withdrawal, missed visits, then branching logic.

## How big can it go?

A test study with 30 ten-item measures, an EMA burst, and 1,500 enrolled participants (about 3,700 screened) synthesizes in about 30 seconds and produces 61,000 rows by 352 columns. Checking that spec takes about 1.5 seconds. The editor previews 150 participants by default (about 4 seconds for that study); change it in the editor's Settings tab.

## The checker

`check_spec()` is deterministic and returns a tibble of issues, each with a path into the spec and a hint. It checks the spec's shape, every field, form, and event against the REDCap project, that lavaan models parse with fixed loadings and possible covariances, that `fixef` names match the formula's actual coefficients (the classic `rand_group2` vs `rand_groupmindfulness` mixup), that random-effect matrices are valid, and that 20 people can be synthesized. With `plausibility = TRUE` it simulates 500 people and compares against `expect:` values from your sources, like alpha or a baseline mean.

It runs wherever a spec changes or gets used:

- `synthesize()` refuses to run on a spec with errors.
- The Shiny editor checks on every keystroke (well, every pause).
- The project `.Rprofile` checks when R starts in the folder.
- `.githooks/pre-commit` blocks commits with errors (`git config core.hooksPath .githooks`).
- `.claude/settings.json` checks when a Claude Code session starts and after every file edit, sending errors straight back to the model.

## Not yet

This is an experiment. Things the design anticipates but v0 doesn't do: multiple arms (one arm per spec), IRT parameters (mirt-style) as an alternative to loadings, EMA stored as fixed events like `day01_p1`, lagged effects between EMA variables, people differing in volatility, page-level survey abandonment and survey timestamps, a Frictionless `datapackage.json`, and exposing the checker as ellmer or MCP tools. The project XML parser follows REDCap's format as best we know it and hasn't been tested against a real export yet. Calculated fields are included in the output for convenience; drop them if a REDCap import complains.
