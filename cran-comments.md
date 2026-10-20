## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

* GitHub Actions: ubuntu-latest (devel, release, oldrel-1), macOS-latest (release), windows-latest (release)
* win-builder (devel)

## Notes for reviewers

* `use_studysynth()` writes `.Rprofile`, `.githooks/pre-commit`, and `.claude/settings.json` only inside the project folder the user names; it never writes to the user's home directory or working directory by default.
* The optional open-text feature caches language model responses under `tools::R_user_dir("studysynth", "cache")`. Examples and tests never use it.
