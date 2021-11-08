# You can modify the PR comment header here. You can use github markdown e.g.
# emojis like :tada:.
# This file will be parsed and evaluate within the context of
# `benchmarks_analyze` and should return the comment text as the last value.
# Available variables for glue substitution:
# * ci: confidence interval
# * refs: BASE and HEAD refs benchmarked against each other.
# See `?touchstone::pr_comment`
glue::glue(
  "This is how benchmark results would change (along with a",
  " {100 * ci}% confidence interval in relative change) if ",
  "{system2('git', c('rev-parse', 'HEAD'), stdout = TRUE)} is merged into {refs[1]}:\n"
)
