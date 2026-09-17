## A copy of run31 with the covariate BILI renamed BL_BILI: a covariate whose
## own name holds an underscore. Only a trailing _<integer> marks a binarized
## FREM level, so BL_BILI must stay one continuous covariate.
local_run31_underscore <- function(env = parent.frame()) {
  td <- withr::local_tempdir(.local_envir = env)
  src <- system.file("extdata/SimNeb", package = "PMXFrem")
  for (f in list.files(src, pattern = "^run31\\.", full.names = TRUE)) file.copy(f, td)
  mod <- file.path(td, "run31.mod")
  L <- readLines(mod)
  stopifnot(sum(grepl("\\bBILI\\b", L)) >= 2) # the FREM comment and $INPUT
  writeLines(gsub("\\bBILI\\b", "BL_BILI", L), mod)
  td
}
