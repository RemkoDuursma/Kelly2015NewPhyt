
# Fit single A-Ci curve to each species
fpil <- fitaci(pilaci)
fpop <- fitaci(popaci)


# Bootstrap the A-Ci curve
# Note: this only works if we have a large sample size for the A-Ci curve!
# Takes a while - only run if cache does not exist.
# If data have changed make sure to delete cache manually.
cf <- "cache/bootaci_results.rdata"
if(!file.exists(cf)){
  bpil <- bootaci(pilaci,999)
  bpop <- bootaci(popaci,999)
  save(bpil, bpop, file=cf)
} else {
  load(cf)
}
bpop <- bpop[!sapply(bpop, inherits, "try-error")]


# Get CI on transition point, using bootstrap again.
cit_pil <- sapply(bpil, "[[", "Ci_transition")
cit_pop <- sapply(bpop, "[[", "Ci_transition")

quantile(cit_pil, probs=c(0.025, 0.5, 0.975))
quantile(cit_pop, probs=c(0.025, 0.5, 0.975))
