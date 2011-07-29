basta <-
function(Data, ststart, stend,model = "SI", niter = 50000, burnin = 5001, thinning = 50, rptp = ststart, th.ini.pars = NULL, th.jumps = NULL, th.priors = NULL, Prop.Hazards = FALSE, ga.ini.pars = NULL, ga.jumps = NULL, ga.priors = NULL, nsim = 1, parallel = FALSE, ncpus = 2, lifetable=TRUE ) UseMethod("basta")

