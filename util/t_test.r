
# PA data ---------------------------------------------------------------------
PA <- read_csv("./data/PA_site.csv")


pairwise.t.test(log(PA$LMAs),PA$site_strata,p.adj="none")

