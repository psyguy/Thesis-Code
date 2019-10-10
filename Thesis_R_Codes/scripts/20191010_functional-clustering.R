load("./data-pc/5200-edges/life-10_eps-0v6v0_a-1v5v0_r-7_g-0.3k-5.2k_Ruben Declercq_20190709_2253.RData")
load("./data-pc/5200-edges/life-10_eps-1v5v0_a-0v6v0_r-8_g-0.3k-5.2k_Margaux Bogaert_20190709_2221.RData")
load("./data-pc/5200-edges/life-10_eps-1v5v0_a-0v6v0_r-9_g-0.3k-5.2k_Hannah Bauwens_20190709_2225.RData")

# load("./data/snp.lean_all_5k_20190713_1356.RData")

a <- brain_case@now$activities %>% t()
brain_case@parameters$params.eps_a
d <- a %>% dist() %>% as.matrix()




s <- d %>% seriate()
pimage(d,s)



c.d <- cluster::diana(d)
pltree(c.d, cex = 0.6, hang = -1,
       main = "activities")
rect.hclust(c.d, k = 5, border = 2:5)


fviz_dist(d, gradient = list(low = "white", high = "black"))


m <- brain_case@now$mat.connectivity
s <- m %>% seriate()

