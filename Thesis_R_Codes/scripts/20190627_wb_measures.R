load("I:/Thesis_Codes/Thesis_R_Codes/data/eps-0v5v1_a-0v6v0_r-3_g-0.3k-5.2k_Daan Pauwels_20190626_1904.RData")
r3.red <- brain_case
rm(brain_case)

# trying within/between partition measures --------------------------------

make_m.list <- function(m){
  m <- r3.red@now$mat.connectivity
  m.p1 <- m[1:50,1:50]
  m.p2 <- m[51:300,51:300]
  
  # removing the within group connections
  m.b <- m
  m.b[1:50,1:50] <- m.b[51:300,51:300] <- 0
  m.list <- list(m.p1= m.p1,
                m.p2 = m.p2,
                m.b = m.b,
                m.whole = m)
  m.list %>% return()
}


coefs.wb <- m.all %>%
  plyr::ldply(function(x) netmeas_coefs(x, x,
                                        concise = T,
                                        normalize.s = F))
