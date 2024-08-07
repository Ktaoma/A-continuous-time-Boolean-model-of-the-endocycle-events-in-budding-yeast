library(dplyr)
library(tidyverse)
library(data.table)
library(pheatmap)
library(stringr)


df <- fread("../../data/Fig7.csv")[,c(2:4)]
df$A <- paste0(df$A,"∆")
df$B <- paste0(df$B,"∆")
d <- reshape(df, idvar = "A", timevar = "B", direction = "wide") %>% as.data.frame()
d2 <- d %>% select(-A)
rownames(d2) <- d$A
colnames(d2) <- sub("predict.", "", colnames(d2))

eno_order <- c("cln3∆", "clb2G-M∆", "cdc5∆", "mcm1∆", "cdc20∆", "cdc14∆", 
               "whi5∆","sbf∆", "mbf∆", "cln2∆", "clb5∆",
               "swi5∆", "cdh1∆", "sic1∆", "nrm1∆")


d3 <- d2[eno_order]
d4 <- d3[order(match(rownames(d3), eno_order)), , drop = FALSE]
d2 <- d4

order_ = colnames(d2)
known_delete <- c("whi5_whi5","sic1_sic1","sbf_sbf",
                  "mbf_mbf","cdc5_cdc5","mcm1_mcm1",
                  "cdc20_cdc20","cdc14_cdc14","swi5_swi5",
                  "cdh1_cdh1","nrm1_nrm1",
                  "cln2_cln2","cln3_cln3","clb5_clb5",
                  "clb2G-M_clb2G-M",
                  "sic1_cln3","cdh1_cln2","clb5_cln2",
                  "sic1_clb2M","mbf_sbf","cdh1_swi5",
                  "cdc20_clb5","sic1_cdh1","clb2G-M_clb5")

df_cat = df
df_cat$cat <- paste(df_cat$A %>% str_replace("∆", ""),"_",df_cat$B %>% str_replace("∆", ""),sep = "")
df_cat$predict <- ifelse(df_cat$cat %in% known_delete,"•",
                         ifelse(df_cat$cat %in% c("whi5_cln3"),"x",""))
df_an = df_cat[,c(1:3)] %>% dcast(A~B) %>% as.data.frame() %>%
  arrange(match(A,order_))
df_an2 = df_an[,-1]
rownames(df_an2) <-df_an[,1]
df_an2 <- df_an2[, order_]

d2[upper.tri(d2)] <- NA

newnames <- lapply(
  rownames(d2),
  function(x) bquote(italic(.(x))))

d2 %>% pheatmap(cluster_rows = FALSE,cluster_cols = FALSE,
                color = c("steelblue","wheat4"),border_color ="white",
                display_numbers = df_an2,
                fontsize_number = 30,na_col = "white",
                number_color = "white",
                labels_col = as.expression(newnames),
                labels_row = as.expression(newnames),
                filename="Fig7.png")
