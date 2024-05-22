library(dplyr)
library(tidyverse)
library(data.table)
library(pheatmap)
library(stringr)

###  endoreplication ##
df <- fread("../../data/SFig1A.csv")[,c(2:4)] %>% filter(A != 'clb2G-M' & B != 'clb2G-M') 
df[df == 'clb5'] <- 'clb6'
df$A <- paste0(df$A,"∆")
df$B <- paste0(df$B,"∆")
d <- reshape(df, idvar = "A", timevar = "B", direction = "wide") %>% as.data.frame()
d2 <- d %>% select(-A)
rownames(d2) <- d$A
colnames(d2) <- sub("predict.", "", colnames(d2))
eno_order <- c("cdh1∆","cln2∆","cln3∆","clb6∆","mbf∆","nrm1∆","sbf∆","cdc20∆",
               "cdc5∆","cdc14∆","mcm1∆","sic1∆","swi5∆","whi5∆")
d3 <- d2[eno_order]
d4 <- d3[order(match(rownames(d3), eno_order)), , drop = FALSE]
d2 <- d4
order_ = colnames(d2)
known_delete <- c("nrm1_nrmss1")
df_cat = df
df_cat$cat <- paste(df_cat$A %>% str_replace("∆", ""),"_",df_cat$B%>% str_replace("∆", ""),sep = "")
df_cat$predict <- ifelse(df_cat$cat %in% known_delete,"•","")
df_an = df_cat[,c(1:3)] %>% dcast(A~B) %>% as.data.frame() %>%
  arrange(match(A, order_))
df_an2 = df_an[,-1]
rownames(df_an2) <-df_an[,1]
df_an2 <- df_an2[, order_]

d2[upper.tri(d2)] <- NA

newnames <- lapply(
  rownames(d2),
  function(x) bquote(italic(.(x))))

  
d2 %>%  pheatmap(cluster_rows = FALSE,cluster_cols = FALSE,
                color = c("steelblue","wheat4"),border_color ="white",
                display_numbers = df_an2,
                fontsize_number = 30,na_col = "white",
                number_color = "white",
                labels_col = as.expression(newnames),
                labels_row = as.expression(newnames),
                filename="SFig1A.png")

###  cdc14 ###
df <- fread("../../data/SFig1B.csv")[,c(2:4)]
df$A <- paste0(df$A,"∆")
df$B <- paste0(df$B,"∆")

d <- reshape(df, idvar = "A", timevar = "B", direction = "wide") %>% as.data.frame()
d2 <- d %>% select(-A)
rownames(d2) <- d$A
colnames(d2) <- sub("predict.", "", colnames(d2))

eno_order <- c("cdh1∆","cdc14∆","cdc20∆","cdc5∆","clb2G-M∆",
               "nrm1∆","sic1∆","swi5∆","mcm1∆","clb5∆","mbf∆","cln2∆","sbf∆","whi5∆","cln3∆")
d3 <- d2[eno_order]
d4 <- d3[order(match(rownames(d3), eno_order)), , drop = FALSE]
d2 <- d4
order_ = colnames(d2)
known_delete <- c("cdc5_cdc5","cdc14_cdc14","swi5_swi5","cdh1_cdh1","clb2G-M_clb2G-M")
df_cat = df
df_cat$cat <- paste(df_cat$A %>% str_replace("∆", ""),"_",df_cat$B%>% str_replace("∆", ""),sep = "")
df_cat$predict <- ifelse(df_cat$cat %in% known_delete,"•","")
df_an = df_cat[,c(1:3)] %>% dcast(A~B) %>% as.data.frame() %>%
  arrange(match(A, order_))
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
                filename="SFig1B.png")
