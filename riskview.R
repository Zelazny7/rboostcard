
df <- haven::read_sas("C:/Projects/py-dev/pyboostcard/riskview/onyx_model.sas7bdat")

df <- as.data.frame(df)

drop_cols <-  c("bad","dev60","val40")
k <- setdiff(names(df), drop_cols)

factors <- sapply(df[k], is.character)

df[k[factors]] <- lapply(df[k[factors]], function(x) as.numeric(as.factor(x)))


bst <- new("Boostcard", constraints="tmp.json", max_depth=4L, min_child_weight=100L)


bst$fit(df[df$dev60 == 1, k], y = df$bad[df$dev60 == 1])



"riskview/onyx_model.sas7bdat"


library(ks)
p <- bst$predict(df)
ksd <- data.frame(y=df$bad, sample=df$dev60, score=-p)

ks_table(y~score|sample, data=ksd, number_bins = 20)
