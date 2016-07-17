library(dplyr)
library(data.table)
set.seed(123)
n = 5e7
k = 5e5
x = runif(n)
grp = sample(k, n, TRUE)

timing = list()

# sapply
timing[["sapply"]] = system.time({
  lt = split(x, grp)
  r.sapply = sapply(lt, function(x) list(sum(x), length(x)), simplify = FALSE)
})

# lapply
timing[["lapply"]] = system.time({
  lt = split(x, grp)
  r.lapply = lapply(lt, function(x) list(sum(x), length(x)))
})

# tapply
timing[["tapply"]] = system.time(
  r.tapply <- tapply(x, list(grp), function(x) list(sum(x), length(x)))
)

# by
timing[["by"]] = system.time(
  r.by <- by(x, list(grp), function(x) list(sum(x), length(x)), simplify = FALSE)
)

# aggregate
timing[["aggregate"]] = system.time(
  r.aggregate <- aggregate(x, list(grp), function(x) list(sum(x), length(x)), simplify = FALSE)
)

# dplyr
timing[["dplyr"]] = system.time({
  df = data_frame(x, grp)
  r.dplyr = summarise(group_by(df, grp), sum(x), n())
})

# system.time({
#   df = data_frame(x, grp)
#   r.plyr = ddply(df, grp, summarise, sumx=sum(x), n=n(x))
# })

# df = data_frame(x, grp) %>% group_by(grp)
# timing[["dplyr2"]] = system.time({
#   r.dplyr = summarise(df, sum(x), n())
# })

# multidplyr
library(multidplyr)
df = partition(data_frame(x, grp), grp)  # takes a long time
timing[['multidplyr']] = system.time({
  summarise(df, sumx=sum(x), n=n())
})



# data.table
timing[["data.table"]] = system.time({
  dt = setnames(setDT(list(x, grp)), c("x","grp"))
  r.data.table = dt[, .(sum(x), .N), grp]
})

# all output size match to group count
sapply(list(sapply=r.sapply, lapply=r.lapply, tapply=r.tapply, by=r.by, aggregate=r.aggregate, dplyr=r.dplyr, data.table=r.data.table),
       function(x) (if(is.data.frame(x)) nrow else length)(x)==k)
#    sapply     lapply     tapply         by  aggregate      dplyr data.table
#      TRUE       TRUE       TRUE       TRUE       TRUE       TRUE       TRUE

as.data.table(sapply(timing, `[[`, "elapsed"), keep.rownames = TRUE)[,.(fun = V1, elapsed = V2)][order(-elapsed)]

save(timing, file='timing.RData')
