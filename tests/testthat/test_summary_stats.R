summary_stats_cont  <- list(
N =   .N,
Nmiss =   .Nmiss,
mean =   .mean ,
sd =   .sd ,
median =   .median,
Q1 =   .Q1 ,
Q3 =   .Q3 ,
IQR =   .IQR,
min =    .min,
max =    .max,
mode =    .mode,
skew =     .skew ,
kurtosis =    .kurtosis,
CIL =    .meanCIlower,
CIU =    .meanCIupper,
HLL =    .HLCIlower,
HLU =    .HLCIupper
)

summary_stats_cat  <- list(
mean =       .factormean,
sd =        .factorsd  ,
median =       .factormedian,
Q1 =        .factorQ1,
Q3 =        .factorQ3,
min =        .factormin,
max =        .factormax,
CIL =        .factor_firstlevel_CIlower,
CIU =        .factor_firstlevel_CIupper,
CIL2 =        .factor_lastlevel_CIlower,
CIU2 =       .factor_lastlevel_CIupper
)

format_summary_stats  <- list(
N =   as.character,  
Nmiss    = as.character,
mean     = as.character,
sd   = as.character,
median   = as.character,
Q1   = as.character,
Q3   = as.character,
IQR      = as.character,
min      = as.character,
max      = as.character,
mode     = as.character,
skew     = as.character,
kurtosis     = as.character,
CIL      = as.character,
CIU      = as.character,
HLL      = as.character,
HLU      = as.character,
mean     = as.character,
sd   = as.character,
median   = as.character,
Q1   = as.character,
Q3   = as.character,
min      = as.character,
max      = as.character,
CIL      = as.character,
CIU      = as.character,
CIL2     = as.character,
CIU2     = as.character
)

test_that("continuous summary stats work",{
    expect_error(descr(ToothGrowth, "supp", summary_stats_cont = summary_stats_cont,
    format_summary_stats = format_summary_stats), NA)
})

ToothGrowth2 <- ToothGrowth
ToothGrowth2$cat_var  <- factor(rep(c("1", "2"), nrow(ToothGrowth)/2) )

test_that("categorical summary stats work",{
    expect_error(descr(ToothGrowth2, "supp", summary_stats_cat = summary_stats_cat,
    format_summary_stats = format_summary_stats), NA)
})


test_that("edge cases are handled correctly in summary stats",{
    expect_equal(.meanCIlower(c(1)), NA_real_)
    expect_equal(.meanCIupper(c(1)), NA_real_)
    expect_equal(.HLCIlower(ordered(c("1", "2", "3", "2", "4", "1", "5", "5"))), suppressWarnings(wilcox.test(c(1, 2, 3, 2, 4, 1, 5, 5),conf.int=TRUE)$conf.int[1]))
    expect_equal(.HLCIupper(ordered(c("1", "2", "3", "2", "4", "1", "5", "5"))), suppressWarnings(wilcox.test(c(1, 2, 3, 2, 4, 1, 5, 5),conf.int=TRUE)$conf.int[2]))
})



test_that("numeric printing fails if some summary stat does not return numeric",{
    expect_warning(expect_error(print(descr(c(3), summary_stats_cont = list(a = as.character),
    format_summary_stats = list(a = as.character)), print_format="numeric")))
})




