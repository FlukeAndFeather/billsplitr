library(igraph)
library(tidyverse)
source("R/billsplit.R")

expenses <- tribble(
  ~id, ~paid_by,  ~amount, ~split_by,
  1,   "Max",     56.73,   c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2),
  2,   "Allison", 316.02,  c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2),
  3,   "Max",     216.76,  c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2),
  4,   "Max",     112.35,  c(Max = 1, JNR = 3, DK = 2),
  5,   "CJN",     120.00,  c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2),
  6,   "Max",     55.70,   c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2),
  7,   "JNR",    125.00,  c(Max = 1, Allison = 1, Aspen = 1, CJN = 3, JNR = 3, DK = 2)
)

split_bill(expenses)
