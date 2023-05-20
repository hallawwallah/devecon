#/*==================================================================================================
#  Project:       All Eyes on Them: A Field Experiment on Citizen Oversight and Electoral Integrity
#  Author:        Natalia Garbiras 
#  Purpose:       This code generates plots displaying the p-values for the main balance checks.
#----------------------------------------------------------------------------------------------------
#  Index:		   A. Program Set-Up
#              B. Generating Plots
#===================================================================================================*/

#===============================================================================================
#                                       A. Program Set-Up
#===============================================================================================

rm(list = ls())

# Set directory path:
replication_dir<-"/Users/mateomontenegro/Dropbox (CEMFI)/Facebook Project 2/Submission/Replication"

install.packages("readxl")
library(readxl)

#===============================================================================================
#                                       B. Generating Plots
#===============================================================================================

dta<- read_excel(paste0(replication_dir,"/Output/Balance/balance_pvals.xlsx"))
colnames(dta)[1]=c("Variable")

pdf(paste0(replication_dir,"/Output/graph_balance_1.pdf"))
mar = par()$mar
op = par(mar = c(3.5, 1.1 * nrow(dta[1:17,]), 0, 1))
par(family="serif")
varline = 6
nline = 4.5
cline = 1
plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(1, nrow(dta[1:17,]) +
                                                    1.5), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
mtext(text = c("Variable\nName", "Control\nMean"),
      side = 2, font = 2, line = c(varline + 2, cline + 1.2), adj = 0.5, las = 2, at = nrow(dta[1:17,]) + 1.28, cex = 0.9)

for (i in 1:nrow(dta[1:17,])) {
  aty = nrow(dta[1:17,]) - i + 1
  mtext(text = dta$Variable[i], side = 2, line = varline, adj = 1,
        las = 2, at = aty, cex = 0.8)
  meanC = round(dta$`Control Group Mean`[i],3)
  mtext(text = c(meanC), side = 2, line = c(cline+0.7), adj = 1, las = 2, at = aty, cex = 0.8)
  if (aty%%2 == 1) {
    polygon(x = c(0, 0, 1, 1), y = c(aty - 0.5, aty +
                                       0.5, aty + 0.5, aty - 0.5), border = FALSE, col = "lightgray")
  }
  # 		
  p1 = round(dta$`Any Treatment vs Control`[i],3)
  p2 = round(dta$`No Letter - Any Ad vs Control`[i],3)
  p3 = round(dta$`Letter - Any Ad vs Control`[i],3)
  points(pch = 16, col = "black", x = p1, y = aty-0.3)
  points(pch = 17, col = "black", x = p2, y = aty)
  points(pch = 18, col = "black", x = p3, y = aty+0.3)
  segments(x0 = 0, x1 = 0, y0 = 0.49, y1 = nrow(dta[1:17,]) + 0.48)
  segments(x0 = 0, x1 = 1, y0 = 0.49, y1 = 0.49)
  segments(x0 = c(0.05, 0.1), x1 = c(0.05, 0.1), y0 = 0.5,
           y1 = nrow(dta[1:17,]) + 0.48, lty = "dotted")
  mtext(side = 1, at = c(0, 0.05, 0.1, 1), text = c("0", ".05", ".1", "1"), cex = 0.7, line = -0.2 + 5/nrow(dta[1:17,]) - 0.01 * nrow(dta[1:17,]))
  mtext(side = 1, line = 1.2, at = 0.5, text = "p-value")
}

op = par(mar = mar)
dev.off()

pdf(paste0(replication_dir,"/Output/graph_balance_2.pdf"))

mar = par()$mar
op = par(mar = c(3.5, 1.1 * nrow(dta[18:34,]), 0, 1))
par(oma=c(2, 0, 0, 0))
par(family="serif")
varline = 6
nline = 4.5
cline = 1
plot(x = NULL, y = NULL, xlim = c(0, 1), ylim = c(1, nrow(dta[18:34,]) +
                                                    1.5), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n")
mtext(text = c("Variable\nName", "Control\nMean"),
      side = 2, font = 2, line = c(varline + 2, cline + 1.2), adj = 0.5, las = 2, at = nrow(dta[18:34,]) + 1.28, cex = 0.9)

for (i in 18:34) {
  aty = nrow(dta) - i + 1
  mtext(text = dta$Variable[i], side = 2, line = varline, adj = 1,
        las = 2, at = aty, cex = 0.8)
  meanC = round(dta$`Control Group Mean`[i],3)
  mtext(text = c(meanC), side = 2, line = c(cline+0.7), adj = 1, las = 2, at = aty, cex = 0.8)
  if (aty%%2 == 1) {
    polygon(x = c(0, 0, 1, 1), y = c(aty - 0.5, aty +
                                       0.5, aty + 0.5, aty - 0.5), border = FALSE, col = "lightgray")
  }
  # 		
  p1 = round(dta$`Any Treatment vs Control`[i],3)
  p2 = round(dta$`No Letter - Any Ad vs Control`[i],3)
  p3 = round(dta$`Letter - Any Ad vs Control`[i],3)
  points(pch = 16, col = "black", x = p1, y = aty-0.3)
  points(pch = 17, col = "black", x = p2, y = aty)
  points(pch = 18, col = "black", x = p3, y = aty+0.3)
  segments(x0 = 0, x1 = 0, y0 = 0.49, y1 = nrow(dta[18:34,]) + 0.48)
  segments(x0 = 0, x1 = 1, y0 = 0.49, y1 = 0.49)
  segments(x0 = c(0.05, 0.1), x1 = c(0.05, 0.1), y0 = 0.5,
           y1 = nrow(dta[18:34,]) + 0.48, lty = "dotted")
  mtext(side = 1, at = c(0, 0.05, 0.1, 1), text = c("0", ".05", ".1", "1"), cex = 0.7, line = -0.2 + 5/nrow(dta[1:17,]) - 0.01 * nrow(dta[1:17,]))
  mtext(side = 1, line = 1.2, at = 0.5, text = "p-value")
}

legend(x=-1, y=-2.5, bty="n", cex = .9, c("Any Treatment vs Control", "No Letter-Any Ad vs Control", "Letter-Any Ad vs Control"),
       pch = c(16, 17,18), pt.cex=1, col = c("black","black","black"), horiz = TRUE, xpd = NA, 
       x.intersp = 0.5, xjust=0, yjust=0, text.width=c(0,0.55,0.6))
op = par(mar = mar)
dev.off()
