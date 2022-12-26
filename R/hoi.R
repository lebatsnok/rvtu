#' Title
#'
#' @param x 
#' @param mud Mudel (vaikimisi kasutatakse RVTU 1. laine andmete põhjal koostatud GRM mudelit, kuid võib ette anda ka mõne teise GRM mudeli) 
#'
#' @return faktorskoorid
#' @export
hoi <- function(x, mud = hoi_mudel) {
  nms <- names(coef(mud))
  fs <- factor.scores(mud, x)$score.dat$z1
  nanai <- rowSums(is.na(x))
  fs[nanai>0] <- NA
  fs
}
