#' Vaimse tervisega seotud heaolu indeks (VTHOI)
#'
#' @param x andmestik (8 tunnust õigest järjekorras: POS1..POS3, B1...B4, Tervis_eh)
#' @param mud Mudel (vaikimisi kasutatakse RVTU 1. laine andmete põhjal koostatud GRM mudelit, kuid võib ette anda ka mõne teise GRM mudeli) 
#'
#' @return faktorskooride vektor (puuduvate andmetega ridades on faktorskoori asemele kirjutatud NA)
#' @export
hoi <- function(x, mud = hoi_mudel, ...) {
  nms <- names(coef(mud))
  fs <- factor.scores(mud, x, ...)$score.dat$z1
  nanai <- rowSums(is.na(x))
  fs[nanai>0] <- NA
  fs
}
