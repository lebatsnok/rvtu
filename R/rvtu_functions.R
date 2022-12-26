#' Title
#'
#' @param file 
#' @param to.list 
#' @param Unlist 
#' @param spray 
#' @param delete.parent 
#' @param Attach 
#'
#' @return
#' @export
#'
#' @examples
Load <- function (file, to.list = TRUE, Unlist = TRUE, spray = FALSE, 
                  delete.parent = FALSE, Attach = FALSE) 
{
  attachYN <- (is.logical(Attach) & isTRUE(Attach)) | is.character(Attach)
  if (attachYN) {
    attachNAME <- if (is.logical(Attach)) 
      date()
    else Attach[1]
  }
  NE <- if (spray) 
    .GlobalEnv
  else if (attachYN) 
    attach(NULL, name = attachNAME)
  else new.env()
  load(file, NE)
  if (attachYN) 
    return(invisible(NULL))
  if (to.list) {
    NE <- as.list(NE)
    if (Unlist & length(NE) == 1) 
      NE <- NE[[1]]
  }
  if (delete.parent) 
    parent.env(NE) <- emptyenv()
  NE
}

#' Title
#'
#' @param x 
#' @param direction 
#' @param keep.na 
#'
#' @return
#' @export
#'
#' @examples
regularize_mk <- function(x, direction="long", keep.na = TRUE){
  x <- sub("Tartu linn", "Tartu", x)
  x <- sub("Tallinna linn", "Tallinn", x)
  if(direction=="long"){
    x <- sub("maa$", " maakond", x)
  }
  if(direction=="short"){
    x <- sub(" maakond$", "maa", x)
  }
  if(keep.na)   x else x[!is.na(x)]
}

#' Title
#'
#' @param D 
#' @param ... 
#' @param dateAsCharacter 
#'
#' @return
#' @export
#'
#' @examples
write_xlsx <- function(D, ..., dateAsCharacter = TRUE){
  characterize <- function(.){
    is.date <- function(x) is(x, "POSIXct") | is(x, "POSIXlt") | is(x, "Date")
    k <- which(sapply(., is.date))
    .[,k] <- lapply(.[,k], as.character)
    .
  }
  if(dateAsCharacter) D <- characterize(D)
  if(!is.null(key(D))){
    WO <- list(data=D, key=key(D))
    if(!is.null(attr(D, "weights"))) WO$weights <- attr(D, "weights")
    if(!is.null(attr(D, "poolikud"))) WO$poolikud <- characterize(attr(D, "poolikud"))
  } else WO <- D
  openxlsx::write.xlsx(WO, ...)
}

#' Title
#'
#' @param x 
#' @param add.numbers 
#'
#' @return
#' @export
#'
#' @examples
paste_levels <- function(x, add.numbers= FALSE){
  il <- which(sapply(x, is.list))
  ilok <- sapply(x[il,1], length)>0
  kombo <- function(a,b) paste(paste(a,b, sep=":"), collapse="|") 
  komps <- mapply(kombo, x[ilok, il[1]], x[ilok, il[2]] )
  res <- rep(NA, NROW(x))
  res[ilok] <- komps
  res
}

#' Title
#'
#' @param x 
#' @param sep1 
#' @param sep2 
#' @param num 
#'
#' @return
#' @export
#'
#' @examples
split_levels <- function(x, sep1 = "|", sep2 = ":", num = TRUE){
  s1 <- strsplit(x, sep1, fixed=TRUE)[[1]]
  s2 <- do.call(rbind, strsplit(s1, sep2, fixed=TRUE))
  LEVELS <- if(num) as.numeric(s2[,1]) else s2[,1]
  LABELS <- s2[,2]
  list(LEVELS=LEVELS, LABELS=LABELS)
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
use_labels <- function(x, ...) {
  UseMethod("use_labels")
}

#' Title
#'
#' @param x 
#' @param vn 
#' @param as.factor 
#'
#' @return
#' @export
#'
#' @examples
use_labels.data.frame <- function(x, vn, as.factor=FALSE){
  # vn - variable name
  sildid <- split_levels(subset(key(x), NAME %in% vn)$LEVELS)
  res <- factor(x[[vn]], levels=sildid$LEVELS, labels=sildid$LABELS)
  if(!as.factor) as.character(res) else res
}

#' Title
#'
#' @param x 
#' @param v 
#' @param as.factor 
#'
#' @return
#' @export
#'
#' @examples
use_labels.list <- function(x, v, as.factor=FALSE){
  sildid <- x
  res <- factor(v, levels=sildid$LEVELS, labels=sildid$LABELS)
  if(!as.factor) as.character(res) else res
}

#' Title
#'
#' @param x 
#' @param as.factor 
#'
#' @return
#' @export
#'
#' @examples
use_labels.default <- function(x, as.factor = FALSE){
  # not safe to use in functions!!
  s <- substitute(x)
  vn <- deparse(s[[3]])  
  df <- get(deparse(s[[2]]))
  sildid <- split_levels(subset(key(df), NAME %in% vn)$LEVELS)
  res <- factor(df[[vn]], levels=sildid$LEVELS, labels=sildid$LABELS)
  if(!as.factor) as.character(res) else res
}

#' Title
#'
#' @param x 
#' @param expr 
#'
#' @return
#' @export
#'
#' @examples
key <- function(x, expr) {
  k <- attr(x, "key")
  if(missing(expr)) return(k)
  ek <- as.environment(k)
  parent.env(ek) <- parent.frame()
  k[eval(substitute(expr), ek),] 
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
add2key <- function(x, ...){
  # KUI list(...)$NAME  %in% k$NAME, siis tekitatakse topeltrida:: 
  k <- attr(x, "key")
  ldf <- as.data.frame(list(...), stringsAsFactors = FALSE)
  unn <- setdiff(names(k), names(ldf))
  for(iii in unn) ldf[[iii]] <- NA
  k <- rbind(k, ldf[, names(k)])
  attr(x, "key") <- k
  x
}

#' Title
#'
#' @param x 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
`key<-` <- function(x,value){
  if(!identical(names(x), value$NAME)) stop("Legendi tabelis peab olema tulp NAME, mis on identne andmetabeli tulpade nimedega! ")
  ok <- key(x)
  attr(x, "key") <- value
  x
}

#' Title
#'
#' @param x 
#' @param value 
#'
#' @return
#' @export
#'
#' @examples
`fkey<-` <- function(x, value){
  K <- value
  attr(x, "key") <- value
  regularize_key(x)
  x
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
regularize_key <- function(x){
  K <- key(x)
  if(identical(names(x), K$NAME)) return(x)
  N <- names(x)
  K <- subset(K, NAME %in% N)
  if(identical(names(x), K$NAME)) return(x)
  mn <- setdiff(N, K$NAME)
  emr <- K[1,]
  emr[,] <- NA
  if("FLEVELS" %in% names(K)) emr$FLEVELS <- emr$FLABELS <- list(NULL)
  nur <- do.call(rbind, replicate(length(mn), emr, simplify = FALSE))
  nur$NAME <- mn
  K <- rbind(K, nur)
  K <- K[match(names(x), K$NAME),]
  attr(x, "key") <- K
  x
}

#' Title
#'
#' @param D 
#' @param ADD 
#'
#' @return
#' @export
#'
#' @examples
include_all <- function(D, ADD = "poolikud"){
  IIADD <- do.call(rbind, attributes(D)[ADD])
  IIADD.unique <- subset(IIADD, !token %in% D$token)
  rbind(D, IIADD.unique)
}

#' Title
#'
#' @param x 
#' @param EXPR 
#' @param var 
#' @param FUN 
#' @param offset 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
compute_scores <- function(x, EXPR, var = "SCALE", FUN = sum,  offset = 0,  ...){
  k <- key(x) 
  w <- eval(substitute(EXPR), k)
  ks <- k[which(w),  ]
  xs <- x[, names(x) %in% ks$NAME] + offset
  if(!identical(names(xs), ks$NAME)) stop("Wrong key")
  if(is.null(var)) return(apply(xs, 1, FUN, ...))
  subscales <- ks[[var]]
  unique_subscales <- unique(subscales)
  res <- as.data.frame(lapply(unique_subscales, function(iii) apply(xs[, subscales %in% iii, drop=FALSE], 1, FUN, ...)))
  names(res) <- unique_subscales
  res
}

#' Title
#'
#' @param x 
#' @param EXPR 
#' @param sel 
#'
#' @return
#' @export
#'
#' @examples
grab <- function(x, EXPR, sel){
  k <- key(x)
  a <- attributes(x) ###### as of now, dropping all atrributes exc. key (+ names, class)
  tulbad <- k$NAME[which(eval(substitute(EXPR), k))]
  res <- x[, tulbad, drop=FALSE]
  #browser()
  k <- subset(k, NAME %in% tulbad)
  if(!missing(sel)){
    read <- which(eval(substitute(sel), x))
    res <- res[read,]
  }
  fkey(res) <- k
  res
}

#' Title
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
"%has%" <- function(a,b) {
  grepl(tolower(b), tolower(a))
}

#' Title
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
"%Has%" <- function(a,b) {
  grepl(b, a)
}


#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
factorize_all <- function(x){
  fs <- key(x)$FACTOR %in% 1
  for(iii in names(x)[fs]){
    x[[iii]] <- factorize(x[[iii]], key(x, NAME %in% iii)$LEVELS)
  }
  x
}

#' Title
#'
#' @param x 
#' @param LABS 
#'
#' @return
#' @export
#'
#' @examples
factorize <- function(x, LABS){
  k <- split_levels(LABS)
  factor(x, levels=k$LEVELS, labels=k$LABELS)
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
label_for_export <- function(x){
  k <- key(x)
  for(iii in names(x)){
    kii <- key(x, NAME %in% iii)
    if(is.character(x[[iii]])) x[[iii]] <- as.numeric(x[[iii]]) ### WHAT??
    if(is.logical(x[[iii]])) x[[iii]] <- as.numeric(x[[iii]])
    if(inherits(x[[iii]], "POSIXt")) x[[iii]] <- as.character(x[[iii]]) 
    if(kii$FACTOR %in% 1) {
      kiil <- split_levels(kii$LEVELS)
      xii <- try(haven::labelled(x[[iii]], labels =setNames(kiil$LEVELS, kiil$LABELS), label=kii$LAB))
    }  else xii <- try(haven::labelled(x[[iii]], label=kii$LAB))
    if(inherits(xii, "try-error")) browser()
    x[[iii]] <- xii
  }
  x
}


