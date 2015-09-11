#' CPU time used measured in sub-milliseconds
#'
#' Clone of base R \code{system.time} function utilizing accuracy of \code{microbenchmark} package.
#' 
#' @param expr Valid R expression to be timed.
#' @param gcFirst Logical - should a garbage collection be performed immediately before the timing? Default is TRUE.
#' 
#' @return \code{proc_time} class object, only \code{elapsed} value is returned, all others are \code{NA}.
#' 
#' @seealso \code{\link{get_nanotime}}
#' @export
system.nanotime = function(expr, gcFirst = TRUE){
    if (!exists("proc.time")) return(rep(NA_real_, 5L))
    if (gcFirst) gc(FALSE)
    time <- get_nanotime()
    on.exit(cat("Timing stopped at:", (get_nanotime() - time) * 1e-9, "\n"))
    expr
    new.time <- get_nanotime()
    on.exit()
    structure(c(NA_real_, NA_real_, (new.time - time) * 1e-9, NA_real_, NA_real_),
              .Names = c("user.self", "sys.self", "elapsed", "user.child", "sys.child"),
              class = "proc_time")
}
