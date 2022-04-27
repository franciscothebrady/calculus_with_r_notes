#' Plot a function of a single variable
#'
#' In a slice plot, there  is one independent variable. The graph
#' shows the output of the function versus the independent variable. It's
#' called a slice plot to distinguish it from a contour plot, in which
#' the graph has one axis for each independent variable and the output
#' of the function is shown by color and labels.
#'
#' @param object (Not intended for user. Receives the input from  any previous graphics layers.)
#' @param formula A tilde formula with one independent variable. All parameters must
#' be assigned specific numerical values.
#' @param domain A named list giving the extent of the x-axis scale. If there
#' is a previous plot layer using the same independent variable name, the
#' domain can be omitted and will be inferred from the previous layer. Short-hands can also be used.(See the domain() function for examples.)
#' @param npts Integer, number of points at which to evaluate the function.
#' @param color Color of curve
#' @param alpha Alpha of curve
#' @param label_text character string label to place near the graph curve. Default: none.
#' @param label_x number between 0 and 1 indicating the horizontal placement  of the `label_text`.
#' @param label_vjust vertical justification of label. One of "left", "middle", "right", "bottom", "center", "top", "inward", or "outward"
#' @param label_color color of label
#' @param label_alpha alpha of label
#' @param singularities numeric vector of x positions at which to break
#' the graph.
#'
#' Additional arguments will be passed to `geom_line()`. Use, e.g. `color="red"`
#' @examples
#' \dontrun{
#' slice_plot(sin(x) ~ x, domain(x = range(-5, 15)))
#' f <- makeFun(sin(2*pi*t/P) ~ t)
#' slice_plot(f(t, P=20) ~ t, domain(t = -5:10), label_text = "Period 20", label_x=0.9)
#' slice_plot(x^2 ~ x) #domain will be set to -5 < x < 5
#' slice_plot(cos(x) ~ x, domain(x[-10:10]) # domain will be -10 < x < 10
#' # see domain
#' }
#'
#' @export
slice_plot <- function(object, formula, domain = list(c(-5,5)),
                       npts=101,
                       color="black",  alpha = 1,
                       label_text =  "", label_x = 1,
                       label_vjust="top",
                       label_color=color, label_alpha = alpha,
                       singularities = numeric(0), ...) {
  # handle a formula argument that is actually a plain expression, not a formula
  s_object <- enquo(object)
  if (!inherits(object, "ggplot")) { # if it's a previous plot, no need for all this work
    if (missing(formula)) formula <- list(c(-5,5))
    f_object <- enquo(formula)
    es_object <- try(eval(rlang::quo_get_expr(s_object)), silent=TRUE)
    if (inherits(es_object, "try-error")) {
      object <- infer_RHS(!!rlang::quo_get_expr(s_object))
      warning("Better to use a complete TILDE expression")
    } else {
      ef_object <- try(eval(rlang::quo_get_expr(f_object)), silent=TRUE)
      if (inherits(ef_object, "try-error")) {
        formula <- infer_RHS(!!rlang::quo_get_expr(f_object))
        warning("Better to use a complete TILDE expression")
      }
    }
  }
  
  # deal with having to accept previous layers
  # or this being the first layer
  if (rlang::is_formula(object)) {
    if (!missing(formula)) domain <- formula
    formula <- object
    object <- NULL
  } else if (inherits(object, "gg")) {
    # get domain from ggplot object, unless domain is already specified
    if (missing(domain)) {
      look_for <- all.vars(formula[[3]]) # the input variable name
      if (look_for %in% names(object$data)) {
        domain <- list()
        domain[[look_for]] <- range(object$data[[look_for]])
      } else {
        stop("Must specify domain or use same x variable as previous layer.")
      }
    }
  } else {
    stop("First argument (or pipe input) must be either a formula  or a ggplot layer.")
  }
  
  # Check that formula  is a function of one variable
  independent_vars <- all.vars(formula[[3]])
  if (length(independent_vars) != 1)
    stop("Formula must have only one var on RHS of tilde.")
  
  Eval_grid <- mosaicCalc:::eval_on_domain(formula, domain, n = npts)
  inf_locations <- is.infinite(Eval_grid[,2])
  if (any(inf_locations))
    Eval_grid[inf_locations, 2] <- NaN
  
  # place the breaks at the singularities
  if (length(singularities) > 0) {
    matches <- Eval_grid[,1] %in% singularities
    Eval_grid[matches,2] <- NaN
    addins <- which(! singularities %in% Eval_grid[,1])
    if (length(addins) > 0) {
      Singularities <- data.frame(x=singularities[addins], y=NaN)
      names(Singularities) <- names(Eval_grid)
      Eval_grid <- bind_rows(Singularities)
    }
  }
  
  # Look for any Inf's. Replace them with NaN so ggplot2 doesn't connect the lines
  # the_infs <- Eval_grid[,2] == Inf
  # if (any(the_infs)) Eval_grid[the_infs, 2] <- NaN
  the_mapping <- aes(x = !!as.name(names(Eval_grid)[1]),
                     y = .output.)
  
  if (is.null(object)) object <- ggplot(Eval_grid, the_mapping)
  
  P <- object + geom_line(data = Eval_grid, the_mapping,
                          color=color, alpha=alpha,...)
  
  # put the label in  place
  if (label_text != "") {
    n <- nrow(Eval_grid)
    row_num <- pmax(1, pmin(n, round(n * label_x)))
    xpos <- Eval_grid[row_num,1]
    ypos <- Eval_grid$.output.[row_num]
    P <- P + geom_text(x = xpos, y = ypos, label=label_text,
                       vjust=label_vjust, color=label_color, alpha=label_alpha)
  }
  
  P
}

# Split out the first arguments to graphics functions into an (optional)
# previous graphics layer and a (mandatory) tilde expression.
graph_one_two <- function(one, two) {
  if (missing(one))
    stop("Graphing functions requires a tilde expression (or equivalent)")
  if (missing(two)) {
    Previous_layer <- NULL
    tilde <- one
  } else {
    Previous_layer <- one
    tilde <- two
  }
  
  # the tilde expression can be anything accepted by `makeFun()`
  if (!inherits(tilde, gsub("makeFun\\.", "", as.character(methods(makeFun)))))
    stop("Must provide a tilde expression (or another form accepted by `makeFun()`.")
  
  if (! (is.null(Previous_layer) || inherits(Previous_layer, "gg")))
    stop("First argument must be a ggplot layer or a tilde expression.")
  
  return(list(P = Previous_layer, fun=makeFun(tilde)))
}

#' @export
unbound_first <- function(f, max_allowed=1) {
  # at most one argument can be unbound
  unbound <- which(lapply(formals(f), class) == 'name')
  if (length(unbound) == 0) return(f)
  if (length(unbound) > max_allowed)
    stop(glue::glue("Only {max_allowed} free argument{ifelse(max_allowed>1, 's', '')} allowed in this context."))
  
  new_order <- c(unbound, setdiff(1:length(formals(f)), unbound))
  
  formals(f) <- formals(f)[new_order]
  
  f
}

#' @export
interval <- function(...) {
  args <- enquos(...)
  if (any(duplicated(names(args))))
    stop("Duplicated input name.")
  res <- list()
  res_names <- character(length(args))
  for (k in 1:length(args)) {
    ex <- args[[k]]
    command <- as.character(rlang::quo_get_expr(ex)[[1]])
    if (nchar(names(args)[k]) > 0) {
      res_names[k] <- names(args)[k]
    } else {
      # It's unnamed, leave it for later
      res_names[k] <- paste0(".unknown_", k)
    }
    if (command == "[") {
      res_names[k] <- as.character(rlang::quo_get_expr(ex)[[2]])
      res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]), rlang::eval_tidy(rlang::quo_get_expr(ex)[[4]]))
    } else if (command == "%pm%") {
      center <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]])
      margin <- rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]])
      res[[k]] <- c(center - margin, center + margin)
    } else if (command %in% c("%%","|", "||", "&" ,"<", "<=", ">", ">=", ":")) {
      res[[k]] <- c(rlang::eval_tidy(rlang::quo_get_expr(ex)[[2]]),
                    rlang::eval_tidy(rlang::quo_get_expr(ex)[[3]]))
    } else {
      res[[k]] <- rlang::eval_tidy(rlang::quo_get_expr(ex))
    }
    
  }
  names(res) <- res_names
  res
}

#' @export
grid_for_plot <- function(vars, npts=5, needed=1) {
  if (is.numeric(vars)) {
    seq(min(vars), max(vars), length=npts)
  } else if (is.list(vars)) {
    for (k in 1:length(vars)) {
      orig <- vars[[k]]
      if (k <= needed) {
        vars[[k]] <- seq(min(orig), max(orig), length=npts)
      } else {
        # reduce it to a single value
        vars[[k]] <- median(orig)
      }
    }
  }
  
  expand.grid(vars)
}

#' @export
eval_on_grid <- function(fun, grid) {
  vals <- try(do.call(fun, grid))
  if (inherits(vals, "try-error")) stop("Need to write non-vectorized evaluation of function  on  grid.")
  grid$.output. <- vals
  grid
}


#' @export
slice_plot2 <- function(one, two, input=c(-5,5),
                        inputs=input, # just for convenience
                        npts=101,
                        color="black",  alpha = 1,
                        ltext =  "", lx = 1,
                        ljust="top",
                        lcolor=color, lalpha = alpha,
                        singularities = numeric(0), ...) {
  Givens <- graph_one_two(one, two)
  P   <- Givens$P
  Fun <- unbound_first(Givens$fun)
  # Fun has at most one unbound input and that one is first
  # it's name is ...
  independent_var <- names(formals(Fun))[[1]]
  
  Grid <- grid_for_plot(inputs, npts=npts, needed=1)
  names(Grid)[1] <- independent_var
  
  Grid <- eval_on_grid(Fun, Grid)
  
  # Look for infinities
  inf_locations <- is.infinite(Grid$.output.)
  if (any(inf_locations))
    Eval_grid$.output.[inf_locations] <- NaN
  
  
  # place the breaks at the singularities
  # NEEDS TO BE CHECKED
  # if (length(singularities) > 0) {
  #   matches <- Eval_grid[,1] %in% singularities
  #   Eval_grid[matches,2] <- NaN
  #   addins <- which(! singularities %in% Eval_grid[,1])
  #   if (length(addins) > 0) {
  #     Singularities <- data.frame(x=singularities[addins], y=NaN)
  #     names(Singularities) <- names(Eval_grid)
  #     Eval_grid <- bind_rows(Singularities)
  #   }
  # }
  
  # Look for any Inf's. Replace them with NaN so ggplot2 doesn't connect the lines
  # the_infs <- Eval_grid[,2] == Inf
  # if (any(the_infs)) Eval_grid[the_infs, 2] <- NaN
  the_mapping <- aes(x = !!as.name(independent_var),
                     y = .output.)
  
  if (is.null(P)) P <- ggplot(Grid, the_mapping)
  
  P <- P + geom_line(data = Grid, the_mapping,
                     color=color, alpha=alpha,...)
  
  # put the label in  place
  if (ltext != "") {
    n <- nrow(Grid)
    row_num <- pmax(1, pmin(n, round(n * label_x)))
    xpos <- Grid[row_num,1]
    ypos <- Grid$.output.[row_num]
    P <- P + geom_text(x = xpos, y = ypos, label=ltext,
                       vjust=ljust, color=lcolor, alpha=lalpha)
  }
  
  P
}
