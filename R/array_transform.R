
#' rotate_dimensions
#'
#' rotate the dimensions of an array s.t. the first margin becomes
#' the last.
#'
#' The invariants for all arrays 'x' with more than one margin are:
#' dim(rotate_dimensions(x)) == c(dim(x)[2:D], dim(x)[1])
#' x['a','b','c'] == rotate_dimensions(x)['b','c','a']
#'
#' @param x an array
#' @return an array meeting the invariants
rotate_dimensions = function(x) {
  cdx = dim(x)
  nd = length(cdx)
  if (nd == 1)
    return(x)
  pdx = cdx[2:nd]
  ppdx = prod(pdx)
  gdx = c(pdx, cdx[1])
  o = vector(mode = 'list', length = cdx[1])
  for (i in 1:cdx[1]) {
    new_dn = c(dimnames(x)[2:nd], dimnames(x)[[1]][i])
    names(new_dn)[nd] = names(dimnames(x))[1]
    o[[i]] = array(
      data = x[seq(from = i, to = length(x) - cdx[1] + i, by = cdx[1])],
      dim = c(pdx, 1), dimnames = new_dn)
  }
  o = do.call(what = abind::abind, args = o)
  dimnames(o) = c(dimnames(x)[2:nd], dimnames(x)[1])
  return(o)
}


#' transform_yearly
#'
#' Transform standard proprotions output to indicators
#' for single-unit array, applied yearly
#'
#' @param x an array of samples with margins:
#'        1) chain
#'        2) iteration
#'        4) year
#'        5) three proportions in standard order:
#'             1) modern CPR
#'             2) traditional CPR
#'             3) unmet need for contraceptives
#' @param f a function that takes a vector with at least three
#'          proportions in order: 1) modern CPR; 2) traditional CPR;
#'          and 3) unmet need and returns one or more indicators.
#' @return an array with the first three margins and the
#'         last margin modified to be 'indicator', which
#'         can be any transformation of the three proprotions.
transform_yearly  = function(x, f, ...) {
  nd = length(dim(x))
  y = apply(X = x, MARGIN = c(1,2,3), f, ...)
  if (length(dim(y)) != nd)
    dim(y) = c(1, dim(y))
  y = rotate_dimensions(y)
  return(y)
}

#' transform
#'
#' Transform standard proprotions output to indicators
#' for single-unit array, summarizing years
#'
#' @param x an array of samples with margins:
#'        1) chain
#'        2) iteration
#'        4) year
#'        5) three proportions in standard order:
#'             1) modern CPR
#'             2) traditional CPR
#'             3) unmet need for contraceptives
#' @param g a function that takes an array with years on the first margin
#'          and a vector with at least three proportions in order on
#'          the other: 1) modern CPR; 2) traditional CPR;
#'          and 3) unmet need and returns one or more indicators.
#' @return an array with the first two margins and the
#'         last margin modified to be 'indicator', which
#'         can be any transformation of the evolution of the three
#'         proprotions over the years..
transform  = function(x, g, ...) {
  y = apply(X = x, MARGIN = c(1,2), g, ...)
  if (length(dim(y)) != nd)
    dim(y) = c(1, dim(y))
  y = rotate_dimensions(y)
  return(y)
}


#' Define three core proportions by order in the prortions vector.
#'
#' Samples from the contraceptive use models always produce estimates
#' in order for a few basic proportions and we create accessors for these.
#'
#' Further transformations refer to the accessors rather than using indexes
#' into the vector of proportions.
#'
#' @param x vector of at least length 3 with the first
#'        three elements being: 1) modern CPR use
#'        2) traditional CPR use; and 3) unmet need
#'        for contraceptives
#' @name proportions_vector
NULL

#' @rdname proportions_vector
#' @return proportion of in-universe respondents using
#'         a modern contraceptive method.
modern_cpr = function(x) x[1]

#' @rdname proportions_vector
#' @return proportion of in-universe respondents using
#'         a traditional contraceptive method.
traditional_cpr = function(x) x[2]

#' @rdname proportions_vector
#' @return proportion of in-universe respondents with
#'         an unmet need for any contraceptive method
unmet_p = function(x) x[3]

#' @rdname proportions_vector
#' @return proprotion of in-universe respondents using
#'         either a modern or traditional contraceptive
#'         method.
total_cpr = function(x) modern_cpr(x) + traditional_cpr(x)

#' @rdname proportions_vector
#' @return proportion of in-uinverse respondents using
#'         a modern or traditional contraceptive method
#'         or in need of either method.
demand = function(x) total_cpr(x) + unmet_p(x)

#' @rdname proportions_vector
#' @return proportion of in-uinverse respondents using
#'         a modern contraceptive method
#'         or in need of a modern method.
demand_modern = function(x) modern_cpr(x) + unmet_modern(x)

#' @rdname proportions_vector
#' @return proportion of in-universe respondents not
#'         using a contraceptive method and not in need
#'         of a contraceptive method.
no_need = function(x) 1 - demand(x)

#' @rdname proportions_vector
#' @return proportion of in-universe respondents with
#'         an unmet need for a modern contraceptive method
unmet_modern = function(x) traditional_cpr(x) + unmet_p(x)

#' @rdname proportions_vector
#' @return proportion of in-universe respondents not
#'         using a modern or traditional contraceptive
#'         method.
non_use = function(x) 1 - total_cpr(x)

#' @rdname proportions_vector
#' @return proportion of demand for a
#'         contraceptive method satisifed using either a
#'         modern or traditional contracepritve method.
demand_satisfied = function(x) total_cpr(x) / demand(x)

#' @rdname proportions_vector
#' @return proprotion of demand among in-universe respondents for a
#'         contraceptive method satisifed using a
#'         modern contracepritve method.
demand_satisfied_modern = function(x) modern_cpr(x) / demand(x)



