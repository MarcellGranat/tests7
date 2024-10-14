#' Range Class
#'
#' A class representing a range with start and end values.
#'
#' @slot start A numeric value representing the start of the range.
#' @slot end A numeric value representing the end of the range.
#'
#' @section Validation:
#' The class has a validator function that checks if the start and end values are of length 1 and if the end value is greater than or equal to the start value.
#'
#' @examples
#'
#' range <- new("range", start = 1, end = 10)
#' range
#' 
#' @export
#' 

range <- S7::new_class("range",
  properties = list(
    start = S7::class_double,
    end = S7::class_double
  ),
  validator = function(self) {
    if (length(self@start) != 1) {
      "@start must be length 1"
    } else if (length(self@end) != 1) {
      "@end must be length 1"
    } else if (self@end < self@start) {
      sprintf(
        "@end (%i) must be greater than or equal to @start (%i)",
        self@end,
        self@start
      )
    }
  },
  constructor = function(start, end) {
    S7::new_object(S7::S7_object(), start = start, end = end) 
  }
)