#' @title `heart` class
#' @description `heart` is an S4 class that serves as an object that holds together different components that represent the anatomical heart. A `heart` object allows for storing clinical information about a patient together, which may more readily allow for analytical approaches to be developed. The heart has plumbing (the coronary arteries), electricity (the nerves), and walls (cardiac chambers). The slots represent these objects.
#' @slot pipes the epicardial and resistance vessels
#' @slot wiring the conduction system of nerves
#' @slot walls cardiac chambers
#' @slot doors valve structures of the heart
#' @noRd
#' @keywords internal
methods::setClass(
	"heart",
	slots = c(
		pipes = "list",
		electric = "list",
		structural = "list"
	),
)