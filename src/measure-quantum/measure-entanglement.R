###############################################################################
# This script implements the measurement for entanglement
#
# Author: Vivek Katial
# Created 2019-11-11 11:16:58
###############################################################################

example_ham <- matrix(
  c(1,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,1,0,
    0,0,0,0,0,0,0,1),
  nrow = 8,
  ncol = 8
)

decomposed_system <- RQEntangle::schmidt.decompose(example_ham)

