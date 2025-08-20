# Experimental Code

This folder contains experimental code that is not yet ready for production use.

## Boundary Methods

The boundary finding functionality (`find_roi_boundaries` and related methods) has been moved here due to failing tests and implementation issues. The code includes:

- `R/boundaries.R` - Main implementation of boundary finding algorithms
- `tests/testthat/test_boundaries_methods.R` - Tests for different boundary methods
- `tests/testthat/test_boundaries_edge_faces.R` - Tests for edge face boundary detection
- `man/find_roi_boundaries.Rd` - Documentation for the main function
- `man/findBoundaries-methods.Rd` - Documentation for the generic method

### Known Issues

1. The sparse matrix creation fails with NA values in some edge cases
2. The boundary face detection logic has type errors when handling single-face edges
3. Various test cases fail with incorrect boundary counts or coordinates

### Future Work

The boundary finding functionality needs to be refactored to handle edge cases properly and ensure consistent behavior across different mesh topologies.