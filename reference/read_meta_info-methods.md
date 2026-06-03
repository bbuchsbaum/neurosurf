# Read Meta Information

Generic function to read image meta info given a file and a
[`FileFormat`](https://bbuchsbaum.github.io/neuroim2/reference/FileFormat-class.html)
instance from neuroim2.

## Usage

``` r
# S4 method for class 'AFNISurfaceFileDescriptor'
read_meta_info(x, file_name)

# S4 method for class 'NIMLSurfaceFileDescriptor'
read_meta_info(x, file_name)

# S4 method for class 'FreesurferAsciiSurfaceFileDescriptor'
read_meta_info(x, file_name)

# S4 method for class 'FreesurferBinarySurfaceFileDescriptor'
read_meta_info(x, file_name)

# S4 method for class 'GIFTISurfaceFileDescriptor'
read_meta_info(x, file_name)
```

## Arguments

- x:

  the file descriptor object

- file_name:

  the name of the file containing meta information.

## Value

A meta info object containing header information from the surface file

A metadata information object describing the surface file structure
