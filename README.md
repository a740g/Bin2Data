# Bin2Data

`Bin2Data` is a command-line utility, written in [QB64-PE](https://www.qb64phoenix.com), that converts binary files into source code, making it easy to embed resources directly into your executables. It can also optionally compress the data.

![Screenshot](screenshot.png)

## Features

* Converts binary files into embeddable source code formats.
* Optionally compresses data using QB64-PE's `_DEFLATE$` function.
* Supports multiple output formats:
  * QB64-PE `DATA` statements (`.bi` file).
  * QB64-PE `CONST` string (`.bi` file).
  * C-style header with a `uint8_t` array (`.h` file).
* Can also output a raw zlib/deflate compressed file (`.deflate`).
* Base64 encoding for QB64-PE `DATA` and `CONST` formats.
* Command-line interface with support for wildcards for batch processing.

## Building and Using Bin2Data

### 1. Build from source

1. Clone this repository: `git clone --recurse-submodules https://github.com/a740g/Bin2Data.git`
2. Change into the directory: `cd Bin2Data`
3. Open `Bin2Data.bas` in the QB64-PE IDE and press `F5` to compile it.

### 2. Command-Line Usage

You can run `Bin2Data` from your terminal. The help screen shows the available options.

```text
Bin2Data: Converts binary files to QB64-PE data
Copyright (c) 2025 Samuel Gomes
https://github.com/a740g

Usage: Bin2Data [-w characters_per_data_line] [-i compression_level] [-d] [-c] [-p] [-r] [-s] [-o] [filespec]
   -w: A number specifying the number of characters per data line. 8-4096 (default 112)
   -i: A number specifying the compression level. 1-10
   -d: Generate DATA (.bas; default)
   -c: Generate a CONST (.bas; suitable for small files)
   -p: Generate a C array (.h)
   -r: Dump the raw compressed file (.deflate)
   -s: Disable compression and store the file instead
   -o: Overwrite output file if it exists

Note:
 * Will create filespec.bi/.h/.deflate (based on the switches)
 * Can bulk convert files using wildcards
 * filespec can be a URL
 * If filespec.bi/.h/.deflate exists, then it will not be overwritten (unless -o is specified)
 * Character per line may be changed in CONST mode due to QB64's 500 line continuation limit
 * C output is barebones. Use sizeof to get the array size
```

## Using the Generated Files

### QB64-PE Projects

For QB64-PE, you need `Base64.bas` from the [Toolbox64](https://github.com/a740g/Toolbox64) library.

#### Example: Using DATA statements (`-d` switch)

This is the default mode. It creates a `.bi` file containing a `RESTORE` label and `DATA` statements.

1. Generate the file: `Bin2Data my_asset.png`
    This will create `my_asset.png.bi`.

2. In your main QB64-PE program:

```vb
' Your program logic here
' ...

' Load the resource.
' The label name is generated based on the filename and size.
' You can find the exact label name in the generated .bi file.
RESTORE data_my_asset_png_bi_12345 
DIM buffer AS STRING
buffer = Base64_LoadResourceData

' Now 'buffer' contains the binary data of my_asset.png
' ...

' At the end of your main code, before SUBs and FUNCTIONs:

' Include the generated data file
'$INCLUDE:'my_asset.png.bi'

' At the bottom of you source code, after SUBs and FUNCTIONs:

' Include the library implementation (assuming you have cloned Toolbox64 in the include directory under your project directory)
'$INCLUDE:'include/Base64.bas'
```

#### Example: Using a CONST string (`-c` switch)

This mode is suitable for smaller files. It creates a `.bi` file containing `CONST` definitions.

1. Generate the file: `Bin2Data -c my_icon.ico`
    This will create `my_icon.ico.bi`.

2. In your main QB64-PE program:

```vb
' Include the generated constants file
'$INCLUDE:'my_icon.ico.bi'

' Your program logic here
' ...

' Load the resource using the generated CONSTs.
' The CONST names are based on the filename and size.
' Check the generated .bi file for the exact names.
DIM buffer AS STRING
buffer = Base64_LoadResourceString(DATA_MY_ICON_ICO_BI_123, SIZE_MY_ICON_ICO_BI_123, COMP_MY_ICON_ICO_BI_123)

' Now 'buffer' contains the binary data of my_icon.ico
' ...

' At the bottom of you source code, after SUBs and FUNCTIONs:

' Include the library implementation (assuming you have cloned Toolbox64 in the include directory under your project directory)
'$INCLUDE:'include/Base64.bas'
```

### C/C++ Projects

When using the `-p` switch, `Bin2Data` generates a C header file (`.h`).

1. Generate the file: `Bin2Data -p my_data.bin`
    This will create `my_data.bin.h`.

2. In your C/C++ code:

```c
#include "my_data.bin.h"
#include <stdio.h>
#include <string.h>

int main() {
    // The header defines symbols based on the filename and size, for example:
    // - const uint8_t my_data_bin_h_567[] = {...}; (the data array)
    // - #define SIZE_MY_DATA_BIN_H_567() 567       (original size)
    // - #define COMP_MY_DATA_BIN_H_567() 1         (is compressed)
    // - #define DATA_MY_DATA_BIN_H_567() ...       (pointer to data)
    
    // Note: If the data is compressed (COMP_...() is 1),
    // you will need a zlib/deflate library to decompress it.
    
    size_t dataSize = sizeof(my_data_bin_h_567);
    
    printf("Original size: %d\n", SIZE_MY_DATA_BIN_H_567());
    printf("Embedded size: %zu\n", dataSize);
    printf("Is compressed: %d\n", COMP_MY_DATA_BIN_H_567());

    // You can now use the my_data_bin_h_567 array.
    
    return 0;
}
```

## Notes

* This tool requires a [recent version](https://github.com/QB64-Phoenix-Edition/QB64pe/releases/latest) of [QB64-PE](https://www.qb64phoenix.com) to build.
* When you clone a repository that contains submodules, you need to run `git submodule update --init --recursive` to fetch the submodule content.

## Assets

* [Icon](https://www.iconarchive.com/artist/umut-pulat.html) by [Umut Pulat](http://12m3.deviantart.com/)
