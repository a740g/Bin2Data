# Bin2Data Documentation

`Bin2Data` is a command-line tool written in [QB64-PE](https://www.qb64phoenix.com) that converts binary files (images, audio, fonts, 3D models, configuration data, etc.) into source code. This enables embedding external resources directly into executables without needing external asset files or installer bundles.

---

## Table of Contents

- [How Bin2Data Works](#how-bin2data-works)
- [Command-Line Options Reference](#command-line-options-reference)
- [Output Formats and Use Cases](#output-formats-and-use-cases)
  - [Use Case 1: QB64-PE DATA Statements (`-d`)](#use-case-1-qb64-pe-data-statements--d)
    - [1A. Integration with Toolbox64](#1a-integration-with-toolbox64)
    - [1B. Standalone Integration (No External Libraries)](#1b-standalone-integration-no-external-libraries)
    - [1C. Loading Embedded Images and Sounds Directly](#1c-loading-embedded-images-and-sounds-directly)
  - [Use Case 2: QB64-PE CONST Strings (`-c`)](#use-case-2-qb64-pe-const-strings--c)
    - [2A. Integration with Toolbox64](#2a-integration-with-toolbox64)
    - [2B. Standalone Integration (No External Libraries)](#2b-standalone-integration-no-external-libraries)
  - [Use Case 3: C Header Arrays in QB64-PE (`-p`)](#use-case-3-c-header-arrays-in-qb64-pe--p)
  - [Use Case 4: Raw Deflate Dumps (`-r`)](#use-case-4-raw-deflate-dumps--r)
    - [4A. Reading and Inflating Raw Deflate Streams in QB64-PE](#4a-reading-and-inflating-raw-deflate-streams-in-qb64-pe)
  - [Use Case 5: Custom Width and Compression Control](#use-case-5-custom-width-and-compression-control)
  - [Use Case 6: Batch and Wildcard Processing](#use-case-6-batch-and-wildcard-processing)
  - [Use Case 7: Remote File and URL Conversion](#use-case-7-remote-file-and-url-conversion)

---

## How Bin2Data Works

When processing an input file:

1. **Reading**: The input file is loaded from disk or downloaded from an HTTP/HTTPS URL into memory.
2. **Compression**: Unless store mode (`-s`) is active, the data is compressed using QB64-PE's native `_DEFLATE$` function. If compression does not result in a smaller size, `Bin2Data` automatically falls back to storing the uncompressed data.
3. **Encoding & Formatting**:
   - For BASIC formats (`DATA` and `CONST`), data is Base64-encoded to produce safe, compact ASCII text lines.
   - For C format, raw or compressed bytes are emitted as a comma-separated `uint8_t` array.
   - For raw format, the compressed stream is written directly to a `.deflate` file.
4. **Identifier Sanitization**: Legal identifiers are generated based on the file name and size, avoiding collisions with QB64-PE reserved keywords, illegal characters, or length limitations (40 characters).

---

## Command-Line Options Reference

```text
Usage: Bin2Data [-w characters_per_data_line] [-i compression_level] [-d] [-c] [-p] [-r] [-s] [-o] [filespec]
```

| Option | Long Option | Argument | Description | Default |
| :--- | :--- | :--- | :--- | :--- |
| `-d` | `--data` | None | Generate QB64-PE `DATA` statements (`.bi` file). | Active by default |
| `-c` | `--const` | None | Generate QB64-PE `CONST` string definition (`.bi` file). | Off |
| `-p` | None | None | Generate a C/C++ header containing a `uint8_t` array (`.h` file). | Off |
| `-r` | `--raw` | None | Dump the raw Deflate-compressed binary stream (`.deflate` file). | Off |
| `-s` | `--store` | None | Bypass compression and store the data as-is. | Off (Compresses) |
| `-o` | `--overwrite` | None | Overwrite existing output files without prompting. | Off (Skips existing) |
| `-w` | `--width` | `8` to `4096` | Characters per data line in generated code. | `112` |
| `-i` | None | `1` to `10` | Compression level passed to `_DEFLATE$` (1 = fastest, 10 = best). | `0` (Library default) |
| `-h`, `-?` | `--help` | None | Display help and usage information. | |

> [!NOTE]
> Options apply sequentially to subsequent file arguments. You can change options between multiple file specifications on the same command line (e.g., `Bin2Data -c icon.ico -d sound.wav`).

---

## Output Formats and Use Cases

### Use Case 1: QB64-PE DATA Statements (`-d`)

This is the default and recommended mode for general asset embedding in QB64-PE. It generates a `.bi` include file containing:

- `$INCLUDEONCE` to prevent multiple includes.
- A unique label: `data_<sanitized_name>_<original_size>:`
- A header line: `DATA <originalSize>, <encodedSize>, <isCompressed>` (`isCompressed` is `-1` if compressed, `0` if stored).
- Followed by lines of Base64-encoded `DATA "..."`.

#### Generating

```sh
Bin2Data -d sprite.png
# Produces: sprite.png.bi
```

#### 1A. Integration with Toolbox64

If you use the [Toolbox64](https://github.com/a740g/Toolbox64) library, include `Resource/Resource.bi` and call `Resource_LoadBase64Data`:

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

'$INCLUDE:'include/Resource/Resource.bi'
'$INCLUDE:'sprite.png.bi'

' The label is named data_<filename>_<size> as generated in sprite.png.bi
RESTORE data_sprite_png_bi_12345

DIM buffer AS STRING
buffer = Resource_LoadBase64Data

PRINT "Successfully loaded sprite.png ("; LEN(buffer); "bytes)"
SYSTEM
```

#### 1B. Standalone Integration (No External Libraries)

You can decode `Bin2Data` `DATA` resources with pure native QB64-PE functions without any third-party dependencies:

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

'$INCLUDE:'sprite.png.bi'

RESTORE data_sprite_png_bi_12345
DIM spriteData AS STRING
spriteData = LoadEmbeddedData

PRINT "Loaded resource of size: "; LEN(spriteData); "bytes"
SYSTEM

''' @brief Decodes embedded Bin2Data DATA statements without external dependencies.
FUNCTION LoadEmbeddedData$
    DIM ogSize AS _UNSIGNED LONG
    DIM datSize AS _UNSIGNED LONG
    DIM isComp AS _BYTE

    ' Read header: original size, base64 size, compression flag
    READ ogSize, datSize, isComp

    DIM buffer AS STRING
    buffer = SPACE$(datSize)

    DIM i AS _UNSIGNED LONG: i = 0
    WHILE i < datSize
        DIM chunk AS STRING
        READ chunk
        MID$(buffer, i + 1) = chunk
        i = i + LEN(chunk)
    WEND

    ' Decode from Base64
    buffer = _BASE64DECODE$(buffer)

    ' Decompress if it was compressed
    IF isComp THEN
        buffer = _INFLATE$(buffer, ogSize)
    END IF

    LoadEmbeddedData = buffer
END FUNCTION
```

#### 1C. Loading Embedded Images and Sounds Directly

Once the binary data is in a string buffer, you can load images or audio using QB64-PE memory and stream routines:

```vb
' Loading an image directly from memory using _MEMIMAGE:
DIM imgData AS STRING
RESTORE data_texture_png_bi_45000
imgData = LoadEmbeddedData

' Create a 32-bit hardware/software image directly from decoded memory
DIM hImg AS LONG
hImg = _LOADIMAGE(imgData, 32, "memory")

IF hImg < -1 THEN
    SCREEN _NEWIMAGE(800, 600, 32)
    _PUTIMAGE (0, 0), hImg
    _DISPLAY
    SLEEP
    _FREEIMAGE hImg
END IF
```

---

### Use Case 2: QB64-PE CONST Strings (`-c`)

`CONST` mode creates a `.bi` file containing string constant definitions instead of `DATA` statements. This is well-suited for smaller assets (e.g. application icons, mouse cursors, small fonts, configuration files) because:

- No `RESTORE` label management is needed.
- Data can be loaded concurrently from multiple routines without cursor collision.

`Bin2Data` automatically splits the constant string using line continuation (`_`) and concatenation (`+`). If the asset would exceed QB64's maximum limit of 500 line continuations, `Bin2Data` automatically adjusts the characters-per-line width to stay within the compiler's limit.

#### Generating

```sh
Bin2Data -c app_icon.ico
# Produces: app_icon.ico.bi
```

The generated file defines:

- `CONST SIZE_<ID> = <size>~&`
- `CONST COMP_<ID> = <isCompressed>%%` (`-1%%` if compressed, `0%%` if stored)
- `CONST DATA_<ID> = "..." + _ ...`

#### 2A. Integration with Toolbox64

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

'$INCLUDE:'include/Resource/Resource.bi'
'$INCLUDE:'app_icon.ico.bi'

DIM iconData AS STRING
iconData = Resource_LoadBase64String(DATA_APP_ICON_ICO_BI_1420, SIZE_APP_ICON_ICO_BI_1420, COMP_APP_ICON_ICO_BI_1420)

PRINT "Loaded icon data: "; LEN(iconData); "bytes"
SYSTEM
```

#### 2B. Standalone Integration (No External Libraries)

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

'$INCLUDE:'app_icon.ico.bi'

DIM iconData AS STRING
iconData = DecodeEmbeddedConst(DATA_APP_ICON_ICO_BI_1420, SIZE_APP_ICON_ICO_BI_1420, COMP_APP_ICON_ICO_BI_1420)

PRINT "Decoded size: "; LEN(iconData); "bytes"
SYSTEM

''' @brief Decodes embedded Bin2Data CONST string without external dependencies.
FUNCTION DecodeEmbeddedConst$ (src AS STRING, ogSize AS _UNSIGNED LONG, isComp AS _BYTE)
    DIM buffer AS STRING
    buffer = _BASE64DECODE$(src)

    IF isComp THEN
        buffer = _INFLATE$(buffer, ogSize)
    END IF

    DecodeEmbeddedConst = buffer
END FUNCTION
```

---

### Use Case 3: C Header Arrays in QB64-PE (`-p`)

The `-p` switch generates a C header file (`.h`) containing a `const uint8_t` byte array and helper macros. QB64-PE allows integrating and linking C headers directly into BASIC programs using `DECLARE LIBRARY`.

#### Generating

```sh
Bin2Data -s -p palette.raw
# Produces: palette.raw.h
```

#### Generated Header Structure

```c
#pragma once

#include <stdint.h>

#define SIZE_PALETTE_RAW_H_768() 768
#define COMP_PALETTE_RAW_H_768() 0
#define DATA_PALETTE_RAW_H_768() ((uintptr_t)(&palette_raw_h_768[0]))

const uint8_t palette_raw_h_768[] = {
    ...
};
```

#### Calling Embedded C Headers from QB64-PE via DECLARE LIBRARY

You can compile the generated C header directly into your QB64-PE application using `DECLARE LIBRARY` and access the byte array via `_MEM`:

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

DECLARE LIBRARY "./palette.raw.h"
    FUNCTION SIZE_PALETTE_RAW_H_768& ALIAS "SIZE_PALETTE_RAW_H_768" ()
    FUNCTION COMP_PALETTE_RAW_H_768%% ALIAS "COMP_PALETTE_RAW_H_768" ()
    FUNCTION DATA_PALETTE_RAW_H_768%& ALIAS "DATA_PALETTE_RAW_H_768" ()
END DECLARE

DIM pData AS _OFFSET: pData = DATA_PALETTE_RAW_H_768
DIM pSize AS _UNSIGNED LONG: pSize = SIZE_PALETTE_RAW_H_768
DIM isComp AS _BYTE: isComp = COMP_PALETTE_RAW_H_768

PRINT "C array memory address: "; HEX$(pData)
PRINT "C array size:           "; pSize; "bytes"
PRINT "Is compressed:          "; isComp

' Copy bytes directly from the C array into a QB64-PE STRING using _MEM
DIM m AS _MEM: m = _MEM(pData, pSize)
DIM buffer AS STRING: buffer = SPACE$(pSize)
_MEMGET m, m.OFFSET, buffer
_MEMFREE m

' If the data was compressed, decompress using QB64-PE's native _INFLATE$
IF isComp THEN
    buffer = _INFLATE$(buffer)
END IF

PRINT "Data successfully loaded into string buffer ("; LEN(buffer); "bytes)"
SYSTEM
```

---

### Use Case 4: Raw Deflate Dumps (`-r`)

The `-r` option compresses the target file using raw Deflate (zlib) format and outputs it directly as `<filespec>.deflate`. This is useful for pre-compressing assets for custom package files, network transfers, or game bundles.

#### Generating

```sh
Bin2Data -r levels.dat
# Produces: levels.dat.deflate
```

#### 4A. Reading and Inflating Raw Deflate Streams in QB64-PE

```vb
$CONSOLE:ONLY
_DEFINE A-Z AS LONG
OPTION _EXPLICIT

DIM compressedData AS STRING
compressedData = _READFILE$("levels.dat.deflate")

' Decompress the raw deflate stream knowing original size (or passing omitted size)
DIM uncompressedData AS STRING
uncompressedData = _INFLATE$(compressedData)

PRINT "Original uncompressed size: "; LEN(uncompressedData); "bytes"
SYSTEM
```

---

### Use Case 5: Custom Width and Compression Control

#### Adjusting Output Line Width (`-w`)

By default, Base64 strings are split into lines of 112 characters. You can customize this width (from 8 to 4096 characters):

```sh
# Generate compact DATA lines formatted to 80 columns:
Bin2Data -w 80 -d file.bin

# Generate wide lines for modern text editors:
Bin2Data -w 256 -c file.bin
```

#### Adjusting Compression Level (`-i`)

Control the Deflate compression effort (1 = fastest, 10 = maximum compression):

```sh
# Fast compression for rapid build iteration:
Bin2Data -i 1 -d big_level.bin

# Maximum compression for production releases:
Bin2Data -i 10 -d big_level.bin
```

#### Disabling Compression (`-s`)

If an asset is already compressed (e.g. JPEG, MP3, PNG, OGG, ZIP), re-compressing it yields no size benefit. Use `-s` to store the raw bytes directly:

```sh
Bin2Data -s -d song.ogg
Bin2Data -s -p texture.jpg
```

---

### Use Case 6: Batch and Wildcard Processing

`Bin2Data` natively processes wildcards and batch file lists across Windows, Linux, and macOS:

```sh
# Convert all PNG files in the assets folder to DATA statements, overwriting existing outputs
Bin2Data -o -d assets/*.png

# Convert all icons to CONST strings
Bin2Data -o -c icons/*.ico

# Mix different modes in a single command:
Bin2Data -o -c icons/*.ico -d sounds/*.wav -p shaders/*.glsl
```

---

### Use Case 7: Remote File and URL Conversion

`Bin2Data` accepts HTTP and HTTPS URLs as input filespecs. It downloads the file directly into memory, compresses it, and outputs the generated file into the current working directory:

```sh
# Download and encode directly from a web URL:
Bin2Data -o -d https://raw.githubusercontent.com/a740g/Bin2Data/master/source/Bin2Data.ico

# Produces: Bin2Data.ico.bi in the local folder
```
