'-----------------------------------------------------------------------------------------------------------------------
' QB64-PE Binary to DATA converter
' Copyright (c) 2023 Samuel Gomes
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' HEADER FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/StringOps.bi'
'$INCLUDE:'include/MathOps.bi'
'$INCLUDE:'include/FileOps.bi'
'$INCLUDE:'include/Base64.bi'
'$INCLUDE:'include/Deflate.bi'
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' METACOMMANDS
'-----------------------------------------------------------------------------------------------------------------------
$NOPREFIX
$CONSOLE:ONLY
$EXEICON:'./Bin2Data.ico'
$VERSIONINFO:ProductName='Bin2Data'
$VERSIONINFO:CompanyName='Samuel Gomes'
$VERSIONINFO:LegalCopyright='Copyright (c) 2023 Samuel Gomes'
$VERSIONINFO:LegalTrademarks='All trademarks are property of their respective owners'
$VERSIONINFO:Web='https://github.com/a740g'
$VERSIONINFO:Comments='https://github.com/a740g'
$VERSIONINFO:InternalName='Bin2Data'
$VERSIONINFO:OriginalFilename='Bin2Data.exe'
$VERSIONINFO:FileDescription='Bin2Data executable'
$VERSIONINFO:FILEVERSION#=2,2,2,0
$VERSIONINFO:PRODUCTVERSION#=2,2,2,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
CONST BASE64_CHARACTERS_PER_LINE_MIN = SIZE_OF_BYTE
CONST BASE64_CHARACTERS_PER_LINE_DEFAULT = 28 * SIZE_OF_LONG
CONST BASE64_CHARACTERS_PER_LINE_MAX = 4096
CONST DEFLATE_COMPRESSION_LEVEL = 0 ' whatever is the default for the library
CONST IDENTIFIER_STYLE_DATA = 0
CONST IDENTIFIER_STYLE_NAME = 1
CONST IDENTIFIER_STYLE_SIZE = 2
CONST IDENTIFIER_STYLE_COMP = 3
CONST TYPE_SIGIL_BYTE = "%%"
CONST TYPE_SIGIL_ULONG = "~&"
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' GLOBAL VARIABLES
'-----------------------------------------------------------------------------------------------------------------------
DIM SHARED dataCPL AS LONG: dataCPL = BASE64_CHARACTERS_PER_LINE_DEFAULT ' characters per data line
DIM SHARED deflateIterations AS UNSIGNED INTEGER ' compression level
DIM SHARED AS BYTE shouldOverwrite, shouldStore, shouldGenCONST ' overwrite, compress and const enable flags
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
CHDIR STARTDIR$

' If there are no command line parameters just show some info and exit
IF COMMANDCOUNT < 1 OR GetProgramArgumentIndex(KEY_QUESTION_MARK) > 0 THEN
    COLOR 7
    PRINT "Bin2Data: Converts binary files to QB64-PE DATA / CONST"
    PRINT "Copyright (c) 2023 Samuel Gomes"
    PRINT "https://github.com/a740g"
    PRINT
    PRINT "Usage: Bin2Data [-w characters_per_data_line] [-i compression_level] [-c] [-s] [-o] [filespec]"
    PRINT "   -w: A number specifying the number of characters per line."; BASE64_CHARACTERS_PER_LINE_MIN; "-"; BASE64_CHARACTERS_PER_LINE_MAX; "(default"; STR$(dataCPL); ")"
    PRINT "   -i: A number specifying the compression level (anything more than 15 will be too slow). 1 -"; UINTEGER_MAX; "(default 15)"
    PRINT "   -c: Store the data in a CONST rather than DATA (suitable for small files)"
    PRINT "   -s: Disable compression and store the file instead"
    PRINT "   -o: Overwrite output file if it exists"
    PRINT
    PRINT "Note:"
    PRINT " * This will create filespec.bi"
    PRINT " * Bulk convert files using wildcards"
    PRINT " * filespec can be a URL"
    PRINT " * If filespec.bi exists, then it will not be overwritten (unless -o is specified)"
    PRINT " * Character per line may be changed in CONST mode due to QB64's 500 line continuation limit"
    PRINT
    PRINT "Usage:"
    PRINT " 1. Encode the binary file using Bin2Data"
    PRINT " 2. Include the file or it's contents"
    PRINT " 3. Include Base64.bi at the top of your source"
    PRINT " 4. Include Base64.bas at the bottom of your source"
    PRINT " 5. Load the file:"
    COLOR 11
    PRINT "     DATA mode:"
    COLOR 14
    PRINT "         RESTORE label_generated_by_bin2data"
    PRINT "         DIM buffer AS STRING"
    PRINT "         buffer = Base64_LoadResourceData"
    PRINT
    COLOR 11
    PRINT "     CONST mode:"
    COLOR 14
    PRINT "         DIM buffer AS STRING"
    PRINT "         buffer = Base64_LoadResourceString(STRING_DATA_CONST, ORIGINAL_SIZE_CONST, IS_COMPRESSED_CONST)"
    COLOR 7
    SYSTEM
END IF

PRINT

' Convert all files requested
DIM argName AS INTEGER
DIM argIndex AS LONG: argIndex = 1 ' start with the first argument

DO
    argName = String_ToLowerCase(GetProgramArgument("wicso", argIndex))

    SELECT CASE argName
        CASE -1 ' ' no more arguments
            EXIT DO

        CASE KEY_LOWER_W
            argIndex = argIndex + 1 ' value at next index
            dataCPL = Math_ClampLong(VAL(COMMAND$(argIndex)), BASE64_CHARACTERS_PER_LINE_MIN, BASE64_CHARACTERS_PER_LINE_MAX)
            PRINT "Characters per data line set to"; dataCPL

        CASE KEY_LOWER_I
            argIndex = argIndex + 1 ' value at next index
            deflateIterations = Math_ClampLong(VAL(COMMAND$(argIndex)), 1, UINTEGER_MAX)
            PRINT "Compression level set to"; deflateIterations

        CASE KEY_LOWER_C
            shouldGenCONST = TRUE
            PRINT "CONST generation enabled"

        CASE KEY_LOWER_S
            shouldStore = TRUE
            PRINT "Store mode enabled"

        CASE KEY_LOWER_O
            shouldOverwrite = TRUE
            PRINT "Overwrite mode enabled"

        CASE ELSE ' probably a file name
            MakeResource COMMAND$(argIndex)
    END SELECT

    argIndex = argIndex + 1 ' move to the next index
LOOP UNTIL argName = -1

SYSTEM
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' FUNCTIONS & SUBROUTINES
'-----------------------------------------------------------------------------------------------------------------------
' Removes invalid characters from filenames and makes a valid QB64 identifier
' This will limit the label size to 40 characters (QB64 max)
FUNCTION MakeLegalIdentifier$ (fileName AS STRING, fileSize AS UNSIGNED LONG, style AS LONG)
    CONST LABEL_LENGTH_MAX = 40
    CONST PREFIX_DATA = "data_"
    CONST PREFIX_SIZE = "SIZE_"
    CONST PREFIX_COMP = "COMP_"
    CONST CONST_KEYWORD = "CONST "
    CONST ASSIGNMENT_STRING = " ="
    CONST LABEL_TERMINATOR = ":"

    DIM nameText AS STRING: nameText = fileName

    DIM i AS UNSIGNED LONG: FOR i = 1 TO LEN(nameText)
        SELECT CASE ASC(nameText, i)
            CASE KEY_0 TO KEY_9, KEY_UPPER_A TO KEY_UPPER_Z, KEY_LOWER_A TO KEY_LOWER_Z, KEY_UNDERSCORE ' legal characters
                ' NOP
            CASE ELSE
                ASC(nameText, i) = KEY_UNDERSCORE ' replace with underscore
        END SELECT
    NEXT

    DIM sizeText AS STRING: sizeText = LTRIM$(STR$(fileSize))
    i = LABEL_LENGTH_MAX - LEN(sizeText) - 6 ' LABEL_LENGTH_MAX - text size of file size - len("data_" + "_")
    IF LEN(nameText) > i THEN nameText = LEFT$(nameText, i) ' chop the name if everything is adding up to be more than LABEL_LENGTH_MAX chars
    nameText = nameText + "_" + sizeText

    SELECT CASE style
        CASE IDENTIFIER_STYLE_NAME
            MakeLegalIdentifier = CONST_KEYWORD + UCASE$(PREFIX_DATA + nameText) + ASSIGNMENT_STRING

        CASE IDENTIFIER_STYLE_SIZE
            MakeLegalIdentifier = CONST_KEYWORD + PREFIX_SIZE + UCASE$(nameText) + TYPE_SIGIL_ULONG + ASSIGNMENT_STRING

        CASE IDENTIFIER_STYLE_COMP
            MakeLegalIdentifier = CONST_KEYWORD + PREFIX_COMP + UCASE$(nameText) + TYPE_SIGIL_BYTE + ASSIGNMENT_STRING

        CASE ELSE ' IDENTIFIER_STYLE_DATA
            MakeLegalIdentifier = PREFIX_DATA + LCASE$(nameText) + LABEL_TERMINATOR
    END SELECT
END FUNCTION


' This reads in fileNames and converts it to Base64 after compressing it (if needed)
' The file is then written as a QB64 header file
SUB MakeResource (fileName AS STRING)
    CONST HEADER_FILE_EXTENSION = ".bi"
    CONST STRING_CONCATENATION = " +"
    CONST LINE_CONTINUATION = " _"
    CONST DATA_STATEMENT = "DATA "
    CONST LINE_CONTINUATION_MAX = 500
    CONST CONST_INDENT_CHARS = 4

    DIM biFileName AS STRING

    IF LEN(GetDriveOrSchemeFromPathOrURL(fileName)) > 2 THEN
        biFileName = GetLegalFileName(GetFileNameFromPathOrURL$(fileName)) + HEADER_FILE_EXTENSION
    ELSE
        biFileName = fileName + HEADER_FILE_EXTENSION
    END IF

    IF FILEEXISTS(biFileName) AND NOT shouldOverwrite THEN
        PRINT biFileName; " already exists!"
        EXIT SUB
    END IF

    PRINT "Loading "; fileName

    ' Read in the whole file
    DIM buffer AS STRING: buffer = LoadFile(fileName)
    DIM ogSize AS UNSIGNED LONG: ogSize = LEN(buffer)

    IF ogSize < 1 THEN
        PRINT fileName; " is empty or could not be read!"
        EXIT SUB
    END IF

    PRINT "File size is"; ogSize; "bytes"

    ' Attempt to compress and see if we get any goodness
    IF NOT shouldStore THEN
        PRINT "Compressing data (this may take some time) ... ";
        DIM compBuf AS STRING: compBuf = DeflatePro(buffer, deflateIterations)
        PRINT "done"
    END IF

    PRINT "Writing to "; biFileName

    ' Open the output file and write the label
    DIM fh AS LONG: fh = FREEFILE
    OPEN biFileName FOR OUTPUT AS fh

    IF shouldGenCONST THEN
        PRINT #fh, MakeLegalIdentifier(GetFileNameFromPathOrURL$(fileName), ogSize, IDENTIFIER_STYLE_SIZE) + STR$(ogSize) + TYPE_SIGIL_ULONG ' write the size const
    ELSE
        PRINT #fh, MakeLegalIdentifier(GetFileNameFromPathOrURL$(fileName), ogSize, IDENTIFIER_STYLE_DATA) ' write the label
    END IF

    IF LEN(compBuf) < ogSize AND NOT shouldStore THEN ' we got goodness
        PRINT "Encoding data (this may take some time) ... ";
        buffer = Base64_Encode(compBuf) ' we do not need the original buffer contents
        PRINT "done"

        IF shouldGenCONST THEN
            PRINT #fh, MakeLegalIdentifier(GetFileNameFromPathOrURL$(fileName), ogSize, IDENTIFIER_STYLE_COMP) + " " + STR$(TRUE) + TYPE_SIGIL_BYTE ' write if file is compressed
        ELSE
            PRINT #fh, DATA_STATEMENT; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(TRUE)) ' write the DATA header
        END IF

        PRINT USING "Compressed###.##%"; 100 - 100! * LEN(compBuf) / ogSize

        compBuf = STRING_EMPTY
    ELSE ' no goodness
        PRINT "Encoding data (this may take some time) ... ";
        buffer = Base64_Encode(buffer) ' we do not need the original buffer contents
        PRINT "done"

        IF shouldGenCONST THEN
            PRINT #fh, MakeLegalIdentifier(GetFileNameFromPathOrURL$(fileName), ogSize, IDENTIFIER_STYLE_COMP) + STR$(FALSE) + TYPE_SIGIL_BYTE ' write if file is compressed
        ELSE
            PRINT #fh, DATA_STATEMENT; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(FALSE)) ' write the DATA header
        END IF

        PRINT "Stored"

        compBuf = STRING_EMPTY
    END IF

    IF shouldGenCONST THEN
        PRINT #fh, MakeLegalIdentifier(GetFileNameFromPathOrURL$(fileName), ogSize, IDENTIFIER_STYLE_NAME) + LINE_CONTINUATION ' write the const name

        ' Adjust character per line to work around QB64 limit if needed
        IF LEN(buffer) \ dataCPL >= LINE_CONTINUATION_MAX - 2 THEN
            dataCPL = LEN(buffer) \ (LINE_CONTINUATION_MAX - 2)
            PRINT "Characters per data line auto-changed to"; dataCPL
        END IF
    END IF

    DIM srcSizeRem AS _UNSIGNED LONG: srcSizeRem = LEN(buffer) MOD dataCPL ' remainder
    DIM srcSizeMul AS _UNSIGNED LONG: srcSizeMul = LEN(buffer) - srcSizeRem ' exact multiple

    DIM i AS UNSIGNED LONG: FOR i = 1 TO srcSizeMul STEP dataCPL
        IF shouldGenCONST THEN
            PRINT #fh, SPACE$(CONST_INDENT_CHARS); CHR$(KEY_QUOTATION_MARK); ' opening quotation mark
        ELSE
            PRINT #fh, DATA_STATEMENT; ' start of line DATA statement
        END IF

        PRINT #fh, MID$(buffer, i, dataCPL); ' the actual data chunk

        IF shouldGenCONST THEN
            PRINT #fh, CHR$(KEY_QUOTATION_MARK); ' closing quotation mark

            ' Now check if we need to write the line continuation combo for CONST string
            IF srcSizeRem > 0 OR i < srcSizeMul - dataCPL THEN
                PRINT #fh, STRING_CONCATENATION + LINE_CONTINUATION ' write a string concat op and then a line continuation
            ELSE
                PRINT #fh, STRING_EMPTY ' move to the next line
            END IF
        ELSE
            PRINT #fh, STRING_EMPTY ' nothing special for DATA, simply move to the next line
        END IF
    NEXT i

    IF srcSizeRem > 0 THEN
        IF shouldGenCONST THEN
            PRINT #fh, SPACE$(CONST_INDENT_CHARS); CHR$(KEY_QUOTATION_MARK); ' opening quotation mark
        ELSE
            PRINT #fh, DATA_STATEMENT; ' start of line DATA statement
        END IF

        PRINT #fh, MID$(buffer, i, dataCPL); ' the actual data chunk

        IF shouldGenCONST THEN
            PRINT #fh, CHR$(KEY_QUOTATION_MARK) ' closing quotation mark
        ELSE
            PRINT #fh, STRING_EMPTY ' nothing special for DATA, simply move to the next line
        END IF
    END IF

    PRINT #fh, STRING_EMPTY ' put a newline (required for CONST!)

    CLOSE fh

    PRINT "Done"
END SUB
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' MODULE FILES
'-----------------------------------------------------------------------------------------------------------------------
'$INCLUDE:'include/ProgramArgs.bas'
'$INCLUDE:'include/StringOps.bas'
'$INCLUDE:'include/FileOps.bas'
'$INCLUDE:'include/Base64.bas'
'$INCLUDE:'include/Deflate.bas'
'-----------------------------------------------------------------------------------------------------------------------
'-----------------------------------------------------------------------------------------------------------------------
