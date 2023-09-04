'-----------------------------------------------------------------------------------------------------------------------
' Binary to Data converter using Base64 library
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
$VERSIONINFO:ProductName=Bin2Data
$VERSIONINFO:CompanyName=Samuel Gomes
$VERSIONINFO:LegalCopyright=Copyright (c) 2023 Samuel Gomes
$VERSIONINFO:LegalTrademarks=All trademarks are property of their respective owners
$VERSIONINFO:Web=https://github.com/a740g
$VERSIONINFO:Comments=https://github.com/a740g
$VERSIONINFO:InternalName=Bin2Data
$VERSIONINFO:OriginalFilename=Bin2Data.exe
$VERSIONINFO:FileDescription=Bin2Data executable
$VERSIONINFO:FILEVERSION#=2,0,1,0
$VERSIONINFO:PRODUCTVERSION#=2,0,1,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
CONST BASE64_CHARACTERS_PER_LINE_MIN = SIZE_OF_BYTE
CONST BASE64_CHARACTERS_PER_LINE_DEFAULT = 28 * SIZE_OF_LONG
CONST BASE64_CHARACTERS_PER_LINE_MAX = 4096
CONST DEFLATE_COMPRESSION_LEVEL = 0 ' whatever is the default for the library
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' GLOBAL VARIABLES
'-----------------------------------------------------------------------------------------------------------------------
DIM SHARED dataCPL AS LONG: dataCPL = BASE64_CHARACTERS_PER_LINE_DEFAULT ' characters per data line
DIM SHARED deflateIterations AS UNSIGNED INTEGER ' compression level
DIM SHARED AS BYTE shouldOverwrite, shouldStore ' overwrite and compress flags
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' PROGRAM ENTRY POINT
'-----------------------------------------------------------------------------------------------------------------------
' Change to the directory specified by the environment
CHDIR STARTDIR$

' If there are no command line parameters just show some info and exit
IF COMMANDCOUNT < 1 OR GetProgramArgumentIndex(KEY_QUESTION_MARK) > 0 THEN
    COLOR 7
    PRINT
    PRINT "Bin2Data: Converts binary files to QB64 Data"
    PRINT
    PRINT "Copyright (c) 2023 Samuel Gomes"
    PRINT
    PRINT "https://github.com/a740g"
    PRINT
    PRINT "Usage: Bin2Data [-w characters_per_data_line] [-i compression_level] [-s] [-o] [filespec]"
    PRINT "   -w: A number specifying the number of characters per line."; BASE64_CHARACTERS_PER_LINE_MIN; "-"; BASE64_CHARACTERS_PER_LINE_MAX; "(default"; STR$(dataCPL); ")"
    PRINT "   -i: A number specifying the compression level (anything more than 15 will be too slow). 1 -"; UINTEGER_MAX; "(default 15)"
    PRINT "   -s: Disable compression and store the file instead"
    PRINT "   -o: Overwrite output file if it exists"
    PRINT
    PRINT "Note:"
    PRINT " * This will create filespec.bi"
    PRINT " * Bulk convert files using wildcards"
    PRINT " * filespec can be a URL"
    PRINT " * If filespec.bi exists, then it will not be overwritten (unless -o is specified)"
    PRINT
    PRINT "Usage:"
    PRINT " 1. Encode the binary file using Bin2Data"
    PRINT " 2. Include the file or it's contents"
    PRINT " 3. Include Base64.bi at the top of your source"
    PRINT " 4. Include Base64.bas at the bottom of your source"
    PRINT " 5. Load the file:"
    COLOR 14
    PRINT "     RESTORE label_generated_by_bin2data"
    PRINT "     DIM buffer AS STRING"
    PRINT "     buffer = LoadResource"
    COLOR 7
    PRINT
    SYSTEM
END IF

PRINT

' Convert all files requested
DIM argName AS INTEGER
DIM argIndex AS LONG: argIndex = 1 ' start with the first argument

DO
    argName = ToLowerCase(GetProgramArgument("wiso", argIndex))

    SELECT CASE argName
        CASE -1 ' ' no more arguments
            EXIT DO

        CASE KEY_LOWER_W ' w
            argIndex = argIndex + 1 ' value at next index
            dataCPL = ClampLong(VAL(COMMAND$(argIndex)), BASE64_CHARACTERS_PER_LINE_MIN, BASE64_CHARACTERS_PER_LINE_MAX)
            PRINT "Characters per data line set to"; dataCPL

        CASE KEY_LOWER_I ' i
            argIndex = argIndex + 1 ' value at next index
            deflateIterations = ClampLong(VAL(COMMAND$(argIndex)), 1, UINTEGER_MAX)
            PRINT "Compression level set to"; deflateIterations

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
' Removes invalid characters from filenames and makes a valid QB64 line label
' This will limit the label size to 40 characters (QB64 max)
FUNCTION MakeLegalLabel$ (fileName AS STRING, fileSize AS UNSIGNED LONG)
    CONST LABEL_LENGTH_MAX = 40

    DIM nameText AS STRING: nameText = fileName

    DIM i AS UNSIGNED LONG: FOR i = 1 TO LEN(nameText)
        SELECT CASE ASC(nameText, i)
            CASE KEY_0 TO KEY_9, KEY_UPPER_A TO KEY_UPPER_Z, KEY_LOWER_A TO KEY_LOWER_Z, KEY_UNDERSCORE ' legal characters
                ' NOP
            CASE ELSE
                ASC(nameText, i) = KEY_UNDERSCORE ' replace with underscore
        END SELECT
    NEXT

    DIM sizeText AS STRING: sizeText = TRIM$(STR$(fileSize))
    i = LABEL_LENGTH_MAX - LEN(sizeText) - 6 ' LABEL_LENGTH_MAX - text size of file size - len("data_" + "_")
    IF LEN(nameText) > i THEN nameText = LEFT$(nameText, i) ' chop the name if everything is adding up to be more than LABEL_LENGTH_MAX chars

    MakeLegalLabel = "data_" + LCASE$(nameText) + "_" + sizeText + ":"
END FUNCTION


' This reads in fileNames and converts it to Base64 after compressing it (if needed)
' The file is then written as a QB64 header file
SUB MakeResource (fileName AS STRING)
    CONST HEADER_FILE_EXTENSION = ".bi"

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
    PRINT #fh, MakeLegalLabel(GetFileNameFromPathOrURL$(fileName), ogSize) ' write the label

    IF LEN(compBuf) < ogSize AND NOT shouldStore THEN ' we got goodness
        PRINT "Encoding data (this may take some time) ... ";
        buffer = EncodeBase64(compBuf) ' we do not need the original buffer contents
        PRINT "done"

        PRINT #fh, "DATA "; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(TRUE))

        PRINT USING "Compressed###.##%"; 100 - 100! * LEN(compBuf) / ogSize

        compBuf = EMPTY_STRING
    ELSE ' no goodness
        PRINT "Encoding data (this may take some time) ... ";
        buffer = EncodeBase64(buffer) ' we do not need the original buffer contents
        PRINT "done"

        PRINT #fh, "DATA "; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(FALSE))
        compBuf = EMPTY_STRING

        PRINT "Stored"
    END IF

    DIM i AS UNSIGNED LONG: FOR i = 1 TO LEN(buffer)
        IF (i - 1) MOD dataCPL = 0 THEN
            IF i > 1 THEN PRINT #fh, EMPTY_STRING
            PRINT #fh, "DATA ";
        END IF

        PRINT #fh, CHR$(ASC(buffer, i));
    NEXT

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
