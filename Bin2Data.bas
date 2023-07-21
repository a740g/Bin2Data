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
$VERSIONINFO:FILEVERSION#=2,0,0,0
$VERSIONINFO:PRODUCTVERSION#=2,0,0,0
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' CONSTANTS
'-----------------------------------------------------------------------------------------------------------------------
CONST BASE64_CHARACTERS_PER_LINE_MIN = 1
CONST BASE64_CHARACTERS_PER_LINE_DEFAULT = 20 * 4
CONST BASE64_CHARACTERS_PER_LINE_MAX = 4096
CONST DEFLATE_COMPRESSION_LEVEL = 0 ' whatever the default for the library
'-----------------------------------------------------------------------------------------------------------------------

'-----------------------------------------------------------------------------------------------------------------------
' GLOBAL VARIABLES
'-----------------------------------------------------------------------------------------------------------------------
DIM SHARED dataCPL AS LONG: dataCPL = BASE64_CHARACTERS_PER_LINE_DEFAULT ' characters per data line
DIM SHARED deflateIterations AS UNSIGNED BYTE ' compression level
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
    PRINT "   -i: A number specifying the compression level (anything more than 15 will be too slow). 1 - 255 (default 15)"
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
    PRINT "     Restore label_generated_by_bin2data"
    PRINT "     Dim buffer As String"
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
            deflateIterations = ClampLong(VAL(COMMAND$(argIndex)), 1, 255)
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
FUNCTION MakeLegalLabel$ (fileName AS STRING, fileSize AS UNSIGNED LONG)
    DIM AS UNSIGNED LONG i
    DIM AS STRING label: label = fileName

    FOR i = 1 TO LEN(label)
        SELECT CASE ASC(label, i)
            CASE KEY_0 TO KEY_9, KEY_UPPER_A TO KEY_UPPER_Z, KEY_LOWER_A TO KEY_LOWER_Z, KEY_UNDERSCORE ' legal characters
                ' NOP
            CASE ELSE
                ASC(label, i) = KEY_UNDERSCORE ' replace with underscore
        END SELECT
    NEXT

    MakeLegalLabel = "Data_" + label + "_" + TRIM$(STR$(fileSize)) + ":"
END FUNCTION


' This reads in fileNames and converts it to Base64 after compressing it (if needed)
' The file is then written as a QB64 include file
SUB MakeResource (fileName AS STRING)
    DIM AS STRING biFileName: biFileName = fileName + ".bi"

    IF FILEEXISTS(biFileName) AND NOT shouldOverwrite THEN
        PRINT biFileName; " already exists!"
        EXIT SUB
    END IF

    PRINT "Loading "; fileName


    ' Read in the whole file
    DIM AS STRING buffer: buffer = LoadFile(fileName)
    DIM AS UNSIGNED LONG ogSize: ogSize = LEN(buffer)

    IF ogSize < 1 THEN
        PRINT fileName; " is empty or could not be read!"
        EXIT SUB
    END IF


    PRINT "File size is"; ogSize; "bytes"

    ' Attempt to compress and see if we get any goodness
    IF NOT shouldStore THEN
        PRINT "Compressing data (this may take some time)"
        DIM AS STRING compBuf: compBuf = DeflatePro(buffer, deflateIterations)
    END IF

    PRINT "Writing to "; biFileName

    ' Open the output file and write the label
    DIM fh AS LONG: fh = FREEFILE
    OPEN biFileName FOR OUTPUT AS fh
    PRINT #fh, MakeLegalLabel(GetFileNameFromPathOrURL$(fileName), ogSize) ' write the label

    IF LEN(compBuf) < ogSize AND NOT shouldStore THEN ' we got goodness
        buffer = EncodeBase64(compBuf) ' we do not need the original buffer contents
        PRINT #fh, "Data "; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(TRUE))

        PRINT USING "Compressed###.##%"; 100 - 100! * LEN(compBuf) / ogSize

        compBuf = EMPTY_STRING
    ELSE ' no goodness
        buffer = EncodeBase64(buffer) ' we do not need the original buffer contents
        PRINT #fh, "Data "; LTRIM$(STR$(ogSize)); ","; LTRIM$(STR$(LEN(buffer))); ","; LTRIM$(STR$(FALSE))
        compBuf = EMPTY_STRING

        PRINT "Stored"
    END IF

    DIM AS UNSIGNED LONG i
    FOR i = 1 TO LEN(buffer)
        IF (i - 1) MOD dataCPL = 0 THEN
            IF i > 1 THEN PRINT #fh, EMPTY_STRING
            PRINT #fh, "Data ";
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
