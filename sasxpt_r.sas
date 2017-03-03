;/******************************************************************;
*      PROGRAM: sasxpt_r.sas
*      VERSION: 0.1.2
*       AUTHOR: @aleaiacta
********************************************************************;
*  DESCRIPTION: This program first imports SAS xpt files and generates 
*               temporary sas7bdat files (with SAS formats). Afterwards a
*               Data Definition Table will be generated. Finally an .R script 
*               file (impsas_xpt.R) will be generated and exported to the (setwd) working directory;
*               The .R script file contains R Code and can be run in R or RStudio.
********************************************************************;
* RESTRICTION:  (a) Only simple SAS formats can be used, e.g. "N"="No"; or 0="No";
*                   Formats like e.g. 30 - 50 = 'Between 30 and 50' or picture formats cannot be used;
*               (b) SAS xport files should be created with the COPY procedure (modify %ImpDset, if warranted);
*               (c) Date/Time/DateTime identification may be incomplete (extend code, if warranted);
*               (d) Times will be imported as "0001-01-01" [date] and [time] - as.POSIXct() and afterwards
*                   converted to 'chron' times class.
*                   Change or delete code, if desired;
********************************************************************;
*        INPUT: (1) SAS directory where .xpt files are stored
*               (2) Set R setwd() working directory
*               (3) Define Sub-Directory where temporary SAS files are stored
*               (4) Include SAS formats (into work library)
*               (5) Optional: Rank order for formats (e.g. No, Yes, Unknown instead of 
*                   alphabetical order No, Unknown, Yes)
*               (6) Optional: Extend Code for identification of Dates, Times and Datetimes, if necessary;
****************************************************************;
*       OUTPUT: An R script file (impsas_xpt.R) that can be run in R and generates R dataframes with 
*               variable labels, levels, formats and R date/times;
********************************************************************;
* DEVELOPED ON: Windows 64-bit, SAS 9.4 TS Level 1M4, R 3.3.2;
********************************************************************;
*    TESTED ON: Windows 64-bit, SAS 9.4 TS Level 1M4, R 3.3.2
********************************************************************;
* 
* SAS MACRO(S): %ImpDset, %FromExp;
*
* PROGRAM HISTORY:
*  DATE MODIFIED  USERID      COMMENT
*  -------------  ----------  ----------------------------------------------
*  23Feb2017      alea        Code "if index(upcase(FORMAT), '') then" replaced by
*                             "if index(upcase(FORMAT), '') gt 0 then" in db01_DDT_DATE;
*  24Feb2017      alea        Code modified in db01_DDT_DATE (if DECODE ne "" added) - to include
*                             non user-defined formats only for identification of date, time, dtim;
*                             Code where=(START ne END or START="**OTHER**") modified - additional;
*                             data check;
*  03Mar2017      alea        split="~~" in proc report added for better readability (no line break with /);
****************************************************************************/

********** SAS Options (see also at end of program: options nobomfile);
options pageno=1 orientation=landscape;

********** Initialize: Delete all Formats in library Work;
proc format; run; quit;
proc catalog cat=work.formats et=format kill; run; quit;

********** Initialize: Delete all datasets in library Work;
data db1; delete; run; proc datasets library=work nodetails nolist KILL memtype=DATA; run;

********************************************************************;
********** MANUAL ENTRY:   (1) SET DIRECTORY WITH .XPT FILENAMES 
**********                 (2) SET R SETWD PATH 
**********                 (3) LIBRARY WITH TEMP. SAS FILES 
**********                 (4) READ SAS FORMATS;
********** ---- BEGIN ----------------------------------------------;
********************************************************************;

********** (1) Set Directory, e.g. C:\Temp\R where .xpt Files are Stored - Do not Change Attributes of Pipe;
filename dir_list pipe 'dir "C:\Temp\R"  /a:-d-h-s /b';

********** (2) Path with XPT Files and R Working Directory "setwd" (CAVE: use slash '/' instead of back slash '\');
**********     NOTE: Must be the same path as defined in (1) dir_list;
%let RSETWD=C:/Temp/R/;

********** (3) Sub-Directory where .xpt Datasets will be converted to temporary .sas7bdat Datasets;
libname tmpsas "C:\Temp\R\SAS_TEMP";

********** (4) Read SAS Formats (SAS program with proc format, or SAS Format Catalog) into 'work.formats';
**********     NOTE: Formats must be available in Library 'work';
%include "C:\Temp\R\Formats01.sas";

********** Or Create SAS Formats here e.g.:; /*
********** Note that the 'notsorted' option will preserve the rank order (instead of alphabetic order) - very useful;
proc format;
     value $NY (notsorted)
           "N"="No"
           "Y"="Yes"
           "U"="Unknown";
run;
quit;
**********/

********** Or copy format catalog to work.formats e.g.:; /*
proc catalog cat=yourlib.yourcatalog;
    copy out=work.formats;
run;
quit;
**********/

********************************************************************;
********** MANUAL ENTRY    (1) SET DIRECTORY WITH .XPT FILENAMES 
**********                 (2) SET R SETWD PATH 
**********                 (3) LIBRARY WITH TEMP. SAS FILES 
**********                 (4) READ SAS FORMATS;
********** ---- END ------------------------------------------------;
********************************************************************;

********************************************************************;
********** AUTOMATIC STEP II.: CONVERT ALL XPT DATASETS TO SAS DATASETS;
********** 
********** ---- BEGIN ----------------------------------------------;
********************************************************************;

********** All temporary .sas7bdat Files will be deleted:;
data tmpsas.db1; delete; run; proc datasets library=tmpsas nodetails nolist kill memtype=data; run;

********** Read pipe (all Files in Specified Directory);
data db00_Pipe;
    infile dir_list pad;
    length FNAME $ 80;
    input FNAME $ char80.;
run;

********** Derive FILENAME from previously created Dataset and keep .xpt files only;
data db00_XPT;
    set db00_Pipe (where=(scan(FNAME, 2) eq 'xpt'));
    FILENAME=scan(FNAME, 1);
run;

********** Read .xpt Files and Copy to .sas7bdat files (library 'tmpsas');
********** NOTE: Modify this macro, if xpt datasets can not be loaded (just as you usually do);
%macro ImpDset(XPTFILE);
    %local File;

    %let File = &RSETWD.&XPTFILE&..xpt;
    ;/********** Library File */ 
    libname xportin xport "&File";
 
    ;/********** .xpt File to .sas7bdat File */ 
    proc copy in=xportin out=tmpsas;
        select &XPTFILE;
    run;
    
    libname xportin clear;
%mend ImpDset;

********** Results from pipe were used to read all .xpt files and convert to .sas7bdat files;
%macro FromExp();
    %********** Invoke the %ImpDset macro for each transport file;
    data _null_; 
        set db00_XPT;
        call execute('%ImpDset('||trim(FILENAME)||');');
    run;
%mend FromExp;
 
********** Execute final Macro for Conversion;
%FromExp();

********************************************************************;
********** AUTOMATIC STEP II.: CONVERT ALL XPT DATASETS TO SAS DATASETS;
********** 
********** ---- END ------------------------------------------------;
********************************************************************;

********************************************************************;
********** AUTOMATIC Step III.: DERIVE R - CODE (R DATES, FACTORS, LEVELS, LABELS);
********** 
********** ---- BEGIN ----------------------------------------------;
********************************************************************;
 
********** Read all Files in Library 1;
proc sql;
    create table db00_library1a as
        select *
        from dictionary.columns
        where libname=upcase("tmpsas");
run;
quit;
proc sort data=db00_library1a; by FORMAT; run;

********** To avoid errors: Remove quotation mark " in column label(s);
data db00_library;
    set db00_library1a;
    ********** Translate "" in Label with ' to avoid errors (and put warning to log);
    if index(LABEL, '"') gt 0 then do;
       put 'WARNING: Do not use quotation mark within definition of Column Labels: ' MEMNAME NAME ' Label = ' LABEL "(quotation mark converted to ')";
       LABEL=translate(LABEL, "'", '"');
       end;
run;

********** Read Formats in Library Work (and sort by FMTNAME);
proc format library=work cntlout=db00_formats1a;
run;
quit;
proc sort data=db00_formats1a; by FMTNAME; run;

********** Derive Full SAS Format Name. Remove quotation mark. Add the same rank (1) for all formats;
data db00_formats1b;
    format FMTNAME RANK;
    format FULL_FNAME $49.;
    format LABEL $200.;
    set db00_formats1a;
    ********** Assign $ for char;
    if TYPE="C" then FULL_FNAME=trim("$") || strip(FMTNAME) || "."; * Add sign $ and dot .;
    if TYPE="N" then FULL_FNAME=trim(strip(FMTNAME)) || "."; * Add dot .;
    ********** Translate "" in Format Label with ' to avoid errors (and put warning to log);
    if index(LABEL, '"') gt 0 then do;
       put 'WARNING: Do not use quotation mark within format definitions, format: ' FULL_FNAME ' Label = ' LABEL "(quotation mark converted to ')";
       LABEL=translate(LABEL, "'", '"');
       end;
    ********** Translate "" in Format Definition Value (START) with ' to avoid errors (and put warning to log);
    if index(START, '"') gt 0 and START ne '"' then do;
       put 'WARNING: Do not use quotation mark within format definitions, format: ' FULL_FNAME ' Code = ' START "(quotation mark converted to ')";
       START=translate(START, "'", '"');
       END=translate(END, "'", '"');
       end;
    ********** Assign rank order to 1;
    RANK=1;
run;

title1 "sasxpt_r.sas ERROR: Listing of non-simple Formats ";
footnote1 "NOTE: Only 'simple' formats (e.g. 'Data-value-1' = 'Label 1') can be used. Change format definitions in proc format!";
proc report data=db00_formats1b nowd split="~~" ls=160 headskip headline missing;
    column FULL_FNAME START END LABEL;
    define FULL_FNAME / width=32 flow order;
    define START / width=30 flow;
    define END / width=30 flow;
    define LABEL / width=40 flow;

    break after FULL_FNAME / skip;
    where (START ne END or START="**OTHER**");
run;

********** Print Error to log:;
data _NULL_;
    set db00_formats1b (where=(START ne END or START="**OTHER**"));
    put "ERROR: Only 'simple' formats can be used. Change format definitions in 'proc format' for: " FULL_FNAME "Start = " START "End = " END;
run;

********************************************************************;
********** (5) ADD CODELIST ITEM RANK MANUALLY - BEGIN;
********************************************************************;

********** Reassign the rank order in case of a given SAS Format Catalog;
********** NOTE: Another more easy way is assigning the rank with (notsorted) option in proc format;
********** Uncomment and/or extend code below, if desired;
data db00_formats1c;
    set db00_formats1b;
    ********** NY (ordered by N, Y, U);
    *if FULL_FNAME="$NY." and START="N" then RANK=1;
    *if FULL_FNAME="$NY." and START="Y" then RANK=2;
    *if FULL_FNAME="$NY." and START="U" then RANK=3;
    ********** NYNA (ordered by N, Y, NA, U);
    *if FULL_FNAME="$NYNA." and START="N" then RANK=1;
    *if FULL_FNAME="$NYNA." and START="Y" then RANK=2;
    *if FULL_FNAME="$NYNA." and START="NA" then RANK=3;
    *if FULL_FNAME="$NYNA." and START="U" then RANK=4;

    ********** Other Rank Order Formats here (if desired):;
run;
PROC sort data=db00_formats1c; by FULL_FNAME RANK; run;

********************************************************************;
********** (5) ADD CODELIST ITEM RANK MANUALLY - END;
********************************************************************;

********** Generate Decodes for Starting Values in Variable FDEFTX; 
data db00_formats;
    format FDEFTX FORM $1000.;
    set db00_formats1c (keep=FULL_FNAME START LABEL TYPE);
    ********** Derive Format/Label (unquoted);
    FDEFTX=trim(strip(START)) || " = " || strip(LABEL);
    ********** In case of Numeric Formats: Derive Format FORM (unquoted);
    if TYPE="N" then do;
       FORM=strip(START);
       end;
    ********** In case of Character Formats: Derive Format FORM (quoted);
    if TYPE="C" then do;
       if not missing(dequote(START)) then FORM=quote(strip(START));
       if missing(dequote(START)) then FORM='""';
       end;
    keep FULL_FNAME FDEFTX TYPE FORM START LABEL;
run;


********** Generate Code for R Factor c(x,y) - exclude numeric format with dot; 
********** NOTE: A dot for numeric SAS values correspond to missing ('NA' in R);
data db00_R_factor;
    format DDTFORMAT $49.;
    format FORMCD $2000.;
    set db00_formats (where=(not(TYPE="N" and compress(START) eq "."))); * do not read numeric formats wit a dot '.';
    by FULL_FNAME;
    retain FORMCD;
    if first.FULL_FNAME then FORMCD=FORM;
    if not(first.FULL_FNAME) then FORMCD=trim(FORMCD) || ", " || (FORM);
    ********** Output when last:;
    if last.FULL_FNAME then do;
        DDTFORMAT=FULL_FNAME;
        output;
        end;
    keep DDTFORMAT FORMCD;
run;
proc sort data=db00_R_factor; by DDTFORMAT; run;


********** Generate Code for R levels c("x","y"); 
data db00_R_level;
    format DDTFORMAT $49.;
    format LEVELCD $2000.;
    set db00_formats (where=(not(TYPE="N" and compress(START) eq "."))); * do not read numeric formats wit a dot '.';
    by FULL_FNAME;
    retain LEVELCD;
    HELP=quote(strip(LABEL));
    if first.FULL_FNAME then LEVELCD=HELP;
    if not(first.FULL_FNAME) then LEVELCD=trim(LEVELCD) || ", " || (HELP);
    ********** Output when last;
    if last.FULL_FNAME then do;
       DDTFORMAT=FULL_FNAME;
       output;
       end;
keep DDTFORMAT LEVELCD;
run;
proc sort data=db00_R_level; by DDTFORMAT; run;

********** Generate List of Decodes (e.g. N = No, Y = Yes) for all (character and numeric) Formats; 
data db00_Decode;
    format DDTFORMAT $49.;
    format DECODE $2000.;
    set db00_formats;
    by FULL_FNAME;
    retain DECODE;
    if first.FULL_FNAME then DECODE=FDEFTX;
    if not(first.FULL_FNAME) then DECODE=trim(DECODE) || ", " || (FDEFTX);
    ********** Output when last;
    if last.FULL_FNAME then do;
       DDTFORMAT=FULL_FNAME;
       output;
       end;
    keep DDTFORMAT DECODE;
run;
proc sort data=db00_Decode; by DDTFORMAT; run;

********** Merge Files with R factors, R Levels, Decodes;
data db01_DDT;
    merge db00_library (in=DE) db00_Decode (rename=DDTFORMAT=FORMAT) db00_R_factor (rename=DDTFORMAT=FORMAT) db00_R_level (rename=DDTFORMAT=FORMAT);
    by FORMAT;
    if DE=1;
run;
proc sort data=db01_DDT; by LIBNAME MEMNAME VARNUM; run;

********************************************************************;
********** (6) MAKE MODIFICATION MANUALLY HERE, IF WARRANTED - BEGIN;
********************************************************************;

********** Derive TYP with element 'date' (Date format), 'time' (time format) and 'dtim' (Datetime format);
********** (necesary to generate corresponding R code);
********** NOTE: Extend code here, when you use other date/time formats;
data db01_DDT_DATE;
    format VARNUM MEMNAME NAME LABEL TYP FORMAT DECODE;
    set db01_DDT (keep=MEMNAME VARNUM NAME LABEL TYPE LENGTH FORMAT DECODE FORMCD LEVELCD);
    TYP=TYPE;
    ********** In case of non user-defined formats;
    if DECODE eq "" then do;
       ********** Identify Dates (extend code, if neccessary);
       if index(upcase(FORMAT), 'DATE') gt 0 then TYP="date";
       if index(upcase(FORMAT), 'DDMMYY') gt 0 then TYP="date";
       if index(upcase(FORMAT), 'MMDDYY') gt 0 then TYP="date";
       if index(upcase(FORMAT), 'YYMMDD') gt 0 then TYP="date";
       if index(upcase(FORMAT), 'E8601DT') gt 0 then TYP="date";
       if index(upcase(FORMAT), 'EURDFD') gt 0 then TYP="date"; * can be overwritten with EURDFDT (dtim);

       ********** Identify Time Formats;
       if index(upcase(FORMAT), 'TIME') gt 0 then TYP="time";
       if index(upcase(FORMAT), 'HHMM') gt 0 then TYP="time";

       ********** Identify DateTime Formats;
       if index(upcase(FORMAT), 'DATETIME') gt 0 then TYP="dtim";
       if index(upcase(FORMAT), 'DATEAMPM') gt 0 then TYP="dtim";
       if index(upcase(FORMAT), 'EURDFDT') gt 0 then TYP="dtim";
       end;
    label TYP="Type";
run;

********************************************************************;
********** (6) MAKE MODIFICATION MANUALLY HERE, IF WARRANTED - END;
********************************************************************;

********** Create Final Data Definition Table (DDT);
data db01_DDT_FINAL;
    set db01_DDT_DATE;
    label DECODE="Decode";
run;

********************************************************************;
********** PRINT DDT - BEGIN;
********************************************************************;
 
title1 'sasxpt_r.sas Data Definition Table';
footnote1 "RECOMMENDATION: All variables should have variable 'labels' and 'formats'";
proc report data=db01_DDT_FINAL nowd split="~~" ls=160 headskip headline missing;
    column MEMNAME NAME LABEL TYP FORMAT DECODE;
    define MEMNAME / width=10 flow order;
    define NAME / width=10;
    define TYP / width=5;
    define LABEL / width=40 flow;
    define FORMAT / width=12 flow;
    define DECODE / width=60 flow;

    break after MEMNAME / skip;
run;

********************************************************************;
********** PRINT DDT- END;
********************************************************************;

********** Transform SAS Date Variables to R Code with R Function as.Date();
data db02_R_DATES;
    set db01_DDT_DATE;
    format RCODE $2000.;
    R_NAME=trim(strip(MEMNAME)) || "$" || strip(NAME);
    ********** Derive Date;
    if TYP="date" then do;
       RCODE=trim(R_NAME) || " <- as.Date(" || strip(R_NAME) || ", origin = " || quote("1960-01-01") || ")";
       output;
       end;
    ********** Derive Time. Note: Prior to the time the date 0001-01-01 is assigned (POSIXct format);
    if TYP="time" then do;
       RCODE=trim(R_NAME) || " <- as.POSIXct(" || strip(R_NAME) 
             || ", tz = " || quote('GMT') || ", origin = " || quote("0001-01-01") || ")";
       output;
       ********** Delete code below or remove *times*, if you prefer POSIXct format for time variables;
       RCODE=trim(R_NAME) || " <- times(format("  || strip(R_NAME) 
             || ", " || quote(strip('%H:%M:%S')) || ")) # chron times class";
       output;
       end;
    ********** Derive Date Times;
    if TYP="dtim" then do;
       RCODE=trim(R_NAME) || " <- as.POSIXct(" || strip(R_NAME) || ", tz = " || quote('GMT') || ", origin = " || quote("1960-01-01") || ")";
       output;
       end;
run;

********** Derive R code for Factors, set Levels and Labels to all variables;
data db02_RCODE;
    set db01_DDT_DATE;
    format RCODE $2000.;
    R_NAME=trim(strip(MEMNAME)) || "$" || strip(NAME);
    ********** Generate R Code in Case of User Defined Formats, including Labels;
    if DECODE ne "" then do;
       RCODE=trim(R_NAME) || " <- factor(" || strip(R_NAME) || ", c(" || strip(FORMCD) || '), exclude = "")';
       output;
       RCODE=trim("levels(") || strip(R_NAME) || ") <- c(" || strip(LEVELCD) || ")";
       output;
       RCODE=trim("label(") || strip(R_NAME) || ") <- " || quote(strip(LABEL)) || "";
       output;
       end; 
    ********** Generate R code (labels) for all other Variables;
    if DECODE eq "" then do;
       RCODE=trim("label(") || strip(R_NAME) || ") <- " || quote(strip(LABEL)) || "";
       output;
       end;
run;

********************************************************************;
********** AUTOMATIC Step III.: DERIVE R - CODE (R DATES, FACTORS, LEVELS, LABELS);
********** 
********** ---- END ------------------------------------------------;
********************************************************************;
 
********************************************************************;
********** AUTOMATIC STEP IV. Create HEADER (setwd, libraries) and pool
**********                    with Rcode for dates labels and formats;
********** 
********** ---- BEGIN ----------------------------------------------;
********************************************************************;
 
********** Create R Header;
data HEADER;
    format RCODE $2000.;
    RCODE="#### Set R working directory";
    output;
    RCODE=trim("setwd(") || quote("&RSETWD") || ")";
    output;
    RCODE='#### Use install.packages("foreign"), install.packages("Hmisc"), install.packages("chron") if necessary';
    output;
    RCODE="library(foreign)";
    output;
    RCODE="library(Hmisc)";
    output;
    RCODE="library(chron)";
    output;
    RCODE="#### Read all .xpt files from working directory. Derive date/times. Set factor, levels, labels.";
    output;
run;

********** Commands for Read File;
data HEADER_READ;
    format RCODE $2000.;
    set db00_XPT;
    RCODE=trim(upcase(FILENAME)) || " <- read.xport(" || quote(strip(FNAME)) || ")";
    keep RCODE;
run;

********** Final R Code (libary, files, R dates, factors, levels, labels);
data RCODE;
    set HEADER HEADER_READ db02_R_DATES db02_RCODE;
    keep RCODE;
run;

********** Define File Name and encoding;
filename exptxt "&RSETWD.impsas_xpt.R" encoding="utf-8";

********** Set Options to No BOM (uft-8 file w/o BOM);
options nobomfile;

********** Export R code to .../impsas_xpt.R in UTF-8 Format;
data _null_;
    set RCODE;
    file exptxt;
    put RCODE;
run;

* NOTE: Open impsas_xpt.R in e.g. in RStudio and set encoding to utf-8;
* (e.g. RStudio -> File -> Reopen with encoding -> UTF-8);
* (This removes also special characters in line 1, if any);

********************************************************************;
********** AUTOMATIC STEP IV. Create HEADER (setwd, libraries) and pool
**********                    with Rcode for dates labels and formats;
********** 
********** ---- END ------------------------------------------------;
********************************************************************;

;/******************************************************************;
********** ---- END OF PROGRAM -------------------------------------;
********************************************************************/

********** Include Program to Check Critical Errors and Non-Critical Discrepancies;
********** Critical: There are values in the SAS database with no corresponding format definition/value labels;
********** Non Critical: there are SAS formats defined, e.g. U="Unknown", but the value in the database is missing;
%include "c:\Temp\R\saschk_format.sas";
