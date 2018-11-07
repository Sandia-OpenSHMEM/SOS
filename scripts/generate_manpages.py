#!/usr/bin/env python

# Copyright (c) 2018 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

import sys
import os
import string
import argparse
import time
import platform
import subprocess

# boldWordsC and boldWordsF are a set of the key words that will be
# bolded in the man page
boldWordsC = {"void", "int", "const", "size_t", "short", "ptrdiff_t", 
            "long", "TYPE", "shmem_global_exit(int", "shmem_ctx_t"}
boldWordsF = {"INTEGER", "CALL", "POINTER", "LOGICAL", "INTEGER*4",
                "INTEGER*8", "REAL*4", "REAL*8", "CHARACTER"}
sizes = ["8", "16", "32", "64", "128"]

def writePageHeader(functionName):
    """
    Write the page header for the man page. Takes in the form of:
    .TH [name of program] [section number] [center footer] [left footer] 
          [center header]
    """

    titleHeader = ".TH " + functionName.upper() + " 3 " \
                     + "\"Open Source Software Solutions, Inc.\" " \
                     + "\"OpenSHMEM Library Documentation\"" + "\n"
    return titleHeader

##########################################################################
##                            MACRO REPLACEMENTS                        ##
##########################################################################

def funcReplacements(tex):
    """
    Convert every FUNC macro with the bolded argument string 
    (.B <argument>).
    -- Special consideration to any periods after the macro, since a 
    period at the beginning of a line denotes a comment and the 
    entire line will not appear.
    (.BR <argument> .)
    """

    keyString = "\FUNC{"
    while (tex.find(keyString) != -1):
        index = tex.find(keyString)
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        innerText = tex[startBrace+1:endBrace]
        if(endBrace + 1 < len(tex) and 
            (tex[endBrace + 1] == "." or tex[endBrace + 1] == ",") 
            or tex[endBrace + 1] == ";"):
            tex = tex[:index] + "\n" + \
                    ".BR \"" + innerText + \
                    "\" " + tex[endBrace + 1] + "\n" + tex[endBrace+2:]
        else:                                                                
            tex = tex[:index] + "\n" + \
                ".B " + innerText + \
                "\n" + tex[endBrace+1:]
    return tex

def varReplacements(tex, keyString):
    """
    Convert every VAR/VARTH macro with the bolded argument string 
    (.I <argument>).
    -- Special consideration to any periods after the macro, since a 
    period at the beginning of a line denotes a comment and the 
    entire line will not appear.
    (.IR <argument> .)
    -- Special consideration to any argument of "VARTH", which will 
    require appending "th" to the argument string.
    """

    while (tex.find(keyString) != -1):
        index = tex.find(keyString)
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        innerText = tex[startBrace+1:endBrace]
        if keyString == "\VARTH{":
            innerText += "th"
        if (endBrace + 1 == len(tex)):
            tex = tex[:index] + "\n.I " + innerText + "\n"
        else:
            if (tex[endBrace + 1] != "."):
                tex = tex[:index] + "\n.I " + innerText \
                                + "\n" + tex[endBrace+1:]
            else:
                tex = tex[:index] + "\n.IR \"" + innerText \
                                + "\" .\n" + tex[endBrace+2:]
    return tex

def variableReplacements(tex):
    tex = varReplacements(tex, "\VAR{")
    tex = varReplacements(tex, "\VARTH{")
    return tex

def mBoxReplacements(tex):
    """
    Replace the Latex command "|mbox{<argument>}|" with just <argument>
    """

    while (tex.find("|\mbox") != -1):
        index = tex.find("|\mbox")
        sbrace = tex.find("{", index)
        ebrace = tex.find("}", sbrace)
        ebar = tex.find("|", sbrace)
        tex = tex[:index] + tex[sbrace+1:ebrace] \
                        + tex[ebar+1:]
    return tex

def boldReplacements(tex):
    """
    SPECIFIC TO shmem_reductions:
    Replace the Latex command: 
    "\\textbf{<NAME>} \\newline <text> <code> \\newline...\\bigskip" 
    --NAME will the title of a new section header with <text> as the 
    immediate content. 
    -- <code> will be replaced as normal (See function codeReplace(tex))
    -- \\bigskip will be replaced by the empty string. 
    """

    while (tex.find("\\textbf{") != -1):
        beg = tex.find("\\textbf{")
        sbrace = tex.find("{", beg)
        ebrace = tex.find("}", sbrace)
        fnewline = tex.find("\\newline", ebrace)
        snewline = tex.find("\\newline", fnewline + 1)
        clean = tex[fnewline + len("\\newline"):snewline]
        clean = macroReplacements(clean)
        tex = tex[:beg] + "\n.SH " + tex[sbrace+1:ebrace] \
                        + "\n" + clean \
                        + tex[snewline + len("\\newline"):]
    tex = tex.replace("\\bigskip", "")
    return tex

def italReplacements(tex):
    """ 
    Replace the Latex command "\\textit{<argument>}" with just 
    argument 
    """

    while (tex.find("\\textit{") != -1):
        beg = tex.find("\\textit{")
        sbrace = tex.find("{", beg)
        ebrace = tex.find("}", sbrace)
        tex = tex[:beg] + tex[sbrace+1:ebrace] \
                        + tex[ebrace+1:]
    return tex

def oprReplacements(tex):
    """
    Convert every OPR macro with the bolded argument string 
    (.I <argument>).
    -- Special consideration to any periods after the macro, since a 
    period at the beginning of a line denotes a comment and the 
    entire line will not appear.
    (.IR <argument> .)
    -- Special consideration to any argument of "VARTH", which will 
    require appending "th" to the argument string. 
    """

    while (tex.find("\\OPR") != -1):
        index = tex.find("\\OPR")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", index)
        if (endBrace + 1 == len(tex) or tex[endBrace + 1] != "."):
            tex = tex[:index] + "\n.I " + tex[startBrace+1:endBrace] \
                            + "\n" + tex[endBrace+1:]
        else:
            tex = tex[:index] + "\n.IR \"" + tex[startBrace+1:endBrace] \
                            + "\" .\n" + tex[endBrace+1:]
    return tex

def constReplacements(tex):
    """
    Replace the Latex command "\CONST{<argument>}" with just 
    argument. 
    """

    while (tex.find("\\CONST") != -1):
        index = tex.find("\\CONST")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        tex = tex[:index] + tex[startBrace+1:endBrace] + tex[endBrace+1:]

    while (tex.find("\\LibConstRef{") != -1):
        index = tex.find("\\LibConstRef{")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        tex = tex[:index] + tex[startBrace+1:endBrace] + tex[endBrace+1:]
    while (tex.find("\\footnote") != -1):
        index = tex.find("\\footnote")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        tex = tex[:index] + "(" + tex[startBrace+1:endBrace] + ")" + tex[endBrace+1:]
    return tex

def paragraphReplacements(tex):
    """
    Replace the Latex command "\CONST{<argument>}" with just 
    argument. 
    """

    while (tex.find("\\paragraph") != -1):
        index = tex.find("\\paragraph")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        tex = tex[:index] + ".SH " + tex[startBrace+1:endBrace] + "\n" + tex[endBrace+1:]
    return tex

def cTypeReplacements(tex):
    """
    Replace the Latex command "\CONST{<argument>}" with just 
    argument. 
    """

    while (tex.find("\\CTYPE") != -1):
        index = tex.find("\\CTYPE")
        startBrace = tex.find("{", index)
        endBrace = tex.find("}", startBrace)
        tex = tex[:index] + tex[startBrace+1:endBrace] + tex[endBrace+1:]
    return tex

def generalReplacements(tex):
    """
    Replace the common Latex macros that take in no arguments and all 
    Latex definitions that contain a backslash (which may have unde-
    fined behavior in the actual manpage). Some text may need to be 
    processed separately if it is followed by a period, which will 
    cause all text in the same line following the period to be not 
    shown.
    """

    tex = tex.replace("\openshmem{}", "OpenSHMEM")
    tex = tex.replace("\openshmem", "OpenSHMEM")
    tex = tex.replace("\\acp{PE}", "PEs")
    tex = tex.replace("\\ac{PE}", "PE")
    tex = tex.replace("\\ac{MPI}", "MPI")
    tex = tex.replace("\\acp{AMO}", "AMOs")
    tex = tex.replace("\\ac{AMO}", "AMO")
    tex = tex.replace("\\ac{API}", "API")
    tex = tex.replace("\\acp{RMA}", "RMAs")
    tex = tex.replace("\\ac{RMA}", "RMA")
    tex = tex.replace("\\ac{MPMD}", "Multiple Program Multiple Data (MPMD)")
    tex = tex.replace("\\CorCppFor", " C/C++/Fortran")
    tex = tex.replace("\\CorCpp{}", " C/C++")
    tex = tex.replace("\\CorCpp", " C/C++")
    tex = tex.replace("\\Fortran{}", "Fortran")
    tex = tex.replace("\\Fortran", "Fortran")
    tex = tex.replace("\\Cstd", "C")
    tex = tex.replace("\\PUT{}", "PUT")
    tex = tex.replace("\\GET{}", "GET")
    tex = tex.replace("\\SIZE{}", "SIZE")
    tex = tex.replace("\\activeset.", "\n.IR \"Active set\" .\n")
    tex = tex.replace("\\activeset{}", "\\activeset")
    tex = tex.replace("\\activeset", "\n.I \"Active set\"\n")
    tex = tex.replace("\\activeset ", "\n.I \"Active set\"\n")
    tex = tex.replace("\\dest.", "\n.IR \"dest\" .\n")
    tex = tex.replace("\\source.", "\n.IR \"source\" .\n")
    tex = tex.replace("\\dest{}", "\n.I \"dest\"\n")
    tex = tex.replace("\\source{}", "\n.I \"source\"\n")
    tex = tex.replace("\\TYPE{}", "TYPE")
    tex = tex.replace("\\TYPENAME{}", "TYPENAME")
    return tex

##########################################################################
##                            SECTION REPLACEMENTS                      ##
##########################################################################

def descrReplacements(tex):
    """
    Search tex file for the keyString "\\apidecription{" and its matching
    parenthesis. All text between will be processed such that there are no
    consecutive spaces, no tabs, and unnecessary "\\n". The text will then 
    have all the macros replaced and put back into its corresponding place
    in the text file. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of the processed text, respectively, for differenti-
    ation between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment.
    """

    startOfText = tex.find("\\apidescription{")
    endOfText = findMatchingBrace(tex, tex.find("{", startOfText))
    sectionText = cleanText(tex[startOfText:endOfText])
    tex = tex[:startOfText] + \
            "./ sectionStart\n" + "\n.SS API Description\n" + \
            sectionText + "\n./ sectionEnd\n" + tex[endOfText+1:]
    tex = tex.replace("\\apidescription{", "")
    return tex

def retReplacements(tex):
    """
    Search tex file for the keyString "\\apireturnavalues{" and its matching
    parenthesis. All text between will be processed such that there are no
    consecutive spaces, no tabs, and unnecessary "\\n". The text will then 
    have all the macros replaced and put back into its corresponding place
    in the text file. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of the processed text, respectively, for differenti-
    ation between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    startOfText = tex.find("\\apireturnvalues{")
    endOfText = findMatchingBrace(tex, tex.find("{", startOfText))
    sectionText = cleanText(tex[startOfText:endOfText])
    tex = tex[:startOfText] + \
            "./ sectionStart\n" + "\n.SS Return Values\n" + \
            sectionText + "\n./ sectionEnd\n" + tex[endOfText + 1:]
    tex = tex.replace("\\apireturnvalues{", "")
    return tex

def notesReplacements(tex):
    """
    Search tex file for the keyString "\\apinotes{" and its matching
    parenthesis. All text between will be processed such that there are no
    consecutive spaces, no tabs, and unnecessary "\\n". The text will then 
    have all the macros replaced and put back into its corresponding place
    in the text file. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of the processed text, respectively, for differenti-
    ation between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    startOfText = tex.find("\\apinotes{")
    endOfText = findMatchingBrace(tex, tex.find("{", startOfText))
    sectionText = cleanText(tex[startOfText:endOfText])
    tex = tex[:startOfText] + \
            "./ sectionStart\n" + "\n.SS API Notes\n" + \
            sectionText + "\n./ sectionEnd\n" + tex[endOfText + 1:]
    tex = tex.replace("\\apinotes{", "")
    return tex

def impnotesReplacements(tex):
    """
    Search tex file for the keyString "\\apiimpnotes{" and its matching
    parenthesis. All text between will be processed such that there are no
    consecutive spaces, no tabs, and unnecessary "\\n". The text will then 
    have all the macros replaced and put back into its corresponding place
    in the text file. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of the processed text, respectively, for differenti-
    ation between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    startOfText = tex.find("\\apiimpnotes{")
    endOfText = findMatchingBrace(tex, tex.find("{", startOfText))
    sectionText = cleanText(tex[startOfText:endOfText])
    tex = tex[:startOfText] + \
            "./ sectionStart\n" + "\n.SS Note to Implementors\n" + \
            sectionText + "\n./ sectionEnd\n" + tex[endOfText + 1:]
    tex = tex.replace("\\apiimpnotes{", "")
    return tex

def sumReplacements(tex, functionName):
    """
    Search tex file for the keyString "\\apisummary{" and its matching
    parenthesis. All text between will be processed such that there are no
    consecutive spaces, no tabs, and unnecessary "\\n". The text will then 
    have all the macros replaced and put back into its corresponding place
    in the text file. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of the processed text, respectively, for differenti-
    ation between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    startOfText = tex.find("\\apisummary{")
    endOfText = findMatchingBrace(tex, tex.find("{", startOfText))
    sectionText = cleanText(tex[startOfText:endOfText])
    tex = tex[:startOfText] + \
            "./ sectionStart\n" + \
            ".SH NAME\n" + functionName + " \- " \
            + sectionText + "\n" + \
            "./ sectionEnd\n" + tex[endOfText+1:]
    tex = tex.replace("\\apisummary{", "")
    return tex

def exampleCodeReplace(sectionText, in_directory, keyString):
    """
    Searches throught the given text and replaces the files of code with
    the actual code in the file. 
    
    All examples in the Latex file follows the following pattern:
        \keyString{<pre-text>}{<directory to code>}{<post-text>}
    -- The <pre-text> and <post-text> should have already been processed 
    prior to this function call and will be concatenated to the return
    string.
    -- The pattern ".nf\\n ... .fi\\n" will have the text in between ".nf"
    and "fi" be presented without formatting. 
    """

    while(sectionText.find(keyString) != -1):
        startOfText = sectionText.find(keyString)
        firstStartBrace = sectionText.find("{", startOfText)
        firstEndBrace = sectionText.find("}", firstStartBrace)
        secStartBrace = sectionText.find("{", firstEndBrace)
        secEndBrace = sectionText.find("}", secStartBrace)
        thirdStartBrace = sectionText.find("{", secEndBrace)
        thirdEndBrace = sectionText.find("}", thirdStartBrace)
        path = sectionText[secStartBrace+1:secEndBrace].strip()
        code = open(in_directory + "/../" + path, "r").read()
        sectionText = sectionText[:startOfText] + "\n" + \
                    sectionText[firstStartBrace+1:firstEndBrace] + "\n\n" + \
                    ".nf\n" + \
                    code + \
                    ".fi\n" + \
                    sectionText[thirdStartBrace+1:thirdEndBrace] + \
                    sectionText[thirdEndBrace + 1:]
    return sectionText

def exampleReplacements(tex, in_directory):
    """
    Search tex file for the keyString "\\begin{apiexamples}" and its matching
    "\\end{apiexamples}". All text between will be processed such that there 
    are no consecutive spaces, no tabs, and unnecessary "\\n". The text will 
    then have all the macros replaced and put back into its corresponding 
    placein the text file. 
    The string "./ sectionStart" is appended at the beginning of the pro-
    cessed text, for differentiation between the text with sections and 
    "dangling text". These strings will not appear in the manpage as any 
    line that begins with a period will be treated as a comment. 
    NOTE: There is no corresponding "./ sectionEnd" in this section because
    the example section of the manpage is always the last section. 
    Dangling text is identified as the text between a "./ sectionEnd" and
    ./sectionStart. Since there will be no text following this section, the
    "./ sectionEnd" flag is no necessary. 
    """

    startOfText = tex.find("\\begin{apiexamples}")
    endOfText = tex.find("\\end{apiexamples}")
    text = tex[startOfText + len("\\begin{apiexamples}"):endOfText]
    sectionText = cleanText(text)

    sectionText = exampleCodeReplace(sectionText, in_directory, 
                    "\\apicexample")
    sectionText = exampleCodeReplace(sectionText, in_directory,
                    "\\apifexample")

    tex = tex[:startOfText] + "\n" \
            "./ sectionStart" + "\n.SS Examples\n" + \
            sectionText + \
            tex[endOfText + len("\\end{apiexamples}"):]
    return tex

def argReplacements(tex):
    """
    All elements within the section defined between "\\begin{apiarguments}"
    and "\end{apiarguments}" will follow the following pattern:
            \\apiargument{<STATUS>}{<name>}{<description>}
    The following code will convert the above text such that STATUS will be
    bolded, <name> will be underlined/italicized, and <description> will be
    regular. 
    In the special case that there are no arguments, the macro will be of 
    the form:
            \\apiargument{"None."}{}{}
    in which case, the code will conver the macro above to have "None." as
    bolded. 
    """

    startOfText = tex.find("\\apiargument")
    endOfText = tex.find("\\end{apiarguments}")
    sectionText = tex[startOfText + len("\\apiargument"):endOfText].strip()
    argList = sectionText.split("\\apiargument")
    cleanArgList = []
    for arg in argList:
        firstStartBrace = arg.find("{")
        firstEndBrace = findMatchingBrace(arg, firstStartBrace)
        secStartBrace = arg.find("{", firstEndBrace)
        secEndBrace = findMatchingBrace(arg, secStartBrace)
        thirdStartBrace = arg.find("{", secEndBrace)
        thirdEndBrace = findMatchingBrace(arg, thirdStartBrace)

        status = cleanText(arg[firstStartBrace+1:firstEndBrace])
        name = cleanText(arg[secStartBrace+1:secEndBrace])
        text = cleanText(arg[thirdStartBrace+1:thirdEndBrace])
        if (arg[firstStartBrace+1:firstEndBrace] == "None."):
            cleanArgList.append(".B None.")
        else:
            cleanArgList.append(".BR \"" + status + \
                        " \" -" + "\n.I " + name + \
                        "\n- " + text + \
                        arg[thirdEndBrace+1:])
    sectionText = "\n\n".join(cleanArgList)
    tex = tex[:startOfText] + "\n" + \
            "./ sectionStart\n" + "\n" + \
            ".SH DESCRIPTION\n.SS Arguments\n" + \
            sectionText + "\n./ sectionEnd\n" + tex[endOfText:]
    tex = tex.replace("\\begin{apiarguments}", "")
    tex = tex.replace("\\end{apiarguments}", "")
    return tex

##########################################################################
##                            TABLE REPLACEMENTS                        ##
##########################################################################

def descTReplacements(tex):
    """
    All elements within the tex file defined to begin with "\\apidesctable{"
    will follow the following pattern:
            \\apidesctable{{<desc>}{<firstColHead>}{<secondColHead>}
    The following code will convert the above text to display such that 
    <desc> will display as regular text, <firstColHead> and <secondColHead>
    will follow the tabular header.
    
    The tabular header is:
                    .TP <number>
                    <firstCol>
                    <secondCol>
    where <number> is the number of spaces between the start of firstCol
    and the start of secondCol. 
    Displays as:
                    <firstCol>                     <secondCol>
                    <----------<number>---------->
    If the length of <firstCol> surpasses <number>, it will display as:
                    <----------<number>---------->
                    <firstCol>                     
                                                 <secondCol>
    
    Every row of the table must include the ".TP" or the text will be
    processed with default formatting. 
    If there there is no <number>, the script will default to 1, or the
    lastest definition of <number> in the current table
    NOTE: Typically manpages ignores "\\n" within the text and displays
    the text as if there is no "\\n". Under the ".TP" header, the flag
    for when the second column begins is the next "\\n" 
    
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of every row processed, respectively, for differenti-
    ating between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    index = tex.find("\\apidesctable{")
    firstStartBrace = tex.find("{", index)
    firstEndBrace = findMatchingBrace(tex, firstStartBrace)
    secStartBrace = tex.find("{", firstEndBrace)
    secEndBrace = findMatchingBrace(tex, secStartBrace)
    thirdStartBrace = tex.find("{", secEndBrace)
    thirdEndBrace = findMatchingBrace(tex, thirdStartBrace)

    desc = cleanText(tex[firstStartBrace + 1: firstEndBrace])
    firstColHead = cleanText(tex[secStartBrace + 1: secEndBrace])
    secondColHead = cleanText(tex[thirdStartBrace + 1: thirdEndBrace])

    tex = tex[:index] + "\n./ sectionStart\n" + desc + \
            "\n.TP 25\n" + firstColHead + "\n" + \
            secondColHead + "\n./ sectionEnd\n" + tex[thirdEndBrace+1:]
    return tex

def tablerowReplacements(tex):
    """
    All elements within the tex file defined to begin with "\\apitablerow"
    will follow the following pattern:
            \\apitablerow{<firstCol>}{<secondCol>}
    The following code will convert the above text to display in tabular 
    form. 
    
    The tabular header is:
                    .TP <number>
                    <firstCol>
                    <secondCol>
    where <number> is the number of spaces between the start of firstCol
    and the start of secondCol. 
    Displays as:
                    <firstCol>                     <secondCol>
                    <----------<number>---------->
    If the length of <firstCol> surpasses <number>, it will display as:
                    <----------<number>---------->
                    <firstCol>                     
                                                 <secondCol>
    
    Every row of the table must include the ".TP" or the text will be
    processed with default formatting. 
    If there there is no <number>, the script will default to 1, or the
    lastest definition of <number> in the current table
    NOTE: Typically manpages ignores "\\n" within the text and displays
    the text as if there is no "\\n". Under the ".TP" header, the flag
    for when the second column begins is the next "\\n" 
    
    In the special case that there is no text in the first column (a 
    method to get the proper indentation in the Latex), continue the 
    text in <secondCol> from the previous row's <secondCol> text will
    have the same behavior from Latex. 
    The strings "./ sectionStart" and "./ sectionEnd" are appending at the
    beginning and end of every row processed, respectively, for differenti-
    ating between the text with sections and "dangling text".
    These strings will not appear in the manpage as any line that begins 
    with a period will be treated as a comment. 
    """

    while(tex.find("\\apitablerow") != -1):
        startOfText = tex.find("\\apitablerow")
        firstStartBrace = tex.find("{", startOfText)
        firstEndBrace = findMatchingBrace(tex, firstStartBrace)
        secStartBrace = tex.find("{", firstEndBrace)
        secEndBrace = findMatchingBrace(tex, secStartBrace)

        firstCol = tex[firstStartBrace + 1:firstEndBrace]
        firstCol = firstCol.replace("\n", " ")
        firstCol = macroReplacements(firstCol)

        secondCol = tex[secStartBrace + 1: secEndBrace]
        secondCol = secondCol.replace("\n", " ")
        secondCol = cleanText(secondCol)

        if (firstCol != ""):
            tex = tex[:startOfText] + "\n" + \
                "./ sectionStart\n.TP 25\n" + firstCol + "\n" + \
                secondCol + "\n./ sectionEnd\n" + tex[secEndBrace+1:]
        else:
            tex = tex[:startOfText] + "\n" + "\n./ sectionStart" + \
                "\n" + secondCol + "\n./ sectionEnd\n" + tex[secEndBrace+1:]
    return tex


##########################################################################
##                    REFERENCES AND REF TABLE REPLACEMENTS             ##
##########################################################################

def refTextReplacements(tex):
    """
    Searches through the tex file and processes "dangling text", or text that
    does not belong to a designated section. It finds these strings by search-
    ing for text in between any two consecutive "./ sectionStart" and 
    "./ sectionEnd" These text will sometimes have references to tables, iden-
    tifiable by the argument in the "\\ref{<argument>}" macro. Each table will
    have a unique integer that it can be identified with, consistent with the
    Latex file.
    The function will return the processed string with an indication array of 
    which tables are to be appended to the end of the man page. 
    """

    tables = [0, 0, 0, 0, 0]
    last = 0
    while(tex.find("./ sectionEnd", last) > 0):
        firstEnd = tex.find("./ sectionEnd", last)
        secStart = tex.find("./ sectionStart", firstEnd)
        last = secStart
        if(tex[firstEnd + len("./ sectionEnd"):secStart].strip() != ""):
            sectionText = tex[firstEnd + len("./ sectionEnd"):secStart].strip()
            sectionText = cleanText(sectionText)

            if (tex.find("\\ref{stdrmatypes}") != -1):
                sectionText = sectionText.replace("\\ref{stdrmatypes}", "1")
                tables[0] = 1
            if (tex.find("\\ref{stdamotypes}") != -1):
                sectionText = sectionText.replace("\\ref{stdamotypes}", "2")
                tables[1] = 1
            if (tex.find("\\ref{extamotypes}") != -1):
                sectionText = sectionText.replace("\\ref{extamotypes}", "3")
                tables[2] = 1
            if (tex.find("\\ref{bitamotypes}") != -1):
                sectionText = sectionText.replace("\\ref{bitamotypes}", "4")
                tables[3] = 1
            if (tex.find("\\ref{p2psynctypes}") != -1):
                sectionText = sectionText.replace("\\ref{p2psynctypes}", "5")
                tables[4] = 1

            sectionText = sectionText.replace("~", " ")
            tex = tex[:firstEnd + len("./ sectionEnd\n")] + "\n\n" + \
                    sectionText + "\n" + tex[secStart:]
    return (tex, tables)

def refMEMReplacements(tex):
    """ 
    Searches through the tex for any reference that specifically points to 
    the memory model and replaces that sentence with a hard-coded string.
    Processing the section that was in the original text is not an option
    because the reference the Latex file does not apply to the man pages. 
    """

    if (tex.find("\\ref{subsec:memory_model}") != -1):
        while(tex.find("\\ref{subsec:memory_model}") != -1):
            index = tex.find("\\ref{subsec:memory_model}")
            enter = 0
            last = 0
            while (enter < index):
                last = enter
                enter = tex.find("\n", enter + 1)
            period = tex.find(".", index)
            sentence = tex[last + 1: period + 1]
            tex = tex[:last + 1] + \
                "Please refer to the subsection on the Memory Model for the" + \
                " definition of the term \"remotely accessible\"." + \
                tex[period + 1:]
    return tex

def parseRefTables(tableTex):
    """ 
    The function takes in a tex file that contains a type table as input
    and returns the processed table in tabular form.
    All elements within the tex file that is desired will follow the 
    following pattern:
    
            <firstCol>    &    <secondCol> \\\\ \hline
    
    The following code will convert the above text to display in tabular 
    form. 
    In the special case of the first row that follows the pattern above,
    the code will bold the strings of <firstCol> and <secondCol> for this
    row only, for these are the column headers 
    
    The tabular header is:
                    .TP <number>
                    <firstCol>
                    <secondCol>
    where <number> is the number of spaces between the start of firstCol
    and the start of secondCol. 
    Displays as:
                    <firstCol>                     <secondCol>
                    <----------<number>---------->
    If the length of <firstCol> surpasses <number>, it will display as:
                    <----------<number>---------->
                    <firstCol>                     
                                                  <secondCol>
    
    Every row of the table must include the ".TP" or the text will be
    processed with default formatting. 
    If there there is no <number>, the script will default to 1, or the
    lastest definition of <number> in the current table
    NOTE: Typically manpages ignores "\\n" within the text and displays
    the text as if there is no "\\n". Under the ".TP" header, the flag
    for when the second column begins is the next "\\n" 
    
    .B at the start of a line will underline/italicize all text until
    the next "\\n" 
    """

    startIdx = tableTex.find("\\hline")
    endIdx = tableTex.find("\\end{tabular}")
    table = cleanText(tableTex[startIdx + len("\\hline"):endIdx])
    text = ""
    count = 0
    typename = []
    while(table.find("\\hline") >= 0):
        line = table[:table.find("\\hline")]
        firstCol = line[:line.find("&")].strip()
        secondCol = line[line.find("&")+1:line.find("\\\\")].strip()
        if (count == 0):
            text += ".TP 25\n" + ".B " + firstCol + "\n" + \
                    ".B " + secondCol + "\n"
        else:
            text += ".TP\n" + firstCol + "\n" + secondCol + "\n"
            typename.append(secondCol)
        table = table[table.find("\\hline") + len("\\hline"):]
        count += 1
    return (text, typename)

def bulletReplacements(tex):
    """ 
    The function takes in a tex file that contains a bulleted list
    as input and returns the processed text to display a list in 
    manpages.
    All elements within the tex file that is desired will follow the 
    following pattern:
    
            \begin{itemize}
            \item ...
            \item ...
            \end{itemize}
    
    The following code will convert the above text to display in tabular 
    form. 
    
    The bullet header is:
                    .IP 
                    \(bu ...
                    \(bu ...
    The .RE flag will have the indentation default to the previous set-
    ting prior to the list.
    
    NOTE: Typically manpages ignores "\\n" within the text and displays
    the text as if there is no "\\n". Under the ".TP" header, the flag
    for when the second column begins is the next "\\n" 
    
    .B at the start of a line will underline/italicize all text until
    the next "\\n" 
    """

    startIdx = tex.find("\\begin{itemize}")
    endIdx = tex.find("\\end{itemize}")
    text = cleanText(tex[startIdx + len("\\begin{itemize}"):endIdx])
    text = text.replace("\\item", "\n\\(bu")
    tex = tex[:startIdx] + "\n.IP\n" + text + "\n.RE" + tex[endIdx + len("\\end{itemize}"):]

    return tex

def addRefTable(tableID, directory):
    """ 
    This function maps the tableID given to each reference table to its
    corresponding to tex file that contains the table. It then calls a
    helper function to parse the table and returns the subsection text, 
    containing the subsection header, the caption for the table, and the
    table. 
    """
    numTable = 0
    if tableID == 1:
        tableTex = open(directory + "/rma_intro.tex", "r").read()
        numTable = 1
    elif tableID == 2: 
        tableTex = open(directory + "/atomics_intro.tex", "r").read()
        numTable = 1
    elif tableID == 3:
        tableTex = open(directory + "/atomics_intro.tex", "r").read()
        numTable = 2
    elif tableID == 4:
        tableTex = open(directory + "/atomics_intro.tex", "r").read()
        numTable = 3
    else:
        tableTex = open(directory + "/p2p_sync_intro.tex", "r").read()
        numTable = 1
    count = 0
    beginTable = 0
    while (count < numTable):
        beginTable = tableTex.find("\\begin{table}", beginTable + 1)
        count = count + 1

    endTable = tableTex.find("\\end{table}", beginTable) + len("\\end{table}")
    tableTex = tableTex[beginTable:endTable]
    captionIdx = tableTex.find("\\TableCaptionRef")
    startBrace = captionIdx + len("\TableCaptionRef")
    endBrace = findMatchingBrace(tableTex, startBrace)

    caption = generalReplacements(tableTex[startBrace+1:endBrace])
    (tex, typenames) = parseRefTables(tableTex)
    text = ".SS Table " + str(tableID) + ":\n" + caption + "\n" + \
            tex
    return (text, typenames)

##########################################################################
##                            CODE REPLACEMENTS                         ##
##########################################################################

def synCReplacements(tex, keyString):
    """ 
    Replaces the code identified between "\\begin{<keyString>}" and 
    "\end{<keyString>}" to have C keywords (defined as a global variable) 
    bolded and the argument variables underlined. Also compiles a list, 
    linkCFuncs, of all functions that falls within the scope of this manpage.
    For instance, shmem_malloc, shmem_free, shmeme_realloc, and shmem_align 
    all fall under the shmem_malloc manpage. In this case, linkCFuncs will
    be ["shmem_malloc", "shmem_free", "shmeme_realloc", "shmem_align"].
    
    .B at the start of a line will bold all text until
    the next "\\n"
    .I at the start of a line will underline/italicize all text until
    the next "\\n"
    .IB will alternate between bold and underline/italicize font. For 
    example:
    
    .IB "A" "B" "C"
    
    will have "A" and "C" as italicized and "B" will be bolded. 
    """

    linkCFuncs = []
    while (tex.find("\\begin{" + keyString + "}") != -1):
        startOfText = tex.find("\\begin{" + keyString + "}")
        endOfText = tex.find("\\end{" + keyString + "}")
        text = tex[startOfText + len("\\begin{" + keyString + "}"):endOfText]
        codeList = text.split(";")
        formattedCode = [];

        for line in codeList:
            line = line.strip()
            line = line.replace("@\\FuncDecl{", "")
            line = line.replace("\\FuncParam{", "")
            line = line.replace("}", "")
            line = line.replace("@", "")
            wordList = line.split()
            for i in xrange(len(wordList)):
                if (wordList[i].find("shmem") != -1 and 
                        wordList[i] not in boldWordsC):
                    stripAsterisk = wordList[i].replace("*", "")
                    argStart = stripAsterisk.find("(")
                    funcName = stripAsterisk[:argStart]
                    linkCFuncs.append(funcName.strip())

                if(i < 2):
                    wordList[i] = "\n.B " + wordList[i]
                else:
                    if (wordList[i] in boldWordsC):
                        wordList[i] = "\n.B " + wordList[i]
                    else:
                        if (wordList[i].find(")") != -1):
                            wordList[i] = "\n.I " + \
                                wordList[i][: wordList[i].find(")")] + \
                                "\n.B );\n"
                        elif (wordList[i].find(",") != -1):
                            wordList[i] = "\n.IB \"" + \
                                wordList[i][: wordList[i].find(",")] \
                                + "\" ,"
                        else:
                            wordList[i] = "\n.I " + wordList[i]
            line = "".join(wordList)
            formattedCode.append(line)

        formattedCode = "\n\n".join(formattedCode)
        if (keyString == "C11synopsis"):
            header = ".SS C11:\n"
        elif (keyString == "Csynopsis"):
            header = ".SS C/C++:\n"
        else:
            header = ""

        tex = tex[:startOfText] + "\n./ sectionStart" + "\n" + \
                header + formattedCode  + "\n" + \
                "./ sectionEnd\n" + \
                tex[endOfText + len("\\end{" + keyString + "}"):]
    return (tex, linkCFuncs)

def synFReplacements(tex, build_fortran):
    """ 
    Replaces the code identified between "\\begin{Fsynopsis}" and 
    "\end{Fsynopsis}" to have Fortran keywords (defined as a global 
    variable) bolded. Also compiles a list, linkFortranFuncs, of all 
    functions that falls within the scope of this manpage. For instance, 
    shmem_malloc, shmem_free, shmeme_realloc, and shmem_align all fall 
    under the shmem_malloc manpage. In this case, linkCFuncs will be 
    ["shmem_malloc", "shmem_free", "shmeme_realloc", "shmem_align"].
    
    .B at the start of a line will bold all text untilthe next "\\n"

    .BR will alternate between bold and regular font. For example:
    
    .BR "A" "B" "C"
    
    will have "A" and "C" as bolded and "B" will be regular. 
    """

    linkFortranFuncs = []
    while (tex.find("\\begin{Fsynopsis}") != -1):
        startOfText = tex.find("\\begin{Fsynopsis}")
        endOfText = tex.find("\\end{Fsynopsis}")
        text = tex[startOfText + len("\\begin{Fsynopsis}\n"):endOfText]
        text = "\n" + text.replace("\n", ";\n")
        text = text.replace("@\\FuncDecl{", "")
        text = text.replace("}", "")
        text = text.replace("@", "")
        last = 0;
        while(text.find(";") != -1):
            s = text.find("\n", last) + len("\n")
            e = text.find(";", s) + len(";")
            line = text[s:e]
            line = line.replace(";", "")
            wordList = line.split()
            if(wordList[0] in boldWordsF):
                line = ".BR \"" + wordList[0] + " \" \"" + \
                        " ".join(wordList[1:]) + "\""
            text = text[:s] + line + text[e:]
            last = s

            if (build_fortran):
                if(wordList[0] == "CALL"):
                    argStart = wordList[1].find("(")
                    if (argStart < 0):
                        funcName = wordList[1]
                    else:
                        funcName = wordList[1][:argStart]
                    linkFortranFuncs.append(funcName.strip())

                if(wordList[0] not in boldWordsF):
                    for i in xrange(len(wordList)):
                        if (wordList[i].find("SHMEM") != -1):
                            argStart = wordList[i].find("(")
                            funcName = wordList[i][:argStart]
                            linkFortranFuncs.append(funcName.strip())

        tex = tex[:startOfText] + "\n./ sectionStart" + \
            "\n\n.B ***************DEPRECATED***************\n" + \
                ".SS Fortran:\n" + "\n.nf\n" + text + "\n.fi" + \
                "\n.B ****************************************\n" + \
                "\n./ sectionEnd"  + "\n" + \
                 tex[endOfText + len("\\end{Fsynopsis}"):]
    return (tex, linkFortranFuncs)

def codeReplace(tex, build_fortran):
    linkFuncsAll = []
    (tex, linkC11) = synCReplacements(tex, "C11synopsis")
    (tex, linkC) = synCReplacements(tex, "Csynopsis")
    (tex, linkCCol) = synCReplacements(tex, "CsynopsisCol")
    (tex, linkFortran) = synFReplacements(tex, build_fortran)
    linkFuncsAll.extend(linkC11)
    linkFuncsAll.extend(linkC)
    linkFuncsAll.extend(linkCCol)
    linkFuncsAll.extend(linkFortran)
    return (tex, linkFuncsAll)

##########################################################################
##                            HELPER FUNCTIONS                          ##
##########################################################################

def cleanText(text):
    """ 
    Gets rid of consecutive spaces and tabs in the given text but maintains
    any "\\n\\n" which is the indicator of a new paragraph. For every para-
    graph, the code will replace all macros and rejoin the paragraphs as 
    the final return string.
    NOTE: Typically manpages ignores "\\n" within the text and displays
    the text as if there is no "\\n". "\\n\\n" will parsed as the beginning
    of a new paragraph. 
    """

    text = text.replace("\t", "")
    text = text.replace("\\newline", "")
    while(text.find("  ") != -1):
        text = text.replace("  ", " ")
    text = text.replace("\n ", "\n")
    paragraphList = text.split("\n\n")

    cleanParagraphList = []
    for paragraph in paragraphList:
        paragraph = macroReplacements(paragraph)
        while (paragraph.find("\n\n") != -1):
            paragraph = paragraph.replace("\n\n", "\n")
        cleanParagraphList.append(paragraph)
    sectionText = "\n\n".join(cleanParagraphList)
    return sectionText

def macroReplacements(paragraph):
    paragraph = variableReplacements(paragraph)
    paragraph = generalReplacements(paragraph)
    paragraph = constReplacements(paragraph)
    paragraph = cTypeReplacements(paragraph)
    paragraph = oprReplacements(paragraph)
    paragraph = funcReplacements(paragraph)
    paragraph = italReplacements(paragraph)
    paragraph = paragraphReplacements(paragraph)
    paragraph = paragraph.replace("\n ", "\n")
    return paragraph

def generateRoutineList(TOC):
    """ 
    Takes the Table of Contents and compiles a list of all function names
    that needs to be parsed to manpages. The code assumes that every function
    entry will be preceded by the macro, "\\subsubsection{\\textbf" 
    """

    count = 0;
    routineList = []
    while(TOC.find("\\subsubsection{\\textbf", count) > 0):
        currIdx = TOC.find("\\subsubsection{\\textbf", count)
        startBrace = TOC.find("\\textbf", currIdx) + len("\\textbf")
        endBrace = TOC.find("}", startBrace)
        func = TOC[startBrace+1:endBrace]
        if (func.find(",") != -1):
            func = func[:func.find(",")]
        name = func.lower().replace("\\", "")
        routineList.append(name)
        count = currIdx + len("\\subsubsection{\\textbf")
    return routineList

def findMatchingBrace(tex, ind):
    """ 
    Takes in the tex string and the index of the open brace to which we 
    need to find the matching brace. 
    Function returns the index of the matching brace 
    """

    count = 0
    if (tex[ind] == "{"):
        count += 1
        ptr = ind + 1
    while(count != 0):
        if(tex[ptr] == "{"):
            count = count + 1
        if(tex[ptr] == "}"):
            count = count - 1
        ptr += 1
    return ptr - 1

##########################################################################
##                        REDIRECTION FUNCTIONS                         ##
##########################################################################

def writeLinkFunctions(functionName, linkFunctions, gen_dir, typenames):
    """
    Takes in a list of all functions that refers to the same man page as
    the one with <functionName>, which is the parent man page. 
    """
    for link in linkFunctions:
        link = link.replace("\\", "")
        if link != functionName and link.find("@") == -1:
            if link.find("TYPENAME") != -1:
                for s in typenames:
                    newlink = link.replace("TYPENAME", s)
                    manFile = open(gen_dir + '/' + newlink + ".3", "w")
                    manFile.write(".so " + functionName + ".3\n")
                    manFile.close()
            elif link.find("SIZE") != -1:
                for s in sizes:
                    newlink = link.replace("SIZE", s)
                    manFile = open(gen_dir + '/' + newlink + ".3", "w")
                    manFile.write(".so " + functionName + ".3\n")
                    manFile.close()
            else:
                manFile = open(gen_dir + '/' + link + ".3", "w")
                manFile.write(".so " + functionName + ".3\n")
                manFile.close()

def parseDeprecationTable(directory, gen_dir, build_fortran):
    """ 
    ***PREREQUISITE: This function only gets calls when the user gives
    a directory to the script. The main routines from the Table of Con-
    tents must have been converted for this function to execute properly.

    Parse the deprecation table in the Appendix of the specification.
    Creates and directs the deprecated functions to the most updated ver-
    sion. 
    If the deprecated function has an entry in the Table of Contents, no
    new page will be created.
    If the deprecated function already has an entry in the output, no new
    page will be created.
    """
    tableTex = open(directory + "/backmatter.tex", "r").read()
    beginTable = tableTex.find("\\begin{longtable}")
    endTable = tableTex.find("\\end{longtable}", beginTable)
    tableRows = tableTex.split("\\hline")
    for row in tableRows:
        columns = row.split("&")
        if (columns[0].find("\\FUNC") != -1):
            argStart = columns[0].find("\\FUNC{") + len("\\FUNC{")
            argEnd = columns[0].find("}", argStart)
            deprFuncName = columns[0][argStart:argEnd].replace("\\", "")
            if "Fortran" in columns[0] and not build_fortran:
                continue
            if not os.path.isfile(gen_dir + "/" + deprFuncName + ".3"):
                manFile = open(gen_dir + '/' + deprFuncName + ".3", "w")
                argStartPar = columns[3].find("\\FUNC{") + len("\\FUNC{")
                argEndPar = columns[3].find("}", argStartPar)
                parentName = columns[3][argStartPar:argEndPar].replace("\\", "")
                if not os.path.isfile(gen_dir + "/" + parentName + ".3"):
                    print("Error: " + deprFuncName + " redirects to "
                        + parentName + ", which has not been generated yet.\n")
                    exit(1)
                manFile.write(".so " + parentName + ".3\n")
                manFile.close()

##########################################################################
##                        EXECUTABLE  FUNCTIONS                         ##
##########################################################################
    
def writeOshcc(gen_dir):
    manFile = open(gen_dir + '/' + "oshcc.1", "w")
    manFile.write(".TH " + "OSHCC" + " 1 " \
                     + "\"OpenSHMEM Library Documentation\"" + "\n")
    manFile.write(".SH NAME\n")
    manFile.write("oshcc - Compiles and links OpenSHMEM programs " + \
        "written in C\n")
    manFile.write(".SH DESCRIPTION\n")
    manFile.write("This command can be used to compile and link Open" + \
        "SHMEM programs written in C.\n")
    manFile.write("It provides the options and any special libraries " + \
        "that are needed to compile and link OpenSHMEM programs.\n\n" + \
        "It is important to use this command, particularly when link" + \
        "ing programs, as it provides the necessary libraries.\n\n")
    manFile.write("usage: oshcc [oshcc_options] [compiler_arguments]\n")
    manFile.write(".SH COMMAND LINE ARGUMENTS\n")
    manFile.write(".TP 8\n")
    manFile.write(".B --help\n")
    manFile.write("- Display this information\n")
    manFile.write(".TP\n")
    manFile.write(".B -showme | -show\n")
    manFile.write("- Print the command that would be run without executing it\n")
    manFile.write(".TP\n")
    manFile.write(".B -E | -M\n")
    manFile.write("- Disable compilation and linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -S\n")
    manFile.write("- Disable linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -c\n")
    manFile.write("- Compile and assemble. Disable link\n")
    manFile.close()

def writeOshCC(gen_dir):
    manFile = open(gen_dir + '/' + "oshc++.1", "w")
    manFile.write(".TH " + "OSHC++" + " 1 " \
                     + "\"OpenSHMEM Library Documentation\"" + "\n")
    manFile.write(".SH NAME\n")
    manFile.write("oshc++ - Compiles and links OpenSHMEM programs " + \
        "written in C++\n")
    manFile.write(".SH DESCRIPTION\n")
    manFile.write("This command can be used to compile and link Open" + \
        "SHMEM programs written in C++.\n")
    manFile.write("It provides the options and any special libraries " + \
        "that are needed to compile and link OpenSHMEM programs.\n\n" + \
        "It is important to use this command, particularly when link" + \
        "ing programs, as it provides the necessary libraries.\n\n")
    manFile.write("usage: oshc++ [oshc++_options] [compiler_arguments]\n")
    manFile.write(".SH COMMAND LINE ARGUMENTS\n")
    manFile.write(".TP 8\n")
    manFile.write(".B --help\n")
    manFile.write("- Display this information\n")
    manFile.write(".TP\n")
    manFile.write(".B -showme | -show\n")
    manFile.write("- Print the command that would be run without executing it\n")
    manFile.write(".TP\n")
    manFile.write(".B -E | -M\n")
    manFile.write("- Disable compilation and linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -S\n")
    manFile.write("- Disable linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -c\n")
    manFile.write("- Compile and assemble. Disable link\n")
    manFile.close()

def writeOshfort(gen_dir):
    manFile = open(gen_dir + '/' + "oshfort.1", "w")
    manFile.write(".TH " + "OSHFORT" + " 1 " \
                     + "\"OpenSHMEM Library Documentation\"" + "\n")
    manFile.write(".SH NAME\n")
    manFile.write("oshfort - Compiles and links OpenSHMEM programs " + \
        "written in Fortran\n")
    manFile.write(".SH DESCRIPTION\n")
    manFile.write("This command can be used to compile and link Open" + \
        "SHMEM programs written in Fortran.\n")
    manFile.write("It provides the options and any special libraries " + \
        "that are needed to compile and link OpenSHMEM programs.\n\n" + \
        "It is important to use this command, particularly when link" + \
        "ing programs, as it provides the necessary libraries.\n\n")
    manFile.write("usage: oshfort [oshfort_options] [compiler_arguments]\n")
    manFile.write(".SH COMMAND LINE ARGUMENTS\n")
    manFile.write(".TP 8\n")
    manFile.write(".B --help\n")
    manFile.write("- Display this information\n")
    manFile.write(".TP\n")
    manFile.write(".B -showme | -show\n")
    manFile.write("- Print the command that would be run without executing it\n")
    manFile.write(".TP\n")
    manFile.write(".B -E | -M\n")
    manFile.write("- Disable compilation and linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -S\n")
    manFile.write("- Disable linking\n")
    manFile.write(".TP\n")
    manFile.write(".B -c\n")
    manFile.write("- Compile and assemble. Disable link\n")
    manFile.close()

def writeOshrun(gen_dir):
    manFile = open(gen_dir + '/' + "oshrun.1", "w")
    manFile.write(".TH " + "OSHRUN" + " 1 " \
                     + "\"OpenSHMEM Library Documentation\"" + "\n")
    manFile.write(".SH NAME\n")
    manFile.write("oshrun - Run a OpenSHMEM program\n")
    manFile.write(".SH DESCRIPTION\n")
    manFile.write(".B oshrun\n")
    manFile.write("launches the OpenSHMEM programs with a given number " + \
        "of processes.\n\n")
    manFile.write("usage: oshrun [oshrun_options] [launcher_arguments]\n")
    manFile.write(".SH COMMAND LINE ARGUMENTS\n")
    manFile.write(".TP 8\n")
    manFile.write(".B --help | -h\n")
    manFile.write("- Display this information\n")
    manFile.close()

def writeExecFunctions(gen_dir):
    writeOshfort(gen_dir)
    writeOshcc(gen_dir)
    writeOshCC(gen_dir)
    writeOshrun(gen_dir)

##########################################################################
##                            MAIN FUNCTIONS                            ##
##########################################################################

def convertFile(functionName, directory, gen_dir, build_fortran=False):
    filename = directory + '/' + functionName + ".tex"
    texFile = open(filename, "r")
    if not os.path.exists(gen_dir):
        os.makedirs(gen_dir)
    manFile = open(gen_dir + '/' + functionName + ".3", "w")
    titleHeader = writePageHeader(functionName)
    manFile.write(titleHeader)

    tex = texFile.read()
    
    tex = tex.replace("\\begin{apidefinition}\n", 
                "./ sectionStart\n.SH   SYNOPSIS\n./ sectionEnd")
    tex = sumReplacements(tex, functionName)
    tex = mBoxReplacements(tex)
    tex = argReplacements(tex)
    tex = descrReplacements(tex)
    tex = notesReplacements(tex)
    tex = retReplacements(tex)
    tex = boldReplacements(tex)
    (tex, linkFunctions) = codeReplace(tex, build_fortran)
    if (tex.find("\\apiimpnotes{") != -1):
        tex = impnotesReplacements(tex)
    while(tex.find("\\apidesctable{") != -1):
        tex = descTReplacements(tex)
    while(tex.find("\\begin{itemize}") != -1):
        tex = bulletReplacements(tex)
    tex = tablerowReplacements(tex)
    tex = refMEMReplacements(tex)
    if (tex.find("\\begin{apiexamples}") != -1):
        tex = exampleReplacements(tex, directory)
    (tex, tables) = refTextReplacements(tex)
    typenames = []
    if (tables[0]):
        (t, typenames) = addRefTable(1, directory)
        tex = tex + t
    if (tables[1]):
        (t, typenames) = addRefTable(2, directory)
        tex = tex + t
    if (tables[2]):
        (t, typenames) = addRefTable(3, directory)
        tex = tex + t
    if (tables[3]):
        (t, typenames) = addRefTable(4, directory)
        tex = tex + t
    if (tables[4]):
        (t, typenames) = addRefTable(5, directory)
        tex = tex + t
    tex = tex.replace("\\begin{apidefinition}", "")
    tex = tex.replace("\\end{apidefinition}", "")
    text = tex[:tex.find(".SS Examples")]
    while(text.find("\n ") != -1):
        text = text.replace("\n ", "\n")
    text = text.replace("\\\\", "\n")
    text = text.replace("$", "")
    tex = text + tex[tex.find(".SS Examples"):]
    tex = tex.replace("\n ", "\n")
    tex = tex.replace("\\begin{DeprecateBlock}", 
        "\n./ sectionStart\n.B ***************DEPRECATED***************\n./ sectionEnd\n")
    tex = tex.replace("\\end{DeprecateBlock}", 
        "\n./ sectionStart\n.B ****************************************\n./ sectionEnd\n")
    tex = tex.replace("\\deprecationstart", 
        "\n./ sectionStart\n.B ***************DEPRECATED***************\n./ sectionEnd\n")
    tex = tex.replace("\\deprecationend", 
        "\n./ sectionStart\n.RE\n.B ****************************************\n./ sectionEnd\n")
    manFile.write(tex.replace("\\n", "\\\\" + "n"))
    print("Finished converting " + functionName + ".tex")
    manFile.close()
    texFile.close()
    writeLinkFunctions(functionName, linkFunctions, gen_dir, typenames)
    return manFile

# Quick fix for the extra newline problem seen in vimpager:
def cleanManFile(man_file):
    """
    Remove all double newlines from manpage file.
    """
    subprocess.call("tr -s '\n' < " + man_file.name + " > .tmpfile", shell=True)
    subprocess.call("mv .tmpfile " + man_file.name, shell=True)

def main():
    parser = argparse.ArgumentParser(description='Generate OpenSHMEM manpages')
    parser.add_argument('-d', '--directory', type=str, required=False,
                        help='OpenSHMEM specification content LaTeX directory path')
    parser.add_argument('-o', '--output-directory', type=str, required=False, default="./man",
                        help='Output directory for generated manpages')
    parser.add_argument('-f', '--filename', type=str, required=False,
                        help='LaTeX file path for an OpenSHMEM routine')
    parser.add_argument('--build-fortran', required=False, default=False, action="store_true",
                        help='build Fortran manpages (requires setting -d)')
    args = parser.parse_args()


    if (args.directory == None) and (args.filename == None):
        parser.print_usage()
        exit(1)

    if platform.system() == 'Darwin' and args.build_fortran:
        print("Warning: OSX may have a case-insensitive filesystem, which " +
              "would break some Fortran manpages.")

    if args.directory != None:
        if not os.path.realpath(args.directory):
            print("Error: input directory, " + args.directory + ", does not appear to exist")
            exit(1)
        else:
            print("Dir found")
            
            if not os.path.isfile(args.directory + "/../main_spec.tex"):
                print("Error: main_spec.tex is not found in the parent of the given directory.")
                exit(1)

            TOC = open(args.directory + "/../main_spec.tex").read()
            routineList = generateRoutineList(TOC)
            for routine in routineList:
                if routine + ".tex" not in os.listdir(args.directory):
                    print("Error: " + routine + ".tex is not in the given directory")
                    exit(1)
                else:
                    man_file = convertFile(routine, args.directory, args.output_directory, args.build_fortran)
                    # cleanManFile(man_file)

            parseDeprecationTable(args.directory, args.output_directory, args.build_fortran)
            writeExecFunctions(args.output_directory)

    else:
        if not os.path.isfile(args.filename):
            print("Error: input file, " + args.filename + ", does not appear to exist.")
            exit(1)
        else:
            per = args.filename.find(".tex")
            last = 0;
            while (args.filename.find("/", last) >= 0):
                last = args.filename.find("/", last) + 1

            functionName = args.filename[last:per]
            man_file = convertFile(functionName, args.filename[:last], args.output_directory)
            # cleanManFile(man_file)

if __name__== "__main__":
    main()
