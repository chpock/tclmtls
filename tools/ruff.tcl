# repeat.tcl --
#
#	Emulation of string repeat for older
#	revisions of Tcl.
#
# Copyright (c) 2000      by Ajuba Solutions.
# Copyright (c) 2001-2006 by Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: repeat.tcl,v 1.1 2006/04/21 04:42:28 andreas_kupries Exp $

# ### ### ### ######### ######### #########
## Requirements

package require Tcl 8.2

namespace eval ::textutil::repeat {}

# ### ### ### ######### ######### #########

namespace eval ::textutil::repeat {
    variable HaveBuiltin [expr {![catch {string repeat a 1}]}]
}

if {0} {
    # Problems with the deactivated code:
    # - Linear in 'num'.
    # - Tests for 'string repeat' in every call!
    #   (Ok, just the variable, still a test every call)
    # - Fails for 'num == 0' because of undefined 'str'.

    proc textutil::repeat::StrRepeat { char num } {
	variable HaveBuiltin
	if { $HaveBuiltin == 0 } then {
	    for { set i 0 } { $i < $num } { incr i } {
		append str $char
	    }
	} else {
	    set str [ string repeat $char $num ]
	}
	return $str
    }
}

if {$::textutil::repeat::HaveBuiltin} {
    proc ::textutil::repeat::strRepeat {char num} {
	return [string repeat $char $num]
    }

    proc ::textutil::repeat::blank {n} {
	return [string repeat " " $n]
    }
} else {
    proc ::textutil::repeat::strRepeat {char num} {
	if {$num <= 0} {
	    # No replication required
	    return ""
	} elseif {$num == 1} {
	    # Quick exit for recursion
	    return $char
	} elseif {$num == 2} {
	    # Another quick exit for recursion
	    return $char$char
	} elseif {0 == ($num % 2)} {
	    # Halving the problem results in O (log n) complexity.
	    set result [strRepeat $char [expr {$num / 2}]]
	    return "$result$result"
	} else {
	    # Uneven length, reduce problem by one
	    return "$char[strRepeat $char [incr num -1]]"
	}
    }

    proc ::textutil::repeat::blank {n} {
	return [strRepeat " " $n]
    }
}

# ### ### ### ######### ######### #########
## Data structures

namespace eval ::textutil::repeat {
    namespace export strRepeat blank
}

# ### ### ### ######### ######### #########
## Ready

package provide textutil::repeat 0.7

# string.tcl --
#
#	Utilities for manipulating strings, words, single lines,
#	paragraphs, ...
#
# Copyright (c) 2000      by Ajuba Solutions.
# Copyright (c) 2000      by Eric Melski <ericm@ajubasolutions.com>
# Copyright (c) 2002      by Joe English <jenglish@users.sourceforge.net>
# Copyright (c) 2001-2014 by Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
# RCS: @(#) $Id: string.tcl,v 1.2 2008/03/22 16:03:11 mic42 Exp $

# ### ### ### ######### ######### #########
## Requirements

package require Tcl 8.2

namespace eval ::textutil::string {}

# ### ### ### ######### ######### #########
## API implementation

# @c Removes the last character from the given <a string>.
#
# @a string: The string to manipulate.
#
# @r The <a string> without its last character.
#
# @i chopping

proc ::textutil::string::chop {string} {
    return [string range $string 0 [expr {[string length $string]-2}]]
}

# @c Removes the first character from the given <a string>.
# @c Convenience procedure.
#
# @a string: string to manipulate.
#
# @r The <a string> without its first character.
#
# @i tail

proc ::textutil::string::tail {string} {
    return [string range $string 1 end]
}

# @c Capitalizes first character of the given <a string>.
# @c Complementary procedure to <p ::textutil::uncap>.
#
# @a string: string to manipulate.
#
# @r The <a string> with its first character capitalized.
#
# @i capitalize

proc ::textutil::string::cap {string} {
    return [string toupper [string index $string 0]][string range $string 1 end]
}

# @c unCapitalizes first character of the given <a string>.
# @c Complementary procedure to <p ::textutil::cap>.
#
# @a string: string to manipulate.
#
# @r The <a string> with its first character uncapitalized.
#
# @i uncapitalize

proc ::textutil::string::uncap {string} {
    return [string tolower [string index $string 0]][string range $string 1 end]
}

# @c Capitalizes first character of each word of the given <a sentence>.
#
# @a sentence: string to manipulate.
#
# @r The <a sentence> with the first character of each word capitalized.
#
# @i capitalize

proc ::textutil::string::capEachWord {sentence} {
    regsub -all {\S+} [string map {\\ \\\\ \$ \\$} $sentence] {[string toupper [string index & 0]][string range & 1 end]} cmd
    return [subst -nobackslashes -novariables $cmd]
}

# Compute the longest string which is common to all strings given to
# the command, and at the beginning of said strings, i.e. a prefix. If
# only one argument is specified it is treated as a list of the
# strings to look at. If more than one argument is specified these
# arguments are the strings to be looked at. If only one string is
# given, in either form, the string is returned, as it is its own
# longest common prefix.

proc ::textutil::string::longestCommonPrefix {args} {
    return [longestCommonPrefixList $args]
}

proc ::textutil::string::longestCommonPrefixList {list} {
    if {[llength $list] <= 1} {
	return [lindex $list 0]
    }

    set list [lsort $list]
    set min [lindex $list 0]
    set max [lindex $list end]

    # Min and max are the two strings which are most different. If
    # they have a common prefix, it will also be the common prefix for
    # all of them.

    # Fast bailouts for common cases.

    set n [string length $min]
    if {$n == 0} {return ""}
    if {0 == [string compare $min $max]} {return $min}

    set prefix ""
    set i 0
    while {[string index $min $i] == [string index $max $i]} {
	append prefix [string index $min $i]
	if {[incr i] > $n} {break}
    }
    set prefix
}

# ### ### ### ######### ######### #########
## Data structures

namespace eval ::textutil::string {
    # Export the imported commands

    namespace export chop tail cap uncap capEachWord
    namespace export longestCommonPrefix
    namespace export longestCommonPrefixList
}

# ### ### ### ######### ######### #########
## Ready

package provide textutil::string 0.8

# trim.tcl --
#
#	Various ways of trimming a string.
#
# Copyright (c) 2000      by Ajuba Solutions.
# Copyright (c) 2000      by Eric Melski <ericm@ajubasolutions.com>
# Copyright (c) 2002-2004 by Johannes-Heinrich Vogeler <vogeler@users.sourceforge.net>
# Copyright (c) 2001-2006 by Andreas Kupries <andreas_kupries@users.sourceforge.net>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: adjust.tcl,v 1.16 2011/12/13 18:12:56 andreas_kupries Exp $

# ### ### ### ######### ######### #########
## Requirements

package require Tcl 8.2
package require textutil::repeat
package require textutil::string

namespace eval ::textutil::adjust {}

# ### ### ### ######### ######### #########
## API implementation

namespace eval ::textutil::adjust {
    namespace import -force ::textutil::repeat::strRepeat
}

proc ::textutil::adjust::adjust {text args} {
    if {[string length [string trim $text]] == 0} {
        return ""
    }

    Configure $args
    Adjust text newtext

    return $newtext
}

proc ::textutil::adjust::Configure {args} {
    variable Justify      left
    variable Length       72
    variable FullLine     0
    variable StrictLength 0
    variable Hyphenate    0
    variable HyphPatterns    ; # hyphenation patterns (TeX)

    set args [ lindex $args 0 ]
    foreach { option value } $args {
	switch -exact -- $option {
	    -full {
		if { ![ string is boolean -strict $value ] } then {
		    error "expected boolean but got \"$value\""
		}
		set FullLine [ string is true $value ]
	    }
	    -hyphenate {
		# the word exceeding the length of line is tried to be
		# hyphenated; if a word cannot be hyphenated to fit into
		# the line processing stops! The length of the line should
		# be set to a reasonable value!

		if { ![ string is boolean -strict $value ] } then {
		    error "expected boolean but got \"$value\""
		}
		set Hyphenate [string is true $value]
		if { $Hyphenate && ![info exists HyphPatterns(_LOADED_)]} {
		    error "hyphenation patterns not loaded!"
		}
	    }
	    -justify {
		set lovalue [ string tolower $value ]
		switch -exact -- $lovalue {
		    left -
		    right -
		    center -
		    plain {
			set Justify $lovalue
		    }
		    default {
			error "bad value \"$value\": should be center, left, plain or right"
		    }
		}
	    }
	    -length {
		if { ![ string is integer $value ] } then {
		    error "expected positive integer but got \"$value\""
		}
		if { $value < 1 } then {
		    error "expected positive integer but got \"$value\""
		}
		set Length $value
	    }
	    -strictlength {
		# the word exceeding the length of line is moved to the
		# next line without hyphenation; words longer than given
		# line length are cut into smaller pieces

		if { ![ string is boolean -strict $value ] } then {
		    error "expected boolean but got \"$value\""
		}
		set StrictLength [ string is true $value ]
	    }
	    default {
		error "bad option \"$option\": must be -full, -hyphenate, \
			-justify, -length, or -strictlength"
	    }
	}
    }

    return ""
}

# ::textutil::adjust::Adjust
#
# History:
#      rewritten on 2004-04-13 for bugfix tcllib-bugs-882402 (jhv)

proc ::textutil::adjust::Adjust { varOrigName varNewName } {
    variable Length
    variable FullLine
    variable StrictLength
    variable Hyphenate

    upvar $varOrigName orig
    upvar $varNewName  text

    set pos 0;                                   # Cursor after writing
    set line ""
    set text ""


    if {!$FullLine} {
	regsub -all -- "(\n)|(\t)"     $orig   " "  orig
	regsub -all -- " +"            $orig  " "   orig
	regsub -all -- "(^ *)|( *\$)"  $orig  ""    orig
    }

    set words [split $orig]
    set numWords [llength $words]
    set numline 0

    for {set cnt 0} {$cnt < $numWords} {incr cnt} {

	set w [lindex $words $cnt]
	set wLen [string length $w]

	# the word $w doesn't fit into the present line
	# case #1: we try to hyphenate

	if {$Hyphenate && ($pos+$wLen >= $Length)} {
	    # Hyphenation instructions
	    set w2 [textutil::adjust::Hyphenation $w]

	    set iMax [llength $w2]
	    if {$iMax == 1 && [string length $w] > $Length} {
		# word cannot be hyphenated and exceeds linesize

		error "Word \"$w2\" can\'t be hyphenated\
			and exceeds linesize $Length!"
	    } else {
		# hyphenating of $w was successfull, but we have to look
		# that every sylable would fit into the line

		foreach x $w2 {
		    if {[string length $x] >= $Length} {
			error "Word \"$w\" can\'t be hyphenated\
				to fit into linesize $Length!"
		    }
		}
	    }

	    for {set i 0; set w3 ""} {$i < $iMax} {incr i} {
		set syl [lindex $w2 $i]
		if {($pos+[string length " $w3$syl-"]) > $Length} {break}
		append w3 $syl
	    }
	    for {set w4 ""} {$i < $iMax} {incr i} {
		set syl [lindex $w2 $i]
		append w4 $syl
	    }

	    if {[string length $w3] && [string length $w4]} {
		# hyphenation was successfull: redefine
		# list of words w => {"$w3-" "$w4"}

		set x [lreplace $words $cnt $cnt "$w4"]
		set words [linsert $x $cnt "$w3-"]
		set w [lindex $words $cnt]
		set wLen [string length $w]
		incr numWords
	    }
	}

	# the word $w doesn't fit into the present line
	# case #2: we try to cut the word into pieces

	if {$StrictLength && ([string length $w] > $Length)} {
	    # cut word into two pieces
	    set w2 $w

	    set over [expr {$pos+2+$wLen-$Length}]

	    incr Length -1
	    set w3   [string range $w2 0 $Length]
	    incr Length
	    set w4   [string range $w2 $Length end]

	    set x [lreplace $words $cnt $cnt $w4]
	    set words [linsert $x $cnt $w3 ]
	    set w [lindex $words $cnt]
	    set wLen [string length $w]
	    incr numWords
	}

	# continuing with the normal procedure

	if {($pos+$wLen < $Length)} {
	    # append word to current line

	    if {$pos} {append line " "; incr pos}
	    append line $w
	    incr pos $wLen
	} else {
	    # line full => write buffer and  begin a new line

	    if {[string length $text]} {append text "\n"}
	    append text [Justification $line [incr numline]]
	    set line $w
	    set pos $wLen
	}
    }

    # write buffer and return!

    if {[string length $text]} {append text "\n"}
    append text [Justification $line end]
    return $text
}

# ::textutil::adjust::Justification
#
# justify a given line
#
# Parameters:
#      line    text for justification
#      index   index for line in text
#
# Returns:
#      the justified line
#
# Remarks:
#      Only lines with size not exceeding the max. linesize provided
#      for text formatting are justified!!!

proc ::textutil::adjust::Justification { line index } {
    variable Justify
    variable Length
    variable FullLine

    set len [string length $line];               # length of current line

    if { $Length <= $len } then {
	# the length of current line ($len) is equal as or greater than
	# the value provided for text formatting ($Length) => to avoid
	# inifinite loops we leave $line unchanged and return!

	return $line
    }

    # Special case:
    # for the last line, and if the justification is set to 'plain'
    # the real justification is 'left' if the length of the line
    # is less than 90% (rounded) of the max length allowed. This is
    # to avoid expansion of this line when it is too small: without
    # it, the added spaces will 'unbeautify' the result.
    #

    set justify $Justify
    if { ( "$index" == "end" ) && \
	    ( "$Justify" == "plain" ) && \
	    ( $len < round($Length * 0.90) ) } then {
	set justify left
    }

    # For a left justification, nothing to do, but to
    # add some spaces at the end of the line if requested

    if { "$justify" == "left" } then {
	set jus ""
	if { $FullLine } then {
	    set jus [strRepeat " " [ expr { $Length - $len } ]]
	}
	return "${line}${jus}"
    }

    # For a right justification, just add enough spaces
    # at the beginning of the line

    if { "$justify" == "right" } then {
	set jus [strRepeat " " [ expr { $Length - $len } ]]
	return "${jus}${line}"
    }

    # For a center justification, add half of the needed spaces
    # at the beginning of the line, and the rest at the end
    # only if needed.

    if { "$justify" == "center" } then {
	set mr [ expr { ( $Length - $len ) / 2 } ]
	set ml [ expr { $Length - $len - $mr } ]
	set jusl [strRepeat " " $ml]
	set jusr [strRepeat " " $mr]
	if { $FullLine } then {
	    return "${jusl}${line}${jusr}"
	} else {
	    return "${jusl}${line}"
	}
    }

    # For a plain justification, it's a little bit complex:
    #
    # if some spaces are missing, then
    #
    # 1) sort the list of words in the current line by decreasing size
    # 2) foreach word, add one space before it, except if it's the
    #    first word, until enough spaces are added
    # 3) rebuild the line

    if { "$justify" == "plain" } then {
	set miss [ expr { $Length - [ string length $line ] } ]

	# Bugfix tcllib-bugs-860753 (jhv)

	set words [split $line]
	set numWords [llength $words]

	if {$numWords < 2} {
	    # current line consists of less than two words - we can't
	    # insert blanks to achieve a plain justification => leave
	    # $line unchanged and return!

	    return $line
	}

	for {set i 0; set totalLen 0} {$i < $numWords} {incr i} {
	    set w($i) [lindex $words $i]
	    if {$i > 0} {set w($i) " $w($i)"}
	    set wLen($i) [string length $w($i)]
	    set totalLen [expr {$totalLen+$wLen($i)}]
	}

	set miss [expr {$Length - $totalLen}]

	# len walks through all lengths of words of the line under
	# consideration

	for {set len 1} {$miss > 0} {incr len} {
	    for {set i 1} {($i < $numWords) && ($miss > 0)} {incr i} {
		if {$wLen($i) == $len} {
		    set w($i) " $w($i)"
		    incr wLen($i)
		    incr miss -1
		}
	    }
	}

	set line ""
	for {set i 0} {$i < $numWords} {incr i} {
	    set line "$line$w($i)"
	}

	# End of bugfix

	return "${line}"
    }

    error "Illegal justification key \"$justify\""
}

proc ::textutil::adjust::SortList { list dir index } {

    if { [ catch { lsort -integer -$dir -index $index $list } sl ] != 0 } then {
        error "$sl"
    }

    return $sl
}

# Hyphenation utilities based on Knuth's algorithm
#
# Copyright (C) 2001-2003 by Dr.Johannes-Heinrich Vogeler (jhv)
# These procedures may be used as part of the tcllib

# textutil::adjust::Hyphenation
#
#      Hyphenate a string using Knuth's algorithm
#
# Parameters:
#      str     string to be hyphenated
#
# Returns:
#      the hyphenated string

proc ::textutil::adjust::Hyphenation { str } {

    # if there are manual set hyphenation marks e.g. "Recht\-schrei\-bung"
    # use these for hyphenation and return

    if {[regexp {[^\\-]*[\\-][.]*} $str]} {
	regsub -all {(\\)(-)} $str {-} tmp
	return [split $tmp -]
    }

    # Don't hyphenate very short words! Minimum length for hyphenation
    # is set to 3 characters!

    if { [string length $str] < 4 } then { return $str }

    # otherwise follow Knuth's algorithm

    variable HyphPatterns;                       # hyphenation patterns (TeX)

    set w ".[string tolower $str].";             # transform to lower case
    set wLen [string length $w];                 # and add delimiters

    # Initialize hyphenation weights

    set s {}
    for {set i 0} {$i < $wLen} {incr i} {
	lappend s 0
    }

    for {set i 0} {$i < $wLen} {incr i} {
	set kmax [expr {$wLen-$i}]
	for {set k 1} {$k < $kmax} {incr k} {
	    set sw [string range $w $i [expr {$i+$k}]]
	    if {[info exists HyphPatterns($sw)]} {
		set hw $HyphPatterns($sw)
		set hwLen [string length $hw]
		for {set l1 0; set l2 0} {$l1 < $hwLen} {incr l1} {
		    set c [string index $hw $l1]
		    if {[string is digit $c]} {
			set sPos [expr {$i+$l2}]
			if {$c > [lindex $s $sPos]} {
			    set s [lreplace $s $sPos $sPos $c]
			}
		    } else {
			incr l2
		    }
		}
	    }
	}
    }

    # Replace all even hyphenation weigths by zero

    for {set i 0} {$i < [llength $s]} {incr i} {
	set c [lindex $s $i]
	if {!($c%2)} { set s [lreplace $s $i $i 0] }
    }

    # Don't start with a hyphen! Take also care of words enclosed in quotes
    # or that someone has forgotten to put a blank between a punctuation
    # character and the following word etc.

    for {set i 1} {$i < ($wLen-1)} {incr i} {
	set c [string range $w $i end]
	if {[regexp {^[:alpha:][.]*} $c]} {
	    for {set k 1} {$k < ($i+1)} {incr k} {
		set s [lreplace $s $k $k 0]
	    }
	    break
	}
    }

    # Don't separate the last character of a word with a hyphen

    set max [expr {[llength $s]-2}]
    if {$max} {set s [lreplace $s $max end 0]}

    # return the syllabels of the hyphenated word as a list!

    set ret ""
    set w ".$str."
    for {set i 1} {$i < ($wLen-1)} {incr i} {
	if {[lindex $s $i]} { append ret - }
	append ret [string index $w $i]
    }
    return [split $ret -]
}

# textutil::adjust::listPredefined
#
#      Return the names of the hyphenation files coming with the package.
#
# Parameters:
#      None.
#
# Result:
#       List of filenames (without directory)

proc ::textutil::adjust::listPredefined {} {
    variable here
    return [glob -type f -directory $here -tails *.tex]
}

# textutil::adjust::getPredefined
#
#      Retrieve the full path for a predefined hyphenation file
#       coming with the package.
#
# Parameters:
#      name     Name of the predefined file.
#
# Results:
#       Full path to the file, or an error if it doesn't
#       exist or is matching the pattern *.tex.

proc ::textutil::adjust::getPredefined {name} {
    variable here

    if {![string match *.tex $name]} {
        return -code error \
                "Illegal hyphenation file \"$name\""
    }
    set path [file join $here $name]
    if {![file exists $path]} {
        return -code error \
                "Unknown hyphenation file \"$path\""
    }
    return $path
}

# textutil::adjust::readPatterns
#
#      Read hyphenation patterns from a file and store them in an array
#
# Parameters:
#      filNam  name of the file containing the patterns

proc ::textutil::adjust::readPatterns { filNam } {

    variable HyphPatterns;                       # hyphenation patterns (TeX)

    # HyphPatterns(_LOADED_) is used as flag for having loaded
    # hyphenation patterns from the respective file (TeX format)

    if {[info exists HyphPatterns(_LOADED_)]} {
	unset HyphPatterns(_LOADED_)
    }

    # the array xlat provides translation from TeX encoded characters
    # to those of the ISO-8859-1 character set

    set xlat(\"s) \337;  # 223 := sharp s    "
    set xlat(\`a) \340;  # 224 := a, grave
    set xlat(\'a) \341;  # 225 := a, acute
    set xlat(\^a) \342;  # 226 := a, circumflex
    set xlat(\"a) \344;  # 228 := a, diaeresis "
    set xlat(\`e) \350;  # 232 := e, grave
    set xlat(\'e) \351;  # 233 := e, acute
    set xlat(\^e) \352;  # 234 := e, circumflex
    set xlat(\`i) \354;  # 236 := i, grave
    set xlat(\'i) \355;  # 237 := i, acute
    set xlat(\^i) \356;  # 238 := i, circumflex
    set xlat(\~n) \361;  # 241 := n, tilde
    set xlat(\`o) \362;  # 242 := o, grave
    set xlat(\'o) \363;  # 243 := o, acute
    set xlat(\^o) \364;  # 244 := o, circumflex
    set xlat(\"o) \366;  # 246 := o, diaeresis "
    set xlat(\`u) \371;  # 249 := u, grave
    set xlat(\'u) \372;  # 250 := u, acute
    set xlat(\^u) \373;  # 251 := u, circumflex
    set xlat(\"u) \374;  # 252 := u, diaeresis "

    set fd [open $filNam RDONLY]
    set status 0

    while {[gets $fd line] >= 0} {

	switch -exact $status {
	    PATTERNS {
		if {[regexp {^\}[.]*} $line]} {
		    # End of patterns encountered: set status
		    # and ignore that line
		    set status 0
		    continue
		} else {
		    # This seems to be pattern definition line; to process it
		    # we have first to do some editing
		    #
		    # 1) eat comments in a pattern definition line
		    # 2) eat braces and coded linefeeds

		    set z [string first "%" $line]
		    if {$z > 0} { set line [string range $line 0 [expr {$z-1}]] }

		    regsub -all {(\\n|\{|\})} $line {} tmp
		    set line $tmp

		    # Now $line should consist only of hyphenation patterns
		    # separated by white space

		    # Translate TeX encoded characters to ISO-8859-1 characters
		    # using the array xlat defined above

		    foreach x [array names xlat] {
			regsub -all {$x} $line $xlat($x) tmp
			set line $tmp
		    }

		    # split the line and create a lookup array for
		    # the repective hyphenation patterns

		    foreach item [split $line] {
			if {[string length $item]} {
			    if {![string match {\\} $item]} {
				# create index for hyphenation patterns

				set var $item
				regsub -all {[0-9]} $var {} idx
				# store hyphenation patterns as elements of an array

				set HyphPatterns($idx) $item
			    }
			}
		    }
		}
	    }
	    EXCEPTIONS {
		if {[regexp {^\}[.]*} $line]} {
		    # End of patterns encountered: set status
		    # and ignore that line
		    set status 0
		    continue
		} else {
		    # to be done in the future
		}
	    }
	    default {
		if {[regexp {^\\endinput[.]*} $line]} {
		    # end of data encountered, stop processing and
		    # ignore all the following text ..
		    break
		} elseif {[regexp {^\\patterns[.]*} $line]} {
		    # begin of patterns encountered: set status
		    # and ignore that line
		    set status PATTERNS
		    continue
		} elseif {[regexp {^\\hyphenation[.]*} $line]} {
		    # some particular cases to be treated separately
		    set status EXCEPTIONS
		    continue
		} else {
		    set status 0
		}
	    }
	}
    }

    close $fd
    set HyphPatterns(_LOADED_) 1

    return
}

#######################################################

# @c The specified <a text>block is indented
# @c by <a prefix>ing each line. The first
# @c <a hang> lines ares skipped.
#
# @a text:   The paragraph to indent.
# @a prefix: The string to use as prefix for each line
# @a prefix: of <a text> with.
# @a skip:   The number of lines at the beginning to leave untouched.
#
# @r Basically <a text>, but indented a certain amount.
#
# @i indent
# @n This procedure is not checked by the testsuite.

proc ::textutil::adjust::indent {text prefix {skip 0}} {
    set text [string trimright $text]

    set res [list]
    foreach line [split $text \n] {
	if {[string compare "" [string trim $line]] == 0} {
	    lappend res {}
	} else {
	    set line [string trimright $line]
	    if {$skip <= 0} {
		lappend res $prefix$line
	    } else {
		lappend res $line
	    }
	}
	if {$skip > 0} {incr skip -1}
    }
    return [join $res \n]
}

# Undent the block of text: Compute LCP (restricted to whitespace!)
# and remove that from each line. Note that this preverses the
# shaping of the paragraph (i.e. hanging indent are _not_ flattened)
# We ignore empty lines !!

proc ::textutil::adjust::undent {text} {

    if {$text == {}} {return {}}

    set lines [split $text \n]
    set ne [list]
    foreach l $lines {
	if {[string length [string trim $l]] == 0} continue
	lappend ne $l
    }
    set lcp [::textutil::string::longestCommonPrefixList $ne]

    if {[string length $lcp] == 0} {return $text}

    regexp "^(\[\t \]*)" $lcp -> lcp

    if {[string length $lcp] == 0} {return $text}

    set len [string length $lcp]

    set res [list]
    foreach l $lines {
	if {[string length [string trim $l]] == 0} {
	    lappend res {}
	} else {
	    lappend res [string range $l $len end]
	}
    }
    return [join $res \n]
}

# ### ### ### ######### ######### #########
## Data structures

namespace eval ::textutil::adjust {
    variable here [file dirname [info script]]

    variable Justify      left
    variable Length       72
    variable FullLine     0
    variable StrictLength 0
    variable Hyphenate    0
    variable HyphPatterns

    namespace export adjust indent undent
}

# ### ### ### ######### ######### #########
## Ready

package provide textutil::adjust 0.7.3

#
# As the author of the procs 'tabify2' and 'untabify2' I suggest that the
# comments explaining their behaviour be kept in this file.
# 1) Beginners in any programming language (I am new to Tcl so I know what I
#    am talking about) can profit enormously from studying 'correct' code.
#    Of course comments will help a lot in this regard.
# 2) Many problems newbies face can be solved by directing them towards
#    available libraries - after all, libraries have been written to solve
#    recurring problems. Then they can just use them, or have a closer look
#    to see and to discover how things are done the 'Tcl way'.
# 3) And if ever a proc from a library should be less than perfect, having
#    comments explaining the behaviour of the code will surely help.
#
# This said, I will welcome any error reports or suggestions for improvements
# (especially on the 'doing things the Tcl way' aspect).
#
# Use of these sources is licensed under the same conditions as is Tcl.
#
# June 2001, Helmut Giese (hgiese@ratiosoft.com)
#
# ----------------------------------------------------------------------------
#
# The original procs 'tabify' and 'untabify' each work with complete blocks
# of $num spaces ('num' holding the tab size). While this is certainly useful
# in some circumstances, it does not reflect the way an editor works:
# 	Counting columns from 1, assuming a tab size of 8 and entering '12345'
#   followed by a tab, you expect to advance to column 9. Your editor might
#   put a tab into the file or 3 spaces, depending on its configuration.
#	Now, on 'tabifying' you will expect to see those 3 spaces converted to a
#	tab (and on the other hand expect the tab *at this position* to be
#	converted to 3 spaces).
#
#	This behaviour is mimicked by the new procs 'tabify2' and 'untabify2'.
#   Both have one feature in common: They accept multi-line strings (a whole
#   file if you want to) but in order to make life simpler for the programmer,
#   they split the incoming string into individual lines and hand each line to
#   a proc that does the real work.
#
#   One design decision worth mentioning here:
#      A single space is never converted to a tab even if its position would
#      allow to do so.
#   Single spaces occur very often, say in arithmetic expressions like
#   [expr (($a + $b) * $c) < $d]. If we didn't follow the above rule we might
#   need to replace one or more of them to tabs. However if the tab size gets
#   changed, this expression would be formatted quite differently - which is
#   probably not a good idea.
#
#   'untabifying' on the other hand might need to replace a tab with a single
#   space: If the current position requires it, what else to do?
#   As a consequence those two procs are unsymmetric in this aspect, but I
#   couldn't think of a better solution. Could you?
#
# ----------------------------------------------------------------------------
#

# ### ### ### ######### ######### #########
## Requirements

package require Tcl 8.2
package require textutil::repeat

namespace eval ::textutil::tabify {}

# ### ### ### ######### ######### #########
## API implementation

namespace eval ::textutil::tabify {
    namespace import -force ::textutil::repeat::strRepeat
}

proc ::textutil::tabify::tabify { string { num 8 } } {
    return [string map [list [MakeTabStr $num] \t] $string]
}

proc ::textutil::tabify::untabify { string { num 8 } } {
    return [string map [list \t [MakeTabStr $num]] $string]
}

proc ::textutil::tabify::MakeTabStr { num } {
    variable TabStr
    variable TabLen

    if { $TabLen != $num } then {
	set TabLen $num
	set TabStr [strRepeat " " $num]
    }

    return $TabStr
}

# ----------------------------------------------------------------------------
#
# tabifyLine: Works on a single line of text, replacing 'spaces at correct
# 		positions' with tabs. $num is the requested tab size.
#		Returns the (possibly modified) line.
#
# 'spaces at correct positions': Only spaces which 'fill the space' between
# an arbitrary position and the next tab stop can be replaced. 
# Example: With tab size 8, spaces at positions 11 - 13 will *not* be replaced,
#          because an expansion of a tab at position 11 will jump up to 16.
# See also the comment at the beginning of this file why single spaces are
# *never* replaced by a tab.
#
# The proc works backwards, from the end of the string up to the beginning:
#	- Set the position to start the search from ('lastPos') to 'end'.
#	- Find the last occurrence of ' ' in 'line' with respect to 'lastPos'
#         ('currPos' below). This is a candidate for replacement.
#       - Find to 'currPos' the following tab stop using the expression
#           set nextTab [expr ($currPos + $num) - ($currPos % $num)]
#         and get the previous tab stop as well (this will be the starting 
#         point for the next iteration).
#	- The ' ' at 'currPos' is only a candidate for replacement if
#	  1) it is just one position before a tab stop *and*
#	  2) there is at least one space at its left (see comment above on not
#	     touching an isolated space).
#	  Continue, if any of these conditions is not met.
#	- Determine where to put the tab (that is: how many spaces to replace?)
#	  by stepping up to the beginning until
#		-- you hit a non-space or
#		-- you are at the previous tab position
#	- Do the replacement and continue.
#
# This algorithm only works, if $line does not contain tabs. Otherwise our 
# interpretation of any position beyond the tab will be wrong. (Imagine you 
# find a ' ' at position 4 in $line. If you got 3 leading tabs, your *real*
# position might be 25 (tab size of 8). Since in real life some strings might 
# already contain tabs, we test for it (and eventually call untabifyLine).
#

proc ::textutil::tabify::tabifyLine { line num } {
    if { [string first \t $line] != -1 } { 		
	# assure array 'Spaces' is set up 'comme il faut'
	checkArr $num
	# remove existing tabs
	set line [untabifyLine $line $num]
    }

    set lastPos end

    while { $lastPos > 0 } {
	set currPos [string last " " $line $lastPos]
	if { $currPos == -1 } {
	    # no more spaces
	    break;
	}

	set nextTab [expr {($currPos + $num) - ($currPos % $num)}]
	set prevTab [expr {$nextTab - $num}]

	# prepare for next round: continue at 'previous tab stop - 1'
	set lastPos [expr {$prevTab - 1}]

	if { ($currPos + 1) != $nextTab } {
	    continue			;# crit. (1)
	}

	if { [string index $line [expr {$currPos - 1}]] != " " } {
	    continue			;# crit. (2)
	}

	# now step backwards while there are spaces
	for {set pos [expr {$currPos - 2}]} {$pos >= $prevTab} {incr pos -1} {
	    if { [string index $line $pos] != " " } {
		break;
	    }
	}

	# ... and replace them
	set line [string replace $line [expr {$pos + 1}] $currPos \t]
    }
    return $line
}

#
# Helper proc for 'untabifyLine': Checks if all needed elements of array
# 'Spaces' exist and creates the missing ones if needed.
#

proc ::textutil::tabify::checkArr { num } {
    variable TabLen2
    variable Spaces

    if { $num > $TabLen2 } {
	for { set i [expr {$TabLen2 + 1}] } { $i <= $num } { incr i } {
	    set Spaces($i) [strRepeat " " $i]
	}
	set TabLen2 $num
    }
}


# untabifyLine: Works on a single line of text, replacing tabs with enough
#		spaces to get to the next tab position.
#		Returns the (possibly modified) line.
#
# The procedure is straight forward:
#	- Find the next tab.
#	- Calculate the next tab position following it.
#	- Delete the tab and insert as many spaces as needed to get there.
#

proc ::textutil::tabify::untabifyLine { line num } {
    variable Spaces

    set currPos 0
    while { 1 } {
	set currPos [string first \t $line $currPos]
	if { $currPos == -1 } {
	    # no more tabs
	    break
	}

	# how far is the next tab position ?
	set dist [expr {$num - ($currPos % $num)}]
	# replace '\t' at $currPos with $dist spaces
	set line [string replace $line $currPos $currPos $Spaces($dist)]

	# set up for next round (not absolutely necessary but maybe a trifle
	# more efficient)
	incr currPos $dist
    }
    return $line
}

# tabify2: Replace all 'appropriate' spaces as discussed above with tabs.
#	'string' might hold any number of lines, 'num' is the requested tab size.
#	Returns (possibly modified) 'string'.
#
proc ::textutil::tabify::tabify2 { string { num 8 } } {

    # split string into individual lines
    set inLst [split $string \n]

    # now work on each line
    set outLst [list]
    foreach line $inLst {
	lappend outLst [tabifyLine $line $num]
    }

    # return all as one string
    return [join $outLst \n]
}


# untabify2: Replace all tabs with the appropriate number of spaces.
#	'string' might hold any number of lines, 'num' is the requested tab size.
#	Returns (possibly modified) 'string'.
#
proc ::textutil::tabify::untabify2 { string { num 8 } } {

    # assure array 'Spaces' is set up 'comme il faut'
    checkArr $num

    set inLst [split $string \n]

    set outLst [list]
    foreach line $inLst {
	lappend outLst [untabifyLine $line $num]
    }

    return [join $outLst \n]
}



# ### ### ### ######### ######### #########
## Data structures

namespace eval ::textutil::tabify {
    variable TabLen  8
    variable TabStr  [strRepeat " " $TabLen]

    namespace export tabify untabify tabify2 untabify2
    
    # The proc 'untabify2' uses the following variables for efficiency.
    # Since a tab can be replaced by one up to 'tab size' spaces, it is handy
    # to have the appropriate 'space strings' available. This is the use of
    # the array 'Spaces', where 'Spaces(n)' contains just 'n' spaces.
    # The variable 'TabLen2' remembers the biggest tab size used.

    variable  TabLen2 0
    variable  Spaces
    array set Spaces {0 ""}
}

# ### ### ### ######### ######### #########
## Ready

package provide textutil::tabify 0.7

# Copyright (c) 2019-2021, Ashok P. Nadkarni
# All rights reserved.
# See the file LICENSE in the source root directory for license.

namespace eval ruff::formatter {}

oo::class create ruff::formatter::Formatter {
    # Data members
    variable References; # Links for cross-reference purposes
    variable Options;    # Document generation options
    variable Namespaces; # Namespaces we are documenting
    variable SortedNamespaces;  # Exactly what it says
    variable FigureCounter; # Counter for figure captions

    constructor {} {
        # Base class for output formatters.
        namespace path [linsert [namespace path] 0 ::ruff ::ruff::private]
        set References [dict create]
    }

    method Option {opt {default {}}} {
        # Returns the value of an option.
        # opt - The option whose value is to be returned.
        if {[info exists Options($opt)]} {
            return $Options($opt)
        }
        return $default
    }
    method Option? {opt var} {
        # Check if option exists and store its variable.
        #  opt - The option of interest.
        #  var - The variable in the caller's context to store the value
        # The value of the option is stored in the variable $var in the
        # callers's context. The variable is unmodified if the option does
        # not exist.
        # Returns 1 if the option exists and 0 otherwise.
        if {[info exists Options($opt)]} {
            upvar 1 $var val
            set val $Options($opt)
            return 1
        }
        return 0
    }

    method Begin {} {
        # Begins the actual generation of the documentation set.
        # 
        # This method should be overridden by the concrete formatter.
        # It should generate appropriate content for the header and other
        # parts that are not dependent on the actual content.
    }

    method DocumentBegin {ns} {
        # Begins the generation of one document.
        #  ns - the namespace for the document. An empty string is passed
        #       for the main document.
        # This method should be overridden by the concrete formatter.
        # It should take any actions necessary to create a new document
        # in the documentation set. Subsequent calls to [fmtpara] and
        # other formatting methods should add to this document.
        set FigureCounter 0
    }

    method DocumentEnd {} {
        # Ends the generation of the current document.
        #
        # Returns the completed document.
        #
        # This method should be overridden by the concrete formatter.
    }

    method AddHeading {level text scope {tooltip {}}} {
        # Adds a heading to document content.
        #  level   - The heading level. May be either a numeric level or
        #            a semantic one keying into the HeaderLevels dictionary.
        #  text    - The heading text.
        #  scope   - The documentation scope of the content.
        #  tooltip - Tooltip as list of lines to display in navigation link.
        # This method should be overridden by the concrete formatter.
        error "Method AddHeading not overridden."
    }

    method AddParagraph {lines scope} {
        # Adds a paragraph to document content.
        #  lines  - List of lines in the paragraph.
        #  scope - The documentation scope of the content.
        # This method should be overridden by the concrete formatter.
        error "Method AddParagraph not overridden."
    }

    method AddParagraphText {text scope} {
        # Adds a paragraph to the document content.
        #  text - Paragraph text to add.
        #  scope - The documentation scope of the content.
        # This is similar to [AddParagraph] except that it accepts a
        # text string as the paragraph as opposed to a list of lines.
        return [my AddParagraph [list $text] $scope]
    }

    method AddDefinitions {definitions scope {preformatted none}} {
        # Adds a definitions block to document content.
        #  definitions  - List of definitions.
        #  scope        - The documentation scope of the content.
        #  preformatted - One of `none`, `both`, `term` or `definition`
        #                 indicating which fields of the definition are
        #                 are already formatted.
        # Each element of $definitions is a dictionary with keys `term`
        # and `definition`. The latter is a list of strings comprising
        # the definition content.
        #
        # This method should be overridden by the concrete formatter.
        error "Method AddDefinitions not overridden."
    }

    method AddParameters {parameters scope} {
        # Adds a parameters section to document content.
        #  parameters - List of parameter definitions each being a dictionary
        #               with keys `type` (parameter or option), `term` being
        #               the parameter text and `definition` being the list of
        #               lines comprising the description.
        #  scope      - The documentation scope of the content.
        #
        # This method formats the parameters as a definition list with
        # arguments italized.

        if {[llength $parameters] == 0} {
            return;             # Do not want even heading if not parameters
        }
        my AddHeading parameters [::msgcat::mc Parameters] $scope

        # Construct a definition block for the parameters
        set definitions [lmap param $parameters {
            set definition [dict get $param definition]
            set term [dict get $param term]
            if {$definition eq "" && $term eq "args"} {
                # Do not document "args" unless specific description given
                # This also makes custom synopsis output a little cleaner
                continue
            }
            set term [my FormatInline [markup_code [dict get $param term]]]
            dict create term $term definition $definition
        }]
        my AddDefinitions $definitions $scope term
        return
    }

    method AddBullets {bullets scope} {
        # Adds a bulleted list to document content.
        #  bullets  - The list of bullets.
        #  scope    - The documentation scope of the content.
        # Each element of $bullets is a list of strings.
        #
        # This method should be overridden by the concrete formatter.
        error "Method AddBullets not overridden."
    }

    method AddPreformattedText {text scope} {
        # Adds preformatted text to document content.
        #  text  - Preformatted text as a string.
        #  scope - The documentation scope of the content.
        # This method should be overridden by the concrete formatter.
        error "Method AddPreformattedText not overridden."
    }

    method AddPreformatted {lines scope} {
        # Adds list of preformatted lines to document content.
        #  lines - Preformatted text as a list of lines.
        #  scope - The documentation scope of the content.
        # [Formatter] provides a base implementation that may be overridden.
        my AddPreformattedText [join $lines \n] $scope
        return
    }

    method AddFenced {lines fence_options scope} {
        # Adds a list of fenced lines to document content.
        #  lines - Preformatted text as a list of lines.
        #  fence_options - options specified with the fence, e.g. diagram ...
        #  scope - The documentation scope of the content.
        # [Formatter] provides a base implementation that ignores the
        # modifier and treats the lines as preformatted lines.
        # It may be overridden by derived classes.
        my AddPreformatted $lines $scope
        return
    }

    method AddReferences {xrefs scope {title {}}} {
        # Adds reference list to document content.
        #  xrefs - List of cross references and links.
        #  scope - The documentation scope of the content.
        #  title - If not empty, a section title is also added.
        #
        # The elements in $xrefs may be plain symbols or Markdown links.
        #
        # [Formatter] provides a base implementation that may be overridden.
        if {[llength $xrefs] == 0} {
            return
        }

        if {$title ne ""} {
            my AddHeading nonav $title $scope
        }

        set re_inlinelink  {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\]\s*\(\s*((?:[^\s\)]+|\([^\s\)]+\))+)?(\s+([\"'])(.*)?\4)?\s*\)}
        set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_autolink    {\A<(?:(\S+@\S+)|(\S+://\S+))>}
        append text [join [lmap xref $xrefs {
            # If the xref looks like a markdown link, keep as is, else make it
            # look like a symbol or heading reference.
            if {[regexp $re_inlinelink $xref] ||
                [regexp $re_reflink $xref] ||
                [regexp $re_autolink $xref]} {
                set xref
            } else {
                markup_reference $xref
            }
        }] ", " ]

        # aa bb -> "[aa], [bb]"
        # The NOTE below is no longer valid as of Ruff 1.0.4 where intervening space
        # will be recognized as two separate reflinks.
        # NOTE: the , in the join is not purely cosmetic. It is also a
        # workaround for Markdown syntax which treats [x] [y] with
        # only intervening whitespace as one text/linkref pair.
        # This Markdown behaviour differs from the CommonMark Markdown spec.
        #
        my AddParagraphText $text $scope
        return
    }

    method AddSynopsis {synopsis scope} {
        # Adds a Synopsis section to the document content.
        #  synopsis - List of alternating elements comprising, in turn,
        #             the command portion and the parameter list.
        # [Formatter] provides a base implementation that may be overridden.

        my AddHeading nonav Synopsis $scope
        my AddPreformatted [lmap {cmd params} $synopsis {
            concat $cmd $params
        }] $scope
        return
    }

    method AddSource {source scope} {
        # Adds a Source code section to the document content.
        #  source - Source code fragment.
        # [Formatter] provides a base implementation that may be overridden.
        my AddHeading nonav Source $scope
        my AddPreformattedText $source $scope
        return
    }

    method AddForward {fwdinfo} {
        # Adds documentation for a class forwarded method.
        #  fwdinfo - dictionary describing the forwarded method
        # The passed dictionary holds the following keys:
        #  name - name of the method
        #  fqn - Fully qualifed name
        #  forward - command to which the method is forwarded
        #
        # This method may be overridden by the concrete formatter.

        set fqn [dict get $fwdinfo fqn]
        set scope [namespace qualifiers $fqn]
        # Forwards are formatted like methods
        my AddProgramElementHeading method $fqn
        my AddParagraph "Forwarded to `[dict get $fwdinfo forward]`." $scope
    }

    method AddProcedureDetail {procinfo} {
        # Adds the detailed information about a procedure or method
        #  procinfo - dictionary describing the procedure. See [AddProcedure]
        #
        #  The concrete implementation can override this.

        dict with procinfo {
            # Creates the following locals
            #  proctype, display_name, fqn, synopsis, parameters, summary,
            #  body, seealso, returns, source
            #
            # Only the fqn and proctype are mandatory.
        }

        set scope [namespace qualifiers $fqn]

        if {[info exists parameters]} {
            my AddParameters $parameters $scope
        }

        if {[info exists body] && [llength $body]} {
            my AddHeading nonav [::msgcat::mc Description] $scope
            my AddParagraphs $body $scope
        }

        if {[info exists returns]} {
            my AddHeading nonav [::msgcat::mc "Return value"] $scope
            my AddParagraph $returns $scope
        }

        if {[info exist seealso]} {
            my AddReferences $seealso $scope [::msgcat::mc "See also"]
        }

        if {[my Option -includesource 0] && [info exists source]} {
            my AddSource $source $scope
        }

        return
    }

    method AddProcedure {procinfo} {
        # Adds documentation for a procedure or method.
        #  procinfo - dictionary describing the procedure.
        # The passed $procinfo dictionary holds the following keys:
        #  proctype     - `proc` or `method`
        #  display_name - The name to be displayed.
        #  fqn          - The fully qualified name used to construct references.
        #  synopsis     - Procedure synopsis as a alternating list comprising
        #                 the command portion and the list of arguments for it.
        #  parameters   - List of parameters in the form of a definition
        #                 list.
        #  summary      - The summary text.
        #  body         - The main description section. A list of paragraphs
        #                 in the form accepted by [AddParagraphs].
        #  returns      - The text for the **Returns** section.
        #  seealso      - The list of cross references.
        #  source       - Source code for the procedure. Should be shown
        #                 if present.
        #
        # Only the proctype and display_name key are mandatory.
        #
        # This method may be overridden by the concrete formatter.

        dict with procinfo {
            # Creates the following locals
            #  proctype, display_name, fqn, synopsis, parameters, summary,
            #  body, seealso, returns, source
            #
            # Only the fqn and proctype are mandatory.
        }

        set scope [namespace qualifiers $fqn]
        if {[info exists summary]} {
            if {[info exists synopsis]} {
                my AddProgramElementHeading $proctype $fqn $summary $synopsis
            } else {
                my AddProgramElementHeading $proctype $fqn $summary
            }
            my AddParagraph $summary $scope
        } else {
            if {[info exists synopsis]} {
                my AddProgramElementHeading $proctype $fqn "" $synopsis
            } else {
                my AddProgramElementHeading $proctype $fqn
            }
        }

        if {[info exists synopsis]} {
            my AddSynopsis $synopsis $scope
        }

        my AddProcedureDetail $procinfo

        return

    }

    method CollectHeadingReference {ns heading} {
        # Adds a reference to a heading to the cross-reference table.
        #  ns - Namespace containing the heading
        #  heading - Text of the heading.
        # Returns the reference for the added heading.
        set ref [my HeadingReference $ns $heading]
        set reference [dict create type heading ref $ref]
        dict set References $heading $reference
        dict set References "${ns}::$heading" $reference
        return $ref
    }

    method CollectSymbolReference {ns symbol {ref {}}} {
        # Adds a reference for a symbol in a namespace to the cross-reference
        # table.
        #  ns     - Namespace containing the heading
        #  symbol - Text of the symbol.
        #  ref    - The reference to use. If empty, the reference is constructed
        #           from the symbol.
        # Returns the reference for the added heading.
        if {$ref eq ""} {
            set ref [my SymbolReference $ns $symbol]
        }
        set reference [dict create type symbol ref $ref]
        dict set References $symbol $reference
        return $ref
    }

    method CollectFigureReference {ns caption {ref {}}} {
        # Adds a reference for a figure in a namespace to the cross-reference
        # table.
        #  ns     - Namespace containing the figure
        #  caption - Figure caption
        #  ref    - The reference to use. If empty, the reference is constructed
        #           from the caption.
        # Returns the reference for the added heading.
        if {$ref eq ""} {
            set ref [my FigureReference $ns $caption]
        }
        incr FigureCounter
        set reference [dict create \
                           type figure \
                           ref $ref \
                           label "[::msgcat::mc Figure] $FigureCounter. $caption"]
        dict set References $caption $reference
        return $ref
    }
    export CollectFigureReference

    method Reference? {lookup refvar} {
        # Looks up the cross-reference table.
        #  lookup - The string to look up.
        #  refvar - Name of a variable in the caller's context to store the
        #           reference.
        # Returns 1 if the reference exists and stores it in $refvar otherwise
        # returns 0 without modifying the variable
        #
        # The value stored in $refvar is a dictionary with keys `type`
        # (`heading`, `symbol` or `figure`) and `ref` (the reference).
        if {[dict exists $References $lookup]} {
            upvar 1 $refvar ref
            set ref [dict get $References $lookup]
            return 1
        }
        return 0
    }

    method ResolvableReference? {lookup scope refvar} {
        # Resolves a reference by searching through containing scopes.
        #  lookup - The string to look up.
        #  scope  - Namespace scope to search.
        #  refvar - Name of a variable in the caller's context to store result.
        #
        # If resolved successfully, the variable $refvar in the caller's
        # contains a dictionary with keys type (`heading`, `symbol` or `figure`),
        # the ref, and label (display label).
        #
        # Returns 1 if the reference exists and stores it in $refvar otherwise
        # returns 0 without modifying the variable

        # If the label falls within the specified scope, we will hide the scope
        # in the displayed label. The label may fall within the scope either
        # as a namespace (::) or a class member (.)

        # If reference is not directly present, we will look up search path
        # but only if lookup value is not fully qualified.
        if {![my Reference? $lookup ref] && ! [string match ::* $lookup]} {
            while {$scope ne "" && ![info exists ref]} {
                # Check class (.) and namespace scope (::)
                if {[my Reference? ${scope}.$lookup ref]} {
                    break
                }
                if {[my Reference? ${scope}::$lookup ref]} {
                    break
                }
                set scope [namespace qualifiers $scope]
            }
        }
        if {[info exists ref]} {
            upvar 1 $refvar upref
            set upref $ref
            if {![dict exists $upref label]} {
                dict set upref label [trim_namespace $lookup $scope]
            }
            return 1
        }
        return 0
    }
    export ResolvableReference?

    method HeadingReference {ns heading} {
        # Generates a reference for a heading in a namespace.
        #  ns - the namespace containing the heading
        #  heading - the text of the heading.
        # This method should be overridden by the concrete formatter.
        # Returns the reference to the heading.
        error "Method HeadingReference not overridden."
    }

    method SymbolReference {ns symbol} {
        # Generates a reference for a symbol in a namespace.
        #  ns - the namespace containing the symbol.
        #  symbol - the text of the symbol.
        # This method should be overridden by the concrete formatter.
        # Returns the reference to the symbol.

        # NOTE: $ns is a separate parameter because although name
        # must be fully qualified, the parent is not necessarily
        # the name space scope because for methods, the class name
        # is the parent but $ns will be the class's parent namespace.

        error "Method SymbolReference not overridden."
    }

    method CollectReferences {ns ns_content} {
        # Collects links to documentation elements in a namespace.
        #  ns - The namespace containing the program elements.
        #  ns_content - Dictionary containing parsed content for the namespace.
        # Returns a dictionary mapping the documentation element name
        # to the text linking to that element.

        # Set up a link for the namespace itself
        my CollectSymbolReference $ns $ns

        # Gather links for preamble headings
        foreach {type content} [dict get $ns_content preamble] {
            if {$type eq "heading"} {
                lassign $content level heading
                my CollectHeadingReference $ns $heading
            }
        }

        # Gather links for procs
        foreach proc_name [dict keys [dict get $ns_content procs]] {
            fqn! $proc_name
            ns_member! $ns $proc_name
            my CollectSymbolReference $ns $proc_name
        }

        # Finally gather links for classes and methods
        # A class name is also treated as a namespace component
        # although that is not strictly true.
        foreach {class_name class_info} [dict get $ns_content classes] {
            ns_member! $ns $class_name
            my CollectSymbolReference $ns $class_name
            set method_info_list [concat [dict get $class_info methods] [dict get $class_info forwards]]
            foreach name {constructor destructor} {
                if {[dict exists $class_info $name]} {
                    lappend method_info_list [dict get $class_info $name]
                }
            }
            foreach method_info $method_info_list {
                # The class name is the scope for methods. Because of how
                # the link target lookup works, we use the namespace
                # operator to separate the class from method. We also
                # store it a second time using the "." separator as that
                # is how they are sometimes referenced.
                set method_name [dict get $method_info name]
                set ref [my CollectSymbolReference $ns ${class_name}::${method_name}]
                my CollectSymbolReference $ns ${class_name}.${method_name} $ref
            }
        }
    }

    method AddParagraphs {paras {scope {}}} {
        # Calls the formatter for each of the passed paragraphs.
        # paras - A flat list of pairs with the first element
        #         in a pair being the type, and the second the content.
        # scope - The namespace scope for the paragraphs.

        foreach {type content} $paras {
            switch -exact -- $type {
                heading {
                    my AddHeading {*}$content $scope
                }
                paragraph {
                    my AddParagraph $content $scope
                }
                definitions {
                    my AddDefinitions $content $scope none
                }
                bullets {
                    my AddBullets $content $scope
                }
                preformatted {
                    my AddPreformatted $content $scope
                }
                fenced {
                    my AddFenced {*}$content $scope
                }
                seealso -
                default {
                    error "Unknown or unexpected paragraph element type '$type'."
                }
            }
        }
        return
    }

    method Namespaces {} {
        # Returns the list of namespaces being documented.
        return $Namespaces
    }

    method SortedNamespaces {} {
        # Returns the list of namespaces being documented.
        if {![info exists SortedNamespaces]} {
            set SortedNamespaces [lsort -dictionary $Namespaces]
        }
        return $SortedNamespaces
    }

    method TransformProcOrMethod {procinfo} {
        # Transforms procedure or method information into form required
        # by formatters.
        #   procinfo - Proc or method information in the format returned
        #    by [ruff::private::extract_proc] or [ruff::private::extract_ooclass].
        #
        # The following document options control specific behaviour.
        #   -includesource BOOLEAN - if true, the source code of the
        #    procedure is also included. Default value is false.
        #   -hidenamespace NAMESPACE - if specified as non-empty,
        #    program element names beginning with NAMESPACE are shown
        #    with that namespace component removed.
        #
        # Returns the proc documentation as a dictionary in the form
        # expected by the [AddProcedure] method.
        set includesource [my Option -includesource false]
        set hidenamespace [my Option -hidenamespace ""]

        dict with procinfo {
            # Creates local vars (IF PRESENT):
            # proctype - method or proc
            # name - proc or method name
            # parameters - parameter definitions
            # summary - summary text
            # returns - return value text
            # seealso - cross references
            # synopsis - parameter names for synopsis
            # class - class (for methods)
        }

        set proc_name    $name
        set parameter_block $parameters; # Since we reuse the name
        set display_name [trim_namespace $proc_name $hidenamespace]

        if {$proctype eq "method"} {
            set scope $class; # Scope is name of class
            set fqn   ${class}::$proc_name
        } else {
            set scope [namespace qualifiers $name]
            set fqn   $proc_name
        }

        # Construct the synopsis and simultaneously the parameter descriptions
        set parameters {}
        set arglist {};             # Used later for synopsis
        foreach param $parameter_block {
            set param_name [dict get $param term]
            if {[dict exists $param definition]} {
                set desc [dict get $param definition]
            } elseif {$param_name eq "args"} {
                set desc [list "Additional options."]
            }
            # The type may be parameter or option
            set param_type [dict get $param type]
            if {$param_type eq "parameter"} {
                if {![dict exists $param default]} {
                    # Little buglet here since args is actually special
                    # only if it is the last argument. Oh well...
                    if {$param_name eq "args"} {
                        lappend arglist "?$param_name?"
                    } else {
                        lappend arglist $param_name
                    }
                } else {
                    lappend arglist "?$param_name?"
                    set optval [dict get $param default]
                    if {$optval eq ""} {
                        set optval \"\"
                    }
                    # If we add on a default clause, the autopunctuation
                    # done in the formatters misses opportunities to punctuate
                    # so we do so here. TBD - refactor into a separate proc.
                    if {[my Option -autopunctuate 0]} {
                        set desc [lreplace $desc 0 0 [string toupper [lindex $desc 0] 0 0]]
                        set last_frag [lindex $desc end]
                        if {[regexp {[[:alnum:]]} [string index $last_frag end]]} {
                            set desc [lreplace $desc end end "${last_frag}."]
                        }
                    }
                    # set desc [linsert $desc 0 "(optional, default [markup_code $optval])" ]
                    lappend desc "Optional, default [markup_code $optval]."
                }
            }

            lappend parameters [list term $param_name definition $desc type $param_type]
        }

        if {$proctype ne "method"} {
            if {[info exists synopsis] && [llength $synopsis]} {
                # Customized parameter list
                foreach param_names $synopsis[set synopsis ""] {
                    lappend synopsis [namespace tail $display_name] $param_names
                }
            } else {
                set synopsis [list [namespace tail $display_name] $arglist]
            }
        } else {
            if {[info exists synopsis] && [llength $synopsis]} {
                # Customized parameter list
                foreach param_names $synopsis[set synopsis ""] {
                    lappend synopsis "OBJECT $display_name" $param_names
                }
            } else {
                switch -exact -- $proc_name {
                    constructor {
                        set unqual_name [namespace tail $class]
                        set synopsis [list \
                                          "$unqual_name create OBJNAME" \
                                          $arglist \
                                          "$unqual_name new" \
                                          $arglist]
                    }
                    destructor  {set synopsis [list "OBJECT destroy"]}
                    default  {
                        set synopsis [list "OBJECT $display_name" $arglist]
                    }
                }
            }
        }

        if {![info exists summary] || $summary eq ""} {
            if {[info exists returns] && $returns ne ""} {
                set summary $returns
            }
        }

        if {$includesource && [info exists source]} {
            if {[info exists ensemble]} {
                append source "\n# NOTE: showing source of procedure implementing ensemble subcommand."
            }
        }

        set result [dict create \
                        proctype $proctype \
                        display_name $display_name \
                        fqn $fqn]
        foreach key {synopsis parameters summary body returns seealso source} {
            if {[info exists $key]} {
                dict set result $key [set $key]
            }
        }

        return $result
    }

    method AddProcedures {procinfodict} {
        # Adds documentation for procedures.
        # procinfodict - Dictionary keyed by name of the proc.
        #                The associated value is in the format returned by
        #                [ruff::private::extract_proc].

        set proc_names [lsort -dictionary [dict keys $procinfodict]]
        foreach proc_name $proc_names {
            my AddProcedure [my TransformProcOrMethod [dict get $procinfodict $proc_name]]
        }
        return
    }

    method TransformClass {classinfo} {
        # Transforms class information into form required by formatters.
        #   classinfo - Class information in the format returned
        #    by [ruff::private::extract_ooclass].
        #
        # The following document options control specific behaviour.
        #   -includesource - if true, the source code of the
        #    procedure is also included. Default value is false.
        #   -hidenamespace - if non-empty, program element names beginning
        #    with NAMESPACE are shown with that namespace component removed.
        #
        # Returns the class documentation as a dictionary in the form
        # expected by the [AddClass] method.

        set includesource [my Option -includesource false]
        set hidenamespace [my Option -hidenamespace ""]

        dict with classinfo {
            # Creates the following locals
            # name - name of class
            # superclasses - list of superclasses
            # mixins - list of mixin classes
            # subclasses - list of subclasses
            # external_methods - list of {method class} pairs
            # filters - ?
            # constructor - constructor definition
            # destructor - destructor definition
            # methods - list of method definitions
            # forwards - list of forwarded methods
        }

        set fqn          $name
        set display_name [trim_namespace $fqn $hidenamespace]

        foreach var {superclasses subclasses mixins} {
            # NOTE: do not sort the list. Order is important for semantics.
            if {[info exists $var] && [llength [set $var]]} {
                set referenced_classes [set $var]
                set $var {}
                foreach referenced_class $referenced_classes {
                    if {[namespace qualifiers $referenced_class] eq [namespace qualifiers $fqn]} {
                        lappend $var [namespace tail $referenced_class]
                    } else {
                        lappend $var [trim_namespace $referenced_class $hidenamespace]
                    }
                }
            }
        }

        # TBD - filters need any processing?

        # Methods are summarized as a definition list.
        set method_summaries [list ]

        # NOTE: Constructor and destructor are added later after other
        # methods are sorted.
        if {[info exists methods]} {
            foreach method_info $methods {
                set method_name [dict get $method_info name]
                if {[dict exists $method_info summary]} {
                    set summary [dict get $method_info summary]
                } elseif {[dict exists $method_info returns]} {
                    set summary [dict get $method_info returns]
                } else {
                    set summary [list "Not documented."]
                }
                lappend method_summaries [list term $method_name definition $summary]
            }
        }
        if {[info exists forwards]} {
            foreach forward_info $forwards {
                set method_name [dict get $forward_info name]
                set summary [list "Method forwarded to [dict get $forward_info forward]"]
                lappend method_summaries [list term $method_name definition $summary]
            }
        }

        # Also add external methods to the method summary.
        if {[info exists external_methods]} {
            foreach external_method $external_methods {
                lassign $external_method method_name imp_class
                if {[namespace qualifiers $fqn] eq [namespace qualifiers $imp_class]} {
                    set referenced_class [namespace tail $imp_class]
                } else {
                    set referenced_class $imp_class
                }
                lappend method_summaries [list term $method_name definition [list "See [markup_reference $referenced_class.$method_name]"]]
            }
        }

        # Sort the method summary table alphabetically
        set method_summaries [lsort -dictionary -index 1 $method_summaries]

        # Insert constructor and destructor at the beginning if present.
        set specials {}
        if {[info exists constructor]} {
            lappend specials [list term "constructor" definition "Constructor for the class."]
        }
        if {[info exists destructor]} {
            lappend specials [list term "destructor" definition "Destructor for the class."]
        }
        if {[llength specials]} {
            set method_summaries [linsert $method_summaries 0 {*}$specials]
        }

        set methods [lmap method_info $methods {
            my TransformProcOrMethod $method_info
        }]
        if {[info exists constructor]} {
            set constructor [my TransformProcOrMethod $constructor]
        }
        if {[info exists destructor]} {
            set destructor [my TransformProcOrMethod $destructor]
        }

        set forwards [lmap fwd $forwards {
            # Set the fqn for forwarded methods
            dict set fwd fqn ${name}::[dict get $fwd name]
        }]

        set result [dict create fqn $fqn display_name $display_name]
        foreach key {
            superclasses subclasses mixins method_summaries mixins
            filters methods constructor destructor forwards
        } {
            if {[info exists $key]} {
                dict set result $key [set $key]
            }
        }
        return $result
    }

    method AddClass {classinfo} {
        # classinfo dictionary contains the following keys. All except
        # name are optional.
        #  fqn - Fully qualified name of the class
        #  display_name - Name of class for display purposes.
        #  superclasses - List of superclasses.
        #  subclasses - List of subclasses.
        #  mixins - List of mixins.
        #  filters - List of filter methods.
        #  method_summaries - Definition list mapping method name to description.
        #  methods - Dictionary of method definitions in the format generated
        #    by [TransformProcOrMethod].
        #  forwards - forwarded methods
        #  constructor - Constructor definition in the same format.
        #  destructor - Destructor definition in the same format.

        dict with classinfo {
            # Creates locals for all the classinfo keys listed above.
        }
        my AddProgramElementHeading class $fqn
        set scope $fqn
        if {[info exists method_summaries]} {
            my AddHeading nonav "Method summary" $scope
            # The method names need to be escaped and linked.
            my AddDefinitions [lmap definition $method_summaries {
                set term [dict get $definition term]
                # TBD - The resolution currently only searches the namespace
                # hierarchy, not the class hierarchy so methods defined
                # in superclasses/mixins etc. will not be found. So
                # those we just mark as code.
                if {[my ResolvableReference? $term $scope dontcare]} {
                    dict set definition term [markup_reference $term]
                } else {
                    dict set definition term [markup_code $term]
                }
            }] $scope none
        }
        foreach var {superclasses mixins subclasses filters} {
            if {[info exists $var]} {
                my AddReferences [set $var] $scope [string totitle $var]
            }
        }
        if {[info exists constructor]} {
            my AddProcedure $constructor
        }
        if {[info exists destructor]} {
            my AddProcedure $destructor
        }
        if {[info exists forwards]} {
            foreach fwd $forwards {
                lappend methods_and_forwards [list [dict get $fwd name] forward $fwd]
            }
        }
        if {[info exists methods]} {
            foreach meth $methods {
                lappend methods_and_forwards [list [dict get $meth display_name] method $meth]
            }
        }
        if {[info exists methods_and_forwards]} {
            set methods_and_forwards [lsort -dictionary -index 0 $methods_and_forwards]
            foreach rec $methods_and_forwards {
                if {[lindex $rec 1] eq "method"} {
                    my AddProcedure [lindex $rec 2]
                } else {
                    my AddForward [lindex $rec 2]
                }
            }
        }
        return
    }

    method AddClasses {classinfodict} {
        # Adds documentation for classes.
        # classinfodict - Dictionary keyed by name of the class.
        #                The associated value is in the format returned by
        #                [ruff::private::extract_class].

        set class_names [lsort -dictionary [dict keys $classinfodict]]
        foreach class_name $class_names {
            my AddClass [my TransformClass [dict get $classinfodict $class_name]]
        }
        return
    }

    method copy_assets {outdir} {
        # Copies any assets to the output directory.
        #   outdir - directory where output files will be stored
        #
        # A derived class may override this if it makes uses of any
        # static assets.
    }

    method extension {} {
        # Returns the default file extension to be used for output files.
        error "Method extension not overridden by derived class."
    }

    method generate_document {ns_info args} {
        # Produces documentation in HTML format from the passed in
        # class and proc metainformation.
        #   ns_info - dictionary keyed by namespace containing parsed documentation
        #    about the namespace.
        #   -autopunctuate BOOLEAN - If `true`, the first letter of definition
        #    descriptions (including parameter descriptions) is capitalized
        #    and a period added at the end if necessary.
        #   -compact BOOLEAN - If `true`, the a formatter-dependent compact
        #    form is generated.
        #   -preamble DICT - a dictionary indexed by a namespace. Each value is
        #    a flat list of pairs consisting of a heading and
        #    corresponding content. These are inserted into the document
        #    before the actual class and command descriptions for a namespace.
        #    The key "::" corresponds to documentation to be printed at
        #    the very beginning.
        #   -includesource BOOLEAN - if true, the source code of the
        #     procedure is also included. Default value is false.
        #   -hidenamespace NAMESPACE - if specified as non-empty,
        #    program element names beginning with NAMESPACE are shown
        #    with that namespace component removed.
        #   -makeindex BOOL - if true, include an index. Ignored for single page
        #   -navigation OPTS - `OPTS` must be a list of elements from amongst
        #    `left`, `right`, `narrow`, `normal` and `wide`. The first two specify
        #    the position of the navigation pane. The last three specify its width.
        #    Not supported by all formatters.
        #   -pagesplit SPLIT - if `none`, a single documentation file is produced.
        #    If `namespace`, a separate file is output for every namespace.
        #   -sortnamespaces BOOLEAN - if `true` (default) the namespaces are
        #    sorted in the navigation otherwise they are in the order passed in.
        #   -title STRING - the title for the documentation.
        #    Used as the title for the document.
        #    If undefined, the string "Reference" is used.

        set Namespaces [dict keys $ns_info]

        array set Options \
            [list \
                 -compact 0 \
                 -includesource false \
                 -hidenamespace "" \
                 -navigation {left normal} \
                 -pagesplit none \
                 -title "" \
                 -sortnamespaces 1 \
                 -autopunctuate 0 \
                ]

        array set Options $args

        if {![info exists Options(-makeindex)]} {
            set Options(-makeindex) [expr {$Options(-pagesplit) ne "none"}]
        }

        if {$Options(-pagesplit) eq "none" && $Options(-makeindex)} {
            app::log_error "Option -makeindex ignored if -pagesplit is specified as none."
            set Options(-makeindex) false
        }

        # First collect all "important" names so as to build a list of
        # linkable targets. These will be used for cross-referencing and
        # also to generate links correctly in the case of
        # duplicate names in different namespaces or classes.
        #

        # First collect section links
        if {[my Option? -preamble preamble]} {
            foreach {type content} $preamble {
                if {$type eq "heading"} {
                    lassign $content level heading
                    my CollectHeadingReference "" $heading
                }
            }
        }

        dict for {ns ns_content} $ns_info {
            my CollectReferences $ns $ns_content
        }

        my Begin
        my DocumentBegin ""
        if {[my Option? -preamble preamble] && $preamble ne ""} {
            # Top level documentation
            my AddParagraphs $preamble ""
        }
        if {[my Option -pagesplit none] ne "none"} {
            lappend docs :: [my DocumentEnd]
        }

        if {[my Option -sortnamespaces true]} {
            set ordered_namespaces [my SortedNamespaces]
        } else {
            set ordered_namespaces [my Namespaces]
        }
        foreach ns $ordered_namespaces {
            if {[my Option -pagesplit none] ne "none"} {
                my DocumentBegin $ns
            }

            set nprocs [dict size [dict get $ns_info $ns procs]]
            set nclasses [dict size [dict get $ns_info $ns classes]]
            # Horrible hack. Some namespaces are not really namespaces but are there
            # just as documentation sections and do not contain actual commands.
            # Strip the leading :: from them for display purposes.
            if {$nprocs == 0 && $nclasses == 0} {
                my AddHeading 1 [string trimleft $ns :] ""
            } else {
                # Adding "Reference" breaks the link anchor
                #my AddHeading 1 "$ns Reference" ""
                my AddHeading 1 $ns ""
            }

            # Print the preamble for this namespace
            my AddParagraphs [dict get $ns_info $ns preamble] $ns

            if {$nprocs != 0} {
                my AddHeading 2 [::msgcat::mc Commands] $ns
                my AddProcedures [dict get $ns_info $ns procs]
            }

            if {$nclasses != 0} {
                my AddHeading 2 [::msgcat::mc Classes] $ns
                my AddClasses [dict get $ns_info $ns classes]
            }

            if {[my Option -pagesplit none] ne "none"} {
                lappend docs $ns [my DocumentEnd]
            }
        }
        if {[my Option -pagesplit none] eq "none"} {
            lappend docs :: [my DocumentEnd]
        }

        return $docs
    }

    method generate_document_index {} {
        # Generates a index document for the entire documentation.
        #
        # The [generate_document] method must have been called before
        # calling this method.
        #
        # Returns the generated documentation index or an empty string
        # if the formatter does not support indexes.
        return [my DocumentIndex]
    }

    method DocumentIndex {} {
        # Returns a index document for the entire documentation.
        #   references - dictionary keyed by namespace at first level,
        #                and `type` and `ref` keys at second level indicating
        #                type of reference and the reference target
        #                respectively.
        # The default implementation does not support index generation and
        # returns an empty string. Formatters should override if they wish.
        return ""
    }

    method FormatInline {text {scope {}}} {
        # Converts Ruff! inline formatting to the output format.
        #  text - Inline text to convert.
        #  scope - Documentation scope for resolving references.
        # This method should be overridden by the concrete subclass.
        error "Method FormatInline not overridden."
    }

    # Credits: tcllib/Caius markdown module
    # This method is here and not in the Html class because other subclasses
    # (notably markdown) also need to use ruff->html inline conversion.
    method ToHtml {text {scope {}}} {
        set text [regsub -all -lineanchor {[ ]{2,}$} $text <br/>]
        set index 0
        set result {}

        set re_backticks   {\A`+}
        set re_whitespace  {\s}
        set re_inlinelink  {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\]\s*\(\s*((?:[^\s\)]+|\([^\s\)]+\))+)?(\s+([\"'])(.*)?\4)?\s*\)}
        # Changed from markdown to require second optional [] to follow first []
        # without any intervening space. This is to allow consecutive symbol references
        # not to be interpreted as [ref] [text] instead of [ref] [ref]
        # set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\s*\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_htmltag     {\A</?\w+\s*>|\A<\w+(?:\s+\w+=(?:\"[^\"]*\"|\'[^\']*\'))*\s*/?>}
        set re_autolink    {\A<(?:(\S+@\S+)|(\S+://\S+))>}
        set re_comment     {\A<!--.*?-->}
        set re_entity      {\A\&\S+;}

        while {[set chr [string index $text $index]] ne {}} {
            switch $chr {
                "\\" {
                    # ESCAPES
                    set next_chr [string index $text [expr $index + 1]]

                    if {[string first $next_chr {\`*_\{\}[]()#+-.!>|}] != -1} {
                        set chr $next_chr
                        incr index
                    }
                }
                {_} {
                    # Unlike Markdown, do not treat underscores as special char
                }
                {*} {
                    # EMPHASIS
                    if {[regexp $re_whitespace [string index $result end]] &&
                        [regexp $re_whitespace [string index $text [expr $index + 1]]]} \
                        {
                            #do nothing
                        } \
                        elseif {[regexp -start $index \
                                     "\\A(\\$chr{1,3})((?:\[^\\$chr\\\\]|\\\\\\$chr)*)\\1" \
                                     $text m del sub]} \
                        {
                            switch [string length $del] {
                                1 {
                                    append result "<em>[my ToHtml $sub $scope]</em>"
                                }
                                2 {
                                    append result "<strong>[my ToHtml $sub $scope]</strong>"
                                }
                                3 {
                                    append result "<strong><em>[my ToHtml $sub $scope]</em></strong>"
                                }
                            }

                            incr index [string length $m]
                            continue
                        }
                }
                {`} {
                    # CODE
                    regexp -start $index $re_backticks $text backticks
                    set start [expr $index + [string length $backticks]]

                    # Look for the matching backticks. If not found,
                    # we will not treat this as code. Otherwise pass through
                    # the entire match unchanged.
                    if {[regexp -start $start -indices $backticks $text terminating_indices]} {
                        set stop [expr {[lindex $terminating_indices 0] - 1}]

                        set sub [string trim [string range $text $start $stop]]

                        append result "<code>[my Escape $sub]</code>"
                        set index [expr [lindex $terminating_indices 1] + 1]
                        continue
                    }
                }
                {!} -
                "[" {
                    # Note: "[", not {[} because latter messes Emacs indentation
                    # LINKS AND IMAGES
                    if {$chr eq {!}} {
                        set ref_type img
                    } else {
                        set ref_type link
                    }

                    set match_found 0
                    set css ""

                    if {[regexp -start $index $re_inlinelink $text m txt url ign del title]} {
                        # INLINE
                        incr index [string length $m]

                        set url [my Escape [string trim $url {<> }]]
                        set txt [my ToHtml $txt $scope]
                        set title [my ToHtml $title $scope]

                        set match_found 1
                    } elseif {[regexp -start $index $re_reflink $text m txt lbl]} {
                        if {$lbl eq {}} {
                            # Be loose in whitespace
                            set lbl [regsub -all {\s+} $txt { }]
                            set display_text_specified 0
                        } else {
                            set display_text_specified 1
                        }

                        if {[my ResolvableReference? $lbl $scope code_link]} {
                            # RUFF CODE REFERENCE
                            # Bug #42 - do not escape else links for names like '<' do not work
                            if {0} {
                                set url [my Escape [dict get $code_link ref]]
                            } else {
                                set url [dict get $code_link ref]
                            }
                            if {! $display_text_specified} {
                                set txt [my Escape [dict get $code_link label]]
                            }
                            set title $txt
                            switch [dict get $code_link type] {
                                symbol {set css "class='ruff_cmd'"}
                                figure {
                                    # TBD - figure numbering ?
                                }
                                heading {}
                            }
                            incr index [string length $m]
                            set match_found 1
                        } else {
                            app::log_error "Warning: no target found for link \"$lbl\". Assuming markdown reference."
                            set lbl [string tolower $lbl]

                            if {[info exists ::Markdown::_references($lbl)]} {
                                lassign $::Markdown::_references($lbl) url title

                                set url [my Escape [string trim $url {<> }]]
                                set txt [my ToHtml $txt $scope]
                                set title [my ToHtml $title $scope]

                                # REFERENCED
                                incr index [string length $m]
                                set match_found 1
                            }
                        }
                    }
                    # PRINT IMG, A TAG
                    if {$match_found} {
                        if {$ref_type eq {link}} {
                            if {$title ne {}} {
                                append result "<a href=\"$url\" title=\"$title\" $css>$txt</a>"
                            } else {
                                append result "<a href=\"$url\" $css>$txt</a>"
                            }
                        } else {
                            if {$title ne {}} {
                                append result "<img src=\"$url\" alt=\"$txt\" title=\"$title\" $css/>"
                            } else {
                                append result "<img src=\"$url\" alt=\"$txt\" $css/>"
                            }
                        }

                        continue
                    }
                }
                {<} {
                    # HTML TAGS, COMMENTS AND AUTOLINKS
                    if {[regexp -start $index $re_comment $text m]} {
                        append result $m
                        incr index [string length $m]
                        continue
                    } elseif {[regexp -start $index $re_autolink $text m email link]} {
                        if {$link ne {}} {
                            set link [my Escape $link]
                            append result "<a href=\"$link\">$link</a>"
                        } else {
                            set mailto_prefix "mailto:"
                            if {![regexp "^${mailto_prefix}(.*)" $email mailto email]} {
                                # $email does not contain the prefix "mailto:".
                                set mailto "mailto:$email"
                            }
                            append result "<a href=\"$mailto\">$email</a>"
                        }
                        incr index [string length $m]
                        continue
                    } elseif {[regexp -start $index $re_htmltag $text m]} {
                        append result $m
                        incr index [string length $m]
                        continue
                    }

                    set chr [my Escape $chr]
                }
                {&} {
                    # ENTITIES
                    if {[regexp -start $index $re_entity $text m]} {
                        append result $m
                        incr index [string length $m]
                        continue
                    }

                    set chr [my Escape $chr]
                }
                {$} {
                    # Ruff extension - treat $var as variables name
                    # Note: no need to escape characters but do so
                    # if you change the regexp
                    if {[regexp -start $index {\$\w+} $text m]} {
                        append result "<code>$m</code>"
                        incr index [string length $m]
                        continue
                    }
                }
                {>} -
                {'} -
                "\"" {
                    # OTHER SPECIAL CHARACTERS
                    set chr [my Escape $chr]
                }
                default {}
            }
            append result $chr
            incr index
        }
        return $result
    }
}

# Copyright (c) 2019-2022, Ashok P. Nadkarni
# All rights reserved.
# See the file LICENSE in the source root directory for license.

namespace eval ruff::formatter {}

oo::class create ruff::formatter::Html {
    superclass ::ruff::formatter::Formatter

    # Data members
    variable Document;        # Current document
    variable DocumentNamespace; # Namespace being documented
    variable Header;          # Common header
    variable Footer;          # Common footer
    variable NavigationLinks; # Navigation links forming ToC
    variable HeaderLevels;    # Header levels for various headers
    variable CssClasses;      # CSS classes for various elements
    variable GlobalIndex;     # Like NavigationLinks but across *all* documents

    constructor args {
        set HeaderLevels {
            class 3
            proc 3
            method 4
            nonav 5
            parameters 5
        }
        set CssClasses {
            class ruffclass
            proc  ruffproc
            method ruffmethod
        }
        set GlobalIndex [dict create]
        next {*}$args
    }

    method NewSourceId {} {
        # Returns a new id to use for a source listing.
        variable SourceIdCounter
        if {![info exists SourceIdCounter]} {
            set SourceIdCounter 0
        }
        return [incr SourceIdCounter]
    }

    method Anchor args {
        # Construct an anchor from the passed arguments.
        #  args - String from which the anchor is to be constructed.
        # The anchor is formed by joining the passed strings with separators.
        # Empty arguments are ignored.
        # Returns an HTML-escaped anchor without the `#` prefix.
        set parts [lmap arg $args {
            if {$arg eq ""} continue
            my Escape $arg
        }]
        return [join $parts -]
    }

    method HeadingReference {ns heading} {
        # Implements the [Formatter.HeadingReference] method for HTML.
        return "[ns_file_base $ns]#[my Anchor $ns $heading]"
    }

    method FigureReference {ns caption} {
        # Returns a link name to use for a figure
        return "[ns_file_base $ns]#[my Anchor $ns $caption]"
    }


    method SymbolReference {ns symbol} {
        # Implements the [Formatter.SymbolReference] method for HTML.
        set ref [ns_file_base $ns]
        # Reference to the global namespace is to the file itself.
        if {$ns eq "::" && $symbol eq ""} {
            return $ref
        }
        return [append ref "#[my Anchor $symbol]"]
    }

    method Begin {} {
        # Implements the [Formatter.Begin] method for HTML.

        next

        # Generate the header used by all files
        # set Header {<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN">}
        set Header "<!DOCTYPE html>"
        append Header "<html><head><meta charset=\"utf-8\"/>\n"
        # append Header "<link rel='stylesheet' href='https://fonts.googleapis.com/css?family=Open+Sans|Noto+Serif|Droid+Sans+Mono'>"
        set titledesc [my Option -title]
        append Header "<title>$titledesc</title>\n"

        if {[my Option -linkassets 1]} {
            append Header [my LinkAsset ruff-min.css ruff.css]
            append Header [my LinkAsset ruff-min.js ruff.js]
        } else {
            append Header [my GetAsset ruff-min.css ruff.css]
            append Header [my GetAsset ruff-min.js ruff.js]
        }

        append Header "</head>\n"

        # If we are not splitting pages, h1 headings are shown in navigation alongside
        # h2.. headers so the latter need to be offset further.
        if {[my Option -pagesplit none] eq "none"} {
            append Header "<body style='--ruff-nav-toc-offset: 1em;'>\n"
        } else {
            append Header "<body>\n"; # Take default of 0 defined in ruff css
        }
        append Header "<div class='ruff-layout'>\n"

        append Header "<header class='ruff-layout-header ruff-hd'>\n"
        if {$titledesc ne ""} {
            if {[my Option? -version version]} {
                append Header "<a style='text-decoration:none;' href='[my SymbolReference :: {}]'>$titledesc (v$version)</a>\n\n"
            } else {
                append Header "<a style='text-decoration:none;' href='[my SymbolReference :: {}]'>$titledesc</a>\n\n"
            }
        }
        # Theme control button
        append Header {
            <div id="ruffButtonBar">
            <button id="ruffNavMove" onclick="ruffMoveNavPane()"></button>
            <button id="ruffToggleTheme" onclick="ruffNextTheme()"></button>
            </div>
        }
        append Header </header>

        # Generate the Footer used by all files
        append Footer "<footer class='ruff-layout-footer ruff-ft'>"
        append Footer "<div style='float: right;'>Document generated by <a href='https://ruff.magicsplat.com'>Ruff!</a></div>"
        if {[my Option? -copyright copyright]} {
            append Footer "<div>&copy; [my Escape $copyright]</div>"
        }
        append Footer "</footer>\n"

        append Footer "</div></body></html>"

        return
    }

    method DocumentBegin {ns} {
        # See [Formatter.DocumentBegin].
        # ns - Namespace for this document.

        next $ns

        set    NavigationLinks [dict create]
        set    Document $Header
        append Document "<main class='ruff-layout-main ruff-bd'>"
        set    DocumentNamespace $ns

        return
    }

    method DocumentEnd {} {
        # See [Formatter.DocumentEnd].

        # Close off <div class='yui-b'><div id=yui-main> from DocumentBegin
        #append Document "</div></div>"

        append Document "</main>"

        # Add the navigation bits and footer
        my Navigation $DocumentNamespace
        append Document $Footer

        next

        set doc $Document
        set Document ""
        return $doc
    }

    method DocumentIndex {} {
        # See [Formatter.DocumentIndex]
        #   references - namespace keyed nested dictionary
        #
        my DocumentBegin Index

        set entries {}
        dict for {key link} $GlobalIndex {
            lappend entries [dict get $link label] $link
        }
        set entries [lsort -stride 2 -dictionary $entries]

        append Document "<h1 class='ruff'>Index</h1><p>"
        append Document "<div class='ruff_index'>\n"
        append Document {<input style='width:100%;' accesskey='s' type='text' id='filterText' onkeyup='myFilterHook()' placeholder='Enter index term'>}
        append Document {
            <div id='indexStatus'>
            <ul>
            <li>Type the index terms you want to search for in the text input field.
            <li>Matching terms will be shown incrementally as you type.
            <li>Press <kbd>Enter</kbd> to navigate to the target of the first displayed
            index entry.
            <li>Alternatively, <kbd>Tab</kbd> to move to the index entry of interest and then press
            <kbd>Enter</kbd> to navigate to that documentation page.
            <li>To jump to this page from any other documentation page,
            press browser-specific shortcut modifiers with <kbd>i</kbd>.
            For example, on IE and Edge this would be
            <kbd>Alt-i</kbd> while on Firefox and Chrome <kbd>Alt-Shift-i</kbd>.
            Other browsers and platforms may differ.
            </ul>
            </div>
        }
        append Document "\n<ul id='indexUL'>\n"

        foreach {label link} $entries {
            set label [my Escape [string trimleft $label :]]
            # set tag  [dict get $link tag]
            set tag li
            set href [dict get $link href]
            set ns ""
            if {[dict exists $link ns]} {
                set ns [dict get $link ns]
                if {$ns ne ""} {
                    set ns " [my Escape $ns]"
                }
            }
            if {[dict exists $link tip]} {
                append Document "<$tag class='ruff-tip'><a href='$href'>$label</a><span class='ruff-tiptext'>[dict get $link tip]</span>$ns</$tag>"
            } else {
                append Document "<$tag><a href='$href'>$label</a>$ns</$tag>"
            }
        }
        append Document "\n</ul>\n"
        append Document "</div>"
        append Document [my GetAsset ruff-index-min.js ruff-index.js]
        append Document "<script>\nmyIndexInit();</script>\n"

        return [my DocumentEnd]
    }

    method AddProcedureDetail {procinfo} {
        # Adds the detailed information about a procedure or method
        #  procinfo - dictionary describing the procedure. See [AddProcedure]
        #
        #  The concrete implementation can override this.

        if {[my Option -compact 0]} {
            append Document "<details><summary class='ruff-expand'><span>Details</span></summary>\n"
        }
        next $procinfo
        if {[my Option -compact 0]} {
            append Document "</details>\n"
        }
    }

    method AddProgramElementHeading {type fqn {tooltip {}} {synopsis {}}} {
        # Adds heading for a program element like procedure, class or method.
        #  type - One of `proc`, `class` or `method`
        #  fqn - Fully qualified name of element.
        #  tooltip - The tooltip lines, if any, to be displayed in navigation pane.
        #  synopsis - The synopsis to be displayed along with tooltip. Alternating
        #     list of command name and argument list.
        # In addition to adding the heading to the document, a link
        # is also added to the collection of navigation links.

        set level    [dict get $HeaderLevels $type]
        set ns       [namespace qualifiers $fqn]
        set anchor   [my Anchor $fqn]
        set href     [my SymbolReference $ns $fqn]
        set linkinfo [dict create level $level href $href ns $ns]

        # Construct tooltip from synopsis and tooltip
        if {[llength $synopsis]} {
            set tip "<pre>[join [my SynopsisToHtml $synopsis] \n]</pre>"
        }
        if {[llength $tooltip]} {
            append tip "[my ToHtml [string trim [join $tooltip { }]] $ns]\n"
        }
        if {[info exists tip]} {
            dict set linkinfo tip $tip
        }

        set name [namespace tail $fqn]
        dict set linkinfo label $name
        dict set NavigationLinks $anchor [dict create LinkInfo $linkinfo Type $type]
        dict set GlobalIndex $anchor $linkinfo
        if {[string length $ns]} {
            set ns_link [my ToHtml [markup_reference $ns]]
            set heading "<a name='$anchor'>[my Escape $name]</a><span class='ns_scope'> \[${ns_link}\]</span>"
        } else {
            set heading "<a name='$anchor'>[my Escape $fqn]</a>"
        }
        append Document [my HeadingWithUplink $level $heading $ns [dict get $CssClasses $type]]
        return
    }

    method AddHeading {level text scope {tooltip {}}} {
        # See [Formatter.AddHeading].
        #  level   - The numeric or semantic heading level.
        #  text    - The heading text.
        #  scope   - The documentation scope of the content.
        #  tooltip - Tooltip to display in navigation link.

        if {![string is integer -strict $level]} {
            set level [dict get $HeaderLevels $level]
        }

        set do_link [expr {$level >= [dict get $HeaderLevels nonav] ? false : true}]

        if {$do_link} {
            set anchor [my Anchor $scope $text]
            set linkinfo [dict create level $level href "#$anchor"]
            if {$tooltip ne ""} {
                set tip "[my ToHtml [string trim [join $tooltip { }]] $scope]\n"
                dict set linkinfo tip $tip
            }
            dict set linkinfo label $text

            # NOTE: <a></a> empty because the text itself may contain anchors.
            set heading "<a name='$anchor'></a>[my ToHtml $text $scope]"

            # Namespace headers do not get a navigation link if page splitting
            # because they are already highlighted in the namespaces section in
            # navigation. Hack - this assumes level 1 heading is not used within
            # the content.
            if {$level > 1 || [my Option -pagesplit none] eq "none"} {

                dict set NavigationLinks $anchor [dict create LinkInfo $linkinfo Type heading]
            }
        } else {
            set heading [my ToHtml $text $scope]
        }
        append Document [my HeadingWithUplink $level $heading $scope]
        return
    }

    method AddParagraph {lines scope} {
        # See [Formatter.AddParagraph].
        #  lines  - The paragraph lines.
        #  scope - The documentation scope of the content.
        append Document "<p class='ruff'>[my ToHtml [string trim [join $lines { }]] $scope]</p>\n"
        return
    }

    method AddDefinitions {definitions scope {preformatted none}} {
        # See [Formatter.AddDefinitions].
        #  definitions  - List of definitions.
        #  scope        - The documentation scope of the content.
        #  preformatted - One of `none`, `both`, `term` or `definition`
        #                 indicating which fields of the definition are
        #                 are already formatted.
        append Document "<table class='ruff_deflist'>\n"
        foreach item $definitions {
            set def [join [dict get $item definition] " "]
            if {[my Option -autopunctuate 0]} {
                set def [string toupper $def 0 0]
                if {[regexp {[[:alnum:]]} [string index $def end]]} {
                    append def "."
                }
            }
            if {$preformatted in {none term}} {
                set def [my ToHtml $def $scope]
            }
            set term [dict get $item term]
            if {$preformatted in {none definition}} {
                set term [my ToHtml $term $scope]
            }
            append Document "<tr><td>" \
                $term \
                "</td><td>" \
                $def \
                "</td></tr>\n"
        }
        append Document "</table>\n"
        return
    }

    method AddBullets {bullets scope} {
        # See [Formatter.AddBullets].
        #  bullets  - The list of bullets.
        #  scope    - The documentation scope of the content.
        append Document "<ul class='ruff'>\n"
        foreach lines $bullets {
            append Document "<li>[my ToHtml [join $lines { }] $scope]</li>\n"
        }
        append Document "</ul>\n"
        return
    }

    method AddPreformattedText {text scope} {
        # See [Formatter.AddPreformattedText].
        #  text  - Preformatted text.
        #  scope - The documentation scope of the content.
        append Document "<pre class='ruff'>\n" \
            [my Escape $text] \
            "\n</pre>\n"
        return
    }

    method AddFenced {lines fence_options scope} {
        # Adds a list of fenced lines to document content.
        #  lines - Preformatted text as a list of lines.
        #  fence_options - options controlling generation and layout
        #  scope - The documentation scope of the content.

        # See if it is a modifier we specialize, else just pass
        # it to default implementation.

        if {[dict exists $fence_options -caption]} {
            set caption [dict get $fence_options -caption]
            set id "id='[my Anchor $scope $caption]'"
            if {[my ResolvableReference? $caption $scope ref] && [dict exists $ref label]} {
                # May have "Figure X" added
                set display_caption [dict get $ref label]
            } else {
                set display_caption $caption
            }
        } else {
            set caption ""
            set display_caption ""
            set id ""
        }

        set fig_classes ruff-figure
        if {[dict exists $fence_options -align]} {
            append fig_classes " ruff-[dict get $fence_options -align]"
        }
        if {[dict exists $fence_options Command] &&
            [lindex [dict get $fence_options Command] 0] eq "diagram"} {
            set diagrammer [lrange [dict get $fence_options Command] 1 end]
            if {[llength $diagrammer] == 0} {
                set diagrammer [program_option -diagrammer]
            }
            append Document "\n<figure $id class='$fig_classes'>"
            set image_url [ruff::diagram::generate \
                               [join $lines \n] \
                               [ruff::private::sanitize_filename $caption] \
                               {*}$diagrammer]
            append Document "\n<img src='$image_url'></img>"
        } else {
            append Document "\n<figure $id class='ruff-snippet $fig_classes'>"
            append Document [my AddPreformattedText [join $lines \n] $scope]
        }
        if {$display_caption ne ""} {
            append Document "\n<figcaption class='ruff-caption'>$display_caption</figcaption>"
        }
        append Document "\n</figure>"
        return
    }

    method SynopsisToHtml {synopsis} {
        # Returns the a list of HTML lines for a synopsis
        #  synopsis - List of alternating elements comprising the command portion
        #             and the parameter list for it.
        set lines [list ]
        foreach {cmds params} $synopsis {
            set cmds   "<span class='ruff_cmd'>[my Escape [join $cmds { }]]</span>"
            if {[llength $params]} {
                set params "<span class='ruff_arg'>[my Escape [join $params { }]]</span>"
            } else {
                set params ""
            }
            lappend lines "$cmds $params"
        }
        return $lines
    }

    method AddSynopsis {synopsis scope} {
        # Adds a Synopsis section to the document content.
        #  synopsis - List of alternating elements comprising the command portion
        #             and the parameter list for it.
        #  scope  - The documentation scope of the content.

        set lines [my SynopsisToHtml $synopsis]
        append Document "<div class='ruff_synopsis'>[join $lines <br>]</div>\n"
        return
    }

    method AddSource {source scope} {
        # Adds a Source code section to the document content.
        #  source - Source code fragment.
        #  scope  - The documentation scope of the content.
        set src_id [my NewSourceId]
        append Document "<div class='ruff_source'>"
        append Document "<p class='ruff_source_link'>"
        append Document "<a id='l_$src_id' href=\"javascript:toggleSource('$src_id')\">Show source</a>"
        append Document "</p>\n"
        append Document "<div id='$src_id' class='ruff_dyn_src'><pre>[my Escape $source]</pre></div>\n"
        append Document "</div>";    # class='ruff_source'

        return
    }

    method Navigation {{highlight_ns {}}} {
        # Adds the navigation box to the document.
        #  highlight_ns - Namespace to be highlighted in navigation.

        set main_title "Start page"
        set main_ref [ns_file_base {}]
        set index_ref [ns_file_base -docindex]

        set scrolling ""
        foreach opt [my Option -navigation {}] {
            switch -exact -- $opt {
                scrolled { set scrolling "" }
                fixed -
                sticky { set scrolling "style='position: sticky; top: 0;'" }
            }
        }

        append Document "<nav class='ruff-nav'><ul $scrolling>"
        if {[my Option -pagesplit none] ne "none"} {
            # Split pages. Add navigation to each page.
            # If highlight_ns is empty, assume main page. Hack hack hack
            if {$highlight_ns eq ""} {
                append Document "<li class='ruff-toc1'><a class='ruff-highlight' style='padding-top:2px;' href='$main_ref'>$main_title</a></li>\n"
            } else {
                append Document "<li class='ruff-toc1'><a style='padding-top:2px;' href='$main_ref'>$main_title</a></li>\n"
            }
            if {[my Option -makeindex 1]} {
                # Another hack hack - Index page namespaced as Index
                if {$highlight_ns eq "Index"} {
                    append Document "<li class='ruff-toc1'><a class='ruff-highlight' href='$index_ref'>Index</a></li>\n"
                } else {
                    append Document "<li class='ruff-toc1'><a href='$index_ref' accesskey='i'>Index</a></li>\n"
                }
            }
            append Document "<hr>\n"
            if {[my Option -sortnamespaces true]} {
                set ordered_namespaces [my SortedNamespaces]
            } else {
                set ordered_namespaces [my Namespaces]
            }
            foreach ns $ordered_namespaces {
                set ref  [ns_file_base $ns]
                set text [string trimleft $ns :]
                if {$ns eq $highlight_ns} {
                    append Document "<li class='ruff-toc1'><a class='ruff-highlight' href='$ref'>$text</a></li>\n"
                } else {
                    append Document "<li class='ruff-toc1'><a href='$ref'>$text</a></li>\n"
                }
            }
            append Document <hr>
        }

        # Add on the per-namespace navigation links
        if {[dict size $NavigationLinks]} {
            set last_lead_word ""
            dict for {text navinfo} $NavigationLinks {
                set link [dict get $navinfo LinkInfo]
                set label [my Escape [string trimleft [dict get $link label] :]]
                set level  [dict get $link level]
                set href [dict get $link href]
                if {[dict get $navinfo Type] eq "proc"} {
                    catch {
                        set remain [lassign $label lead_word]
                        if {[llength $remain] > 0} {
                            if {$lead_word eq $last_lead_word} {
                                set label "<span style='visibility:hidden'>&nbsp;&nbsp;&nbsp;</span> [join $remain { }]"
                            }
                        }
                        set last_lead_word $lead_word
                    }
                }
                if {[dict exists $link tip]} {
                    append Document "<li class='ruff-toc$level ruff-tip'><a href='$href'>$label</a><span class='ruff-tiptext'>[dict get $link tip]</span></li>"
                } else {
                    append Document "<li class='ruff-toc$level'><a href='$href'>$label</a></li>"
                }
            }
        }
        append Document "</ul></nav>";
        return
    }

    method HeadingWithUplink {level heading scope {cssclass ruff}} {
        # Returns the HTML fragment wrapping the given heading.
        # level - heading level
        # heading - bare HTML fragment to use for heading
        # scope - the namespace scope to be used for uplinks
        #
        # If the heading level is less than 5, links to the namespace
        # and documentation top are inserted into the heading.

        set hlevel "h$level"
        if {$level >= 5} {
            # Plain header
            return "<$hlevel class='$cssclass'>$heading</$hlevel>"
        }

        # If the scope is the document top scope, no need to add uplink since
        # the "Top" link is essentially the same.
        if {$scope ne "" && $scope ne $DocumentNamespace && [my Reference? $scope scope_ref]} {
            set links "<a href='[dict get $scope_ref ref]'>[namespace tail $scope]</a>, "
        }
        if {[my Option -pagesplit none] eq "none"} {
            append links "<a href='#top'>Top</a>"
        } else {
            append links \
                "<a href='#top'>Top</a>, " \
                "<a href='[my SymbolReference :: {}]'>Main</a>"
            if {[my Option -makeindex true]} {
                append links ", <a href='[my SymbolReference -docindex {}]'>Index</a>"
            }
        }
        set links "<span class='ruff-uplink'>$links</span>"
        # NOTE: the div needed to reset the float from links
        return "<$hlevel class='$cssclass'>$heading$links</$hlevel>\n<div style='clear:both;'></div>\n"
    }

    method Escape {s} {
        # Returns an HTML-escaped string.
        #  s - string to be escaped
        # Protects characters in $s against interpretation as
        # HTML special characters.
        #
        # Returns the escaped string

        return [string map {
            &    &amp;
            \"   &quot;
            <    &lt;
            >    &gt;
        } $s]
    }

    method LinkAsset {asset args} {
        # Returns HTML to be included to link to an asset
        #   asset - the name of asset to be included
        #   args - files to check for ensuring $asset is up to date
        #
        set path [file join [ruff_dir] assets $asset]
        foreach arg $args {
            set arg [file join [ruff_dir] assets $arg]
            if {[file exists $arg] && [file mtime $arg] > [file mtime $path]} {
                error "Asset $arg is newer than $path. Regenerate $path."
            }
        }
        if {[file extension $asset] eq ".css"} {
            return "<link rel='stylesheet' type='text/css' href='assets/$asset' />\n"
        } else {
            return "<script type='text/javascript' src='assets/$asset'></script>\n"
        }
    }

    method GetAsset {asset args} {
        # Returns HTML to be included for an asset
        #   asset - the name of asset to be included
        #   args - files to check for ensuring $asset is up to date
        #
        set path [file join [ruff_dir] assets $asset]
        foreach arg $args {
            set arg [file join [ruff_dir] assets $arg]
            if {[file exists $arg] && [file mtime $arg] > [file mtime $path]} {
                error "Asset $arg is newer than $path. Regenerate $path."
            }
        }

        if {[file extension $asset] eq ".css"} {
            return "<style>[read_asset_file $asset utf-8]</style>\n"
        } else {
            return "<script>[read_asset_file $asset utf-8]</script>\n"
        }
    }

    method copy_assets {outdir} {

                #file mkdir [file join $outdir assets]
                #foreach { fn data } {
                #    ruff-min.css "OnJvb3R7LS1ydWZmLWdyaWQtdGVtcGxhdGUtcm93czptaW4tY29udGVudCAxZnI7LS1ydWZmLWdyaWQtdGVtcGxhdGUtY29sdW1uczptaW5tYXgoMjAwcHgsIG1pbi1jb250ZW50KSAxZnI7LS1ydWZmLWdyaWQtdGVtcGxhdGUtYXJlYXM6InRvcGFyZWEgdG9wYXJlYSIgIm5hdmFyZWEgbWFpbmFyZWEiICJib3RhcmVhIGJvdGFyZWEiOy0tcnVmZi10aXAtei1pbmRleDoxMDstLXJ1ZmYtbmF2LXRvYy1vZmZzZXQ6MGVtO3BhZGRpbmctbGVmdDpjYWxjKDEwMHZ3IC0gMTAwJSk7LS1ydWZmLXRoZW1lLWdyYWRpZW50OmxpbmVhci1ncmFkaWVudCg5MGRlZywgbGlnaHRibHVlLCBjb3JhbCwgbGlnaHRncmVlbil9LnJ1ZmYtdGhlbWUtbGlnaHR7LS1ydWZmLWNvbG9yOiM0NDQ7LS1ydWZmLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtbWlub3ItY29sb3I6Izg4ODstLXJ1ZmYtbGF5b3V0LWJhY2tncm91bmQtY29sb3I6I2ZlZmVmZTstLXJ1ZmYtaGQtY29sb3I6IzY2NjstLXJ1ZmYtaGQtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1oZC1mb250OmxhcmdlIGJvbGQ7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiNGRkY1RUE7LS1ydWZmLW5hdi1jb2xvcjojNjY2Oy0tcnVmZi1uYXYtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLW5hdi1oaWdobGlnaHQtYmFja2dyb3VuZC1jb2xvcjpjb3JhbDstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtaC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLWgtY29sb3I6Izk2OEM4MzstLXJ1ZmYtYmQtaDEtY29sb3I6IzY2NjstLXJ1ZmYtYmQtaDEtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtYS1jb2xvcjpibHVlOy0tcnVmZi1iZC1zb3VyY2VsaW5rLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYS1jb2xvcik7LS1ydWZmLWJkLXNvdXJjZWxpbmstYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1zeW5vcHNpcy1ib3JkZXI6bm9uZTstLXJ1ZmYtYmQtdGlwLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1mdC1jb2xvcjp2YXIoLS1ydWZmLWJkLW1pbm9yLWNvbG9yKTstLXJ1ZmYtZnQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1mdC1jb2xvcik7LS1ydWZmLWZ0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1jbG91ZHMsLnJ1ZmYtdGhlbWUtZGFyaywucnVmZi10aGVtZS1saWdodCwucnVmZi10aGVtZS1zb2xhcnstLXJ1ZmYtbmF2LXRpcC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWNvbG9yKTstLXJ1ZmYtYmQtY29sb3I6dmFyKC0tcnVmZi1jb2xvcil9LnJ1ZmYtdGhlbWUtY2xvdWRzey0tcnVmZi1jb2xvcjojMTExOy0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yOndoaXRlc21va2U7LS1ydWZmLW1pbm9yLWNvbG9yOiM4ODg7LS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yOmF6dXJlOy0tcnVmZi1oZC1jb2xvcjojNjY2Oy0tcnVmZi1oZC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWZvbnQ6bGFyZ2UgYm9sZDstLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3I6bGlnaHRibHVlOy0tcnVmZi1uYXYtY29sb3I6IzIxMjEyMTstLXJ1ZmYtbmF2LXRpcC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjp2YXIoLS1ydWZmLWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6IzE0YTdmZjstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6I2NmZWJmNzstLXJ1ZmYtYmQtY29kZS1iYWNrZ3JvdW5kLWNvbG9yOiNjZmZjZmY7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oLWNvbG9yOiM5NjhDODM7LS1ydWZmLWJkLWgxLWNvbG9yOiM2NjY7LS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLWEtY29sb3I6Ymx1ZTstLXJ1ZmYtYmQtc291cmNlbGluay1jb2xvcjp2YXIoLS1ydWZmLWJkLWEtY29sb3IpOy0tcnVmZi1iZC1zb3VyY2VsaW5rLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtc3lub3BzaXMtYm9yZGVyOm5vbmU7LS1ydWZmLWJkLXRpcC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtZnQtY29sb3I6dmFyKC0tcnVmZi1iZC1taW5vci1jb2xvcik7LS1ydWZmLWZ0LW1pbm9yLWNvbG9yOnZhcigtLXJ1ZmYtZnQtY29sb3IpOy0tcnVmZi1mdC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcil9LnJ1ZmYtdGhlbWUtZGFyaywucnVmZi10aGVtZS1zb2xhcnstLXJ1ZmYtbWlub3ItY29sb3I6I2FhYTstLXJ1ZmYtbmF2LWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWNvbG9yOmluaGVyaXR9LnJ1ZmYtdGhlbWUtZGFya3stLXJ1ZmYtY29sb3I6I2RkZDstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjojMjcyNDJjOy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMjEyMTIxOy0tcnVmZi1oZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1oZC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWZvbnQ6bGFyZ2UgYm9sZDstLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3I6IzIyMjcyZTstLXJ1ZmYtbmF2LXRpcC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjojZTZlZmY1Oy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6IzExNjRhMzstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oLWNvbG9yOmxpZ2h0Ymx1ZTstLXJ1ZmYtYmQtaDEtY29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpOy0tcnVmZi1iZC1oMS1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1hLWNvbG9yOiM0ODliZjU7LS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3I6dmFyKC0tcnVmZi1iZC1hLWNvbG9yKTstLXJ1ZmYtYmQtc291cmNlbGluay1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcjpub25lOy0tcnVmZi1iZC10aXAtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2xvcik7LS1ydWZmLWZ0LWNvbG9yOnZhcigtLXJ1ZmYtYmQtbWlub3ItY29sb3IpOy0tcnVmZi1mdC1taW5vci1jb2xvcjp2YXIoLS1ydWZmLWZ0LWNvbG9yKTstLXJ1ZmYtZnQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLXRoZW1lLXNvbGFyey0tcnVmZi1jb2xvcjp3aGl0ZXNtb2tlOy0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yOiMwMTE7LS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yOiMwMDJiMzU7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiMwMDM2NDE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjojMDAzNjQxOy0tcnVmZi1iZC1oLWNvbG9yOmNvcm5zaWxrOy0tcnVmZi1iZC1hLWNvbG9yOnBhbGVncmVlbjstLXJ1ZmYtYmQtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1tYXJvb24sLnJ1ZmYtdGhlbWUtc2xhdGUsLnJ1ZmYtdGhlbWUtc29sYXIsLnJ1ZmYtdGhlbWUtdjF7LS1ydWZmLWhkLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWJhY2tncm91bmQtY29sb3I6aW5oZXJpdDstLXJ1ZmYtaGQtZm9udDpsYXJnZSBib2xkOy0tcnVmZi1uYXYtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpOy0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbGF5b3V0LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1taW5vci1jb2xvcjp2YXIoLS1ydWZmLW1pbm9yLWNvbG9yKTstLXJ1ZmYtYmQtdGFibGUtYm9yZGVyOiM4MDgwODA7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oMS1jb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3I6dmFyKC0tcnVmZi1iZC1hLWNvbG9yKTstLXJ1ZmYtYmQtc291cmNlbGluay1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcjpub25lOy0tcnVmZi1mdC1jb2xvcjp2YXIoLS1ydWZmLWJkLW1pbm9yLWNvbG9yKTstLXJ1ZmYtZnQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1mdC1jb2xvcik7LS1ydWZmLWZ0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1zbGF0ZXstLXJ1ZmYtY29sb3I6I2NjYzstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjojODI5YWIxOy0tcnVmZi1taW5vci1jb2xvcjojYWFhOy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMTgxYTI2Oy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMWEyMDJjOy0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcjpyZ2IoNDUsNTUsNzIpOy0tcnVmZi1uYXYtY29sb3I6dmFyKC0tcnVmZi1jb2xvcik7LS1ydWZmLW5hdi10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLWJkLWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWNvbG9yOiNlY2RiYmE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtaC1jb2xvcjpsaWdodHN0ZWVsYmx1ZTstLXJ1ZmYtYmQtYS1jb2xvcjpsaWdodHNreWJsdWU7LS1ydWZmLWJkLXRpcC1jb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcil9LnJ1ZmYtdGhlbWUtbWFyb29uLC5ydWZmLXRoZW1lLXYxey0tcnVmZi1taW5vci1jb2xvcjojODg4Oy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjp3aGl0ZTstLXJ1ZmYtbmF2LWNvbG9yOndoaXRlOy0tcnVmZi1uYXYtdGlwLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtY29kZS1jb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTstLXJ1ZmYtYmQtaC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yOndoaXRlc21va2V9LnJ1ZmYtdGhlbWUtdjF7LS1ydWZmLWNvbG9yOiMxMjEyMTI7LS1ydWZmLWJhY2tncm91bmQtY29sb3I6d2hpdGU7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiMwMDY2NjY7LS1ydWZmLWJkLWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtYS1jb2xvcjpibHVlfS5ydWZmLXRoZW1lLW1hcm9vbnstLXJ1ZmYtY29sb3I6Izg0NDstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjp3aGl0ZXNtb2tlOy0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcjptYXJvb247LS1ydWZmLWJkLWNvbG9yOiMyMTIxMjE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjojZmZmMGYwOy0tcnVmZi1iZC1hLWNvbG9yOiM0NGZ9I3J1ZmZCdXR0b25CYXJ7ZmxvYXQ6cmlnaHR9I3J1ZmZOYXZNb3ZlLCNydWZmVG9nZ2xlVGhlbWV7aGVpZ2h0OjIwcHg7Ym9yZGVyOjA7Y3Vyc29yOnBvaW50ZXI7dmVydGljYWwtYWxpZ246dGV4dC10b3B9I3J1ZmZUb2dnbGVUaGVtZXtiYWNrZ3JvdW5kLWltYWdlOnZhcigtLXJ1ZmYtdGhlbWUtZ3JhZGllbnQpO3RyYW5zaXRpb246LjI1cztiYWNrZ3JvdW5kLXNpemU6MjAwJSBhdXRvO3dpZHRoOjIwcHh9I3J1ZmZUb2dnbGVUaGVtZTpob3ZlcntiYWNrZ3JvdW5kLXBvc2l0aW9uOnJpZ2h0IGNlbnRlcjt2ZXJ0aWNhbC1hbGlnbjp0ZXh0LXRvcH0jcnVmZk5hdk1vdmV7Y29sb3I6I2FkZDhlNjtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7dGV4dC1hbGlnbjpjZW50ZXJ9Kiw6OmFmdGVyLDo6YmVmb3Jle2JveC1zaXppbmc6Ym9yZGVyLWJveH1ib2R5e2NvbG9yOnZhcigtLXJ1ZmYtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yKX1hLGE6dmlzaXRlZHtjb2xvcjppbmhlcml0O2JhY2tncm91bmQtY29sb3I6aW5oZXJpdH0ucnVmZi1sYXlvdXR7ZGlzcGxheTpncmlkO2dyaWQtdGVtcGxhdGUtcm93czp2YXIoLS1ydWZmLWdyaWQtdGVtcGxhdGUtcm93cyk7Z3JpZC10ZW1wbGF0ZS1jb2x1bW5zOnZhcigtLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zKTtncmlkLXRlbXBsYXRlLWFyZWFzOnZhcigtLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1hcmVhcyk7Y29sdW1uLWdhcDoxcmVtO21pbi1oZWlnaHQ6MTAwdmg7bWF4LXdpZHRoOjYwcmVtO21hcmdpbjowIGF1dG87YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTtwYWRkaW5nOjAgMTBweH0ucnVmZi1sYXlvdXQtaGVhZGVye2dyaWQtYXJlYTp0b3BhcmVhfS5ydWZmLWxheW91dC1tYWlue2dyaWQtYXJlYTptYWluYXJlYX0ucnVmZi1sYXlvdXQtbmF2e2dyaWQtYXJlYTpuYXZhcmVhfS5ydWZmLWxheW91dC1mb290ZXJ7Z3JpZC1hcmVhOmJvdGFyZWF9aDEsaDIsaDMsaDQsaDUsaDZ7bWFyZ2luLWJvdHRvbTouNWVtO21hcmdpbi10b3A6MH1saXttYXJnaW4tdG9wOi41ZW19c3Bhbi5uc19zY29wZXtjb2xvcjp2YXIoLS1ydWZmLW1pbm9yLWNvbG9yKTtmb250LXNpemU6ODUlfXNwYW4ubnNfc2NvcGUgYVtocmVmXTpsaW5rLHNwYW4ubnNfc2NvcGUgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjpub25lO2NvbG9yOnZhcigtLXJ1ZmYtbWlub3ItY29sb3IpfXNwYW4ubnNfc2NvcGUgYVtocmVmXTpob3Zlcnt0ZXh0LWRlY29yYXRpb246dW5kZXJsaW5lfS5ydWZmLXRpcHtwb3NpdGlvbjpyZWxhdGl2ZX0ucnVmZi10aXA6aG92ZXIgLnJ1ZmYtdGlwdGV4dHt2aXNpYmlsaXR5OnZpc2libGV9LnJ1ZmYtdGlwdGV4dCBwcmV7bWFyZ2luLXRvcDowfS5ydWZmLXRpcHRleHR7bWluLXdpZHRoOjIwZW07dGV4dC1hbGlnbjpsZWZ0O2JvcmRlcjowO3Bvc2l0aW9uOmFic29sdXRlO3otaW5kZXg6dmFyKC0tcnVmZi10aXAtei1pbmRleCk7bWFyZ2luLWxlZnQ6NHB4O3BhZGRpbmc6MnB4IDNweDt2aXNpYmlsaXR5OmhpZGRlbn0ucnVmZi1oZHtmb250LWZhbWlseToiVGltZXMgTmV3IFJvbWFuIixzZXJpZjtmb250LXNpemU6MjAwJTtwYWRkaW5nOjVweCAwIDEwcHg7Y29sb3I6dmFyKC0tcnVmZi1oZC1jb2xvcik7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWhkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWZ0e3RleHQtYWxpZ246bGVmdDtib3JkZXItdG9wOjFweCBzb2xpZCB2YXIoLS1ydWZmLWZ0LWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLWZ0LWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtZnQtYmFja2dyb3VuZC1jb2xvcik7bWFyZ2luOjEwcHggMH0ucnVmZi1mdCBkaXZ7cGFkZGluZzo1cHggMH0ucnVmZi1iZCwucnVmZi1uYXZ7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZn0ucnVmZi1uYXZ7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLW5hdi1wYWRkaW5nLXg6NHB4O3BhZGRpbmc6M3B4IHZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCkgMnB4IHZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCk7b3ZlcmZsb3c6dmlzaWJsZTtmb250LXNpemU6ODUlO21hcmdpbjowfS5ydWZmLW5hdiB1bHtsaXN0LXN0eWxlOm5vbmU7bWFyZ2luOjA7cGFkZGluZzowfS5ydWZmLW5hdiBsaSxib2R5e21hcmdpbjowfS5ydWZmLW5hdiAucnVmZi10b2MxLHNwYW4ubnNfc2NvcGV7Zm9udC13ZWlnaHQ6NzAwfS5ydWZmLW5hdiAucnVmZi10b2Mye3BhZGRpbmctbGVmdDpjYWxjKDJlbSArIHZhcigtLXJ1ZmYtbmF2LXRvYy1vZmZzZXQsMCkpO3RleHQtaW5kZW50Oi0yZW19LnJ1ZmYtbmF2IC5ydWZmLXRvYzN7cGFkZGluZy1sZWZ0OmNhbGMoM2VtICsgdmFyKC0tcnVmZi1uYXYtdG9jLW9mZnNldCwwKSk7dGV4dC1pbmRlbnQ6LTJlbX0ucnVmZi1uYXYgLnJ1ZmYtdG9jNHtwYWRkaW5nLWxlZnQ6Y2FsYyg0ZW0gKyB2YXIoLS1ydWZmLW5hdi10b2Mtb2Zmc2V0LDApKTt0ZXh0LWluZGVudDotMmVtfS5ydWZmLW5hdiAucnVmZi10b2M1e3BhZGRpbmctbGVmdDpjYWxjKDVlbSArIHZhcigtLXJ1ZmYtbmF2LXRvYy1vZmZzZXQsMCkpO3RleHQtaW5kZW50Oi0yZW19LnJ1ZmYtbmF2IGhye2NvbG9yOmluaGVyaXQ7bWFyZ2luLXRvcDouMmVtO21hcmdpbi1ib3R0b206LjJlbX0ucnVmZi1uYXYgYTpob3ZlciwucnVmZi1uYXYgYTpsaW5rLC5ydWZmLW5hdiBhOnZpc2l0ZWR7dGV4dC1kZWNvcmF0aW9uOm5vbmU7Y29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpO2JhY2tncm91bmQtY29sb3I6aW5oZXJpdH0ucnVmZi1uYXYgYTpob3Zlcntjb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWNvbG9yKX0ucnVmZi1uYXYgYS5ydWZmLWhpZ2hsaWdodHtjb2xvcjp2YXIoLS1ydWZmLW5hdi1oaWdobGlnaHQtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3IpO21hcmdpbi1sZWZ0OmNhbGMoLTEqdmFyKC0tcnVmZi1uYXYtcGFkZGluZy14KSk7cGFkZGluZy1sZWZ0OnZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCk7cGFkZGluZy1yaWdodDp2YXIoLS1ydWZmLW5hdi1wYWRkaW5nLXgpfS5ydWZmLW5hdiAucnVmZi10aXB0ZXh0e2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtdGlwLWJhY2tncm91bmQtY29sb3IpO2NvbG9yOnZhcigtLXJ1ZmYtbmF2LXRpcC1jb2xvcik7dGV4dC1pbmRlbnQ6MH0ucnVmZi1iZHtjb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7Zm9udC1zaXplOjkzJTtsaW5lLWhlaWdodDoxLjN9LnJ1ZmYtYmQgLnJ1ZmYtdXBsaW5re2ZvbnQtc2l6ZTp4LXNtYWxsO2ZvbnQtdmFyaWFudDpub3JtYWw7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZjtmbG9hdDpyaWdodDtwYWRkaW5nOjJweH0ucnVmZi1iZCAucnVmZi11cGxpbmsgYVtocmVmXSwucnVmZi1iZCAucnVmZi11cGxpbmsgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjp1bmRlcmxpbmU7Y29sb3I6dmFyKC0tcnVmZi1iZC1oLWNvbG9yKX0ucnVmZi1iZCBoMSwucnVmZi1iZCBoMSAucnVmZi11cGxpbmsgYVtocmVmXSwucnVmZi1iZCBoMSAucnVmZi11cGxpbmsgYVtocmVmXTp2aXNpdGVke2NvbG9yOnZhcigtLXJ1ZmYtYmQtaDEtY29sb3IpfS5ydWZmLWJkIHRhYmxlLnJ1ZmZfZGVmbGlzdHttYXJnaW46LjVlbSAxZW0gMWVtO2JvcmRlcjp0aGluIHNvbGlkO2JvcmRlci1jb2xsYXBzZTpjb2xsYXBzZTtib3JkZXItY29sb3I6dmFyKC0tcnVmZi1iZC10YWJsZS1ib3JkZXIpO3BhZGRpbmc6NHB4fS5ydWZmLWJkIC5ydWZmX2RlZmxpc3QgdGQsLnJ1ZmYtYmQgLnJ1ZmZfZGVmbGlzdCB0aHtib3JkZXI6dGhpbiBzb2xpZDtib3JkZXItY29sb3I6Z3JheTtwYWRkaW5nOi4xZW0gLjNlbSAuM2VtfS5ydWZmLWJkIC5ydWZmX2RlZmxpc3QgdGR7dmVydGljYWwtYWxpZ246dG9wO2ZvbnQtc2l6ZTo5MyV9LnJ1ZmYtYmQgLnJ1ZmZfZGVmbGlzdCB0aHtiYWNrZ3JvdW5kLWNvbG9yOiNjY2N9LnJ1ZmYtYmQgaDF7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3IpO3BhZGRpbmctbGVmdDoycHg7bWFyZ2luLWxlZnQ6LTJweH0ucnVmZi1iZCBoMiwucnVmZi1iZCBoMywucnVmZi1iZCBoNCwucnVmZi1iZCBoNSwucnVmZi1iZCBoNntjb2xvcjp2YXIoLS1ydWZmLWJkLWgtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1oLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWJkIGgxLC5ydWZmLWJkIGgye2ZvbnQtZmFtaWx5OiJUaW1lcyBOZXcgUm9tYW4iLHNlcmlmfS5ydWZmLWJkIGgye2ZvbnQtdmFyaWFudDpzbWFsbC1jYXBzfS5ydWZmLWJkIGgzLC5ydWZmLWJkIGg0LC5ydWZmLWJkIGg1LC5ydWZmLWJkIGg2e21hcmdpbi1ib3R0b206LjJlbX0ucnVmZi1iZCBoNXtmb250LXN0eWxlOml0YWxpY30ucnVmZi1iZCBoNSwucnVmZi1iZCBoNntmb250LXdlaWdodDo0MDA7Zm9udC1zaXplOmluaGVyaXR9LnJ1ZmYtYmQgaDMucnVmZmNsYXNzLC5ydWZmLWJkIGgzLnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDMucnVmZnByb2MsLnJ1ZmYtYmQgaDQucnVmZmNsYXNzLC5ydWZmLWJkIGg0LnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDQucnVmZnByb2MsLnJ1ZmYtYmQgaDUucnVmZmNsYXNzLC5ydWZmLWJkIGg1LnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDUucnVmZnByb2N7Ym9yZGVyLWJvdHRvbTp0aGluIHNvbGlkO21hcmdpbi1ib3R0b206LjJlbTttYXJnaW4tdG9wOjJlbX0ucnVmZi1iZCAucnVmZl9jbWQsLnJ1ZmYtYmQgY29kZXtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29kZS1iYWNrZ3JvdW5kLWNvbG9yKTtib3JkZXItcmFkaXVzOjRweDtwYWRkaW5nLWxlZnQ6MnB4O3BhZGRpbmctcmlnaHQ6MnB4fS5ydWZmLWJkIC5ydWZmX3N5bm9wc2lzLC5ydWZmLWJkIHByZXtjb2xvcjp2YXIoLS1ydWZmLWJkLWNvZGUtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWJkIHByZXtwYWRkaW5nOjVweDtmb250LWZhbWlseTpDb25zb2xhcywiQ291cmllciBOZXciLG1vbm9zcGFjZTtmb250LXNpemU6c21hbGxlcjtsaW5lLWhlaWdodDoxLjJlbTt3aGl0ZS1zcGFjZTpwcmUtd3JhcDtvdmVyZmxvdy13cmFwOmJyZWFrLXdvcmQ7ZGlzcGxheTppbmxpbmUtYmxvY2s7dGV4dC1hbGlnbjpsZWZ0fS5ydWZmLWJkIGFbaHJlZl0sLnJ1ZmYtYmQgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjpub25lO2NvbG9yOnZhcigtLXJ1ZmYtYmQtYS1jb2xvcil9LnJ1ZmYtYmQgYVtocmVmXTpob3ZlciwucnVmZi1leHBhbmQ+c3Bhbnt0ZXh0LWRlY29yYXRpb246dW5kZXJsaW5lfS5ydWZmX2R5bl9zcmN7ZGlzcGxheTpub25lfS5ydWZmLWJkIC5ydWZmX3N5bm9wc2lze2JvcmRlcjp2YXIoLS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcik7bWFyZ2luOjAgMmVtIDFlbTtwYWRkaW5nOi41ZW19LnJ1ZmYtYmQgLnJ1ZmZfYXJnLC5ydWZmLWJkIC5ydWZmX2NtZCwucnVmZi1iZCAucnVmZl9jb25zdCwucnVmZi1iZCAucnVmZl9zeW5vcHNpcywucnVmZi1iZCBjb2Rle2ZvbnQtZmFtaWx5OkNvbnNvbGFzLCJDb3VyaWVyIE5ldyIsbW9ub3NwYWNlfS5ydWZmLWJkIC5ydWZmX2FyZ3tmb250LXN0eWxlOml0YWxpYztmb250LXNpemU6c21hbGxlcn0ucnVmZi1iZCAucnVmZl9zb3VyY2VfbGlua3tmb250LXNpemU6c21hbGx9LnJ1ZmYtYmQgLnJ1ZmZfc291cmNlX2xpbmsgYVtocmVmXXtjb2xvcjp2YXIoLS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1zb3VyY2VsaW5rLWJhY2tncm91bmQtY29sb3IpO3RleHQtZGVjb3JhdGlvbjp1bmRlcmxpbmV9LnJ1ZmZfaW5kZXh7Zm9udC1zaXplOnNtYWxsZXJ9LnJ1ZmZfaW5kZXggdWwgbGl7bGlzdC1zdHlsZS10eXBlOm5vbmV9LnJ1ZmZfaW5kZXggdWwgbGkgYXt0ZXh0LWRlY29yYXRpb246bm9uZX0jaW5kZXhVTCwjcnVmZk5hdk1vdmV7bGluZS1oZWlnaHQ6MX0ucnVmZi1iZCAjaW5kZXhVTCAucnVmZi10aXB0ZXh0LC5ydWZmLWJkICNpbmRleFVMIC5ydWZmLXRpcHRleHQgcHJlLC5ydWZmLWJkICNpbmRleFVMIC5ydWZmLXRpcHRleHQgcHJlIC5ydWZmX2FyZywucnVmZi1iZCAjaW5kZXhVTCAucnVmZi10aXB0ZXh0IHByZSAucnVmZl9jbWR7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLWJkLXRpcC1jb2xvcil9LnJ1ZmYtZmlndXJle21hcmdpbjouNWVtIDFlbX0ucnVmZi1zbmlwcGV0e2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWxlZnR7dGV4dC1hbGlnbjpsZWZ0fS5ydWZmLWNlbnRlcnt0ZXh0LWFsaWduOmNlbnRlcn0ucnVmZi1yaWdodHt0ZXh0LWFsaWduOnJpZ2h0fS5ydWZmLWNhcHRpb257Zm9udC1zdHlsZTppdGFsaWM7Zm9udC1zaXplOnNtYWxsZXI7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWZpZ3VyZSBpbWd7bWF4LXdpZHRoOjEwMCU7aGVpZ2h0OmF1dG99LnJ1ZmYtZXhwYW5kPnNwYW57Zm9udC1zaXplOnNtYWxsfXN1bW1hcnkucnVmZi1leHBhbmR7bWFyZ2luLWJvdHRvbToxZW19"
                #    ruff-min.js "ZnVuY3Rpb24gdG9nZ2xlU291cmNlKGlkKXt2YXIgZWxlbTt2YXIgbGluaztpZihkb2N1bWVudC5nZXRFbGVtZW50QnlJZCl7ZWxlbT1kb2N1bWVudC5nZXRFbGVtZW50QnlJZChpZCk7bGluaz1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgibF8iK2lkKX1lbHNlIGlmKGRvY3VtZW50LmFsbCl7ZWxlbT1ldmFsKCJkb2N1bWVudC5hbGwuIitpZCk7bGluaz1ldmFsKCJkb2N1bWVudC5hbGwubF8iK2lkKX1lbHNlIHJldHVybiBmYWxzZTtpZihlbGVtLnN0eWxlLmRpc3BsYXk9PSJibG9jayIpe2VsZW0uc3R5bGUuZGlzcGxheT0ibm9uZSI7bGluay5pbm5lckhUTUw9IlNob3cgc291cmNlIn1lbHNle2VsZW0uc3R5bGUuZGlzcGxheT0iYmxvY2siO2xpbmsuaW5uZXJIVE1MPSJIaWRlIHNvdXJjZSJ9fWZ1bmN0aW9uIHJ1ZmZTZXRUaGVtZSh0aGVtZU5hbWUpe2xvY2FsU3RvcmFnZS5ydWZmX3RoZW1lPXRoZW1lTmFtZTtkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuY2xhc3NOYW1lPSJydWZmLXRoZW1lLSIuY29uY2F0KHRoZW1lTmFtZSl9ZnVuY3Rpb24gcnVmZk5leHRUaGVtZSgpe3RoZW1lTmFtZXM9SlNPTi5wYXJzZShsb2NhbFN0b3JhZ2UucnVmZl90aGVtZXMpO2N1cnJlbnRUaGVtZT1sb2NhbFN0b3JhZ2UucnVmZl90aGVtZTtpZihjdXJyZW50VGhlbWU9PT11bmRlZmluZWQpe3RoZW1lSW5kZXg9MH1lbHNle3RoZW1lSW5kZXg9dGhlbWVOYW1lcy5pbmRleE9mKGN1cnJlbnRUaGVtZSk7Kyt0aGVtZUluZGV4O2lmKHRoZW1lSW5kZXg+PXRoZW1lTmFtZXMubGVuZ3RoKXt0aGVtZUluZGV4PTB9fXJ1ZmZTZXRUaGVtZSh0aGVtZU5hbWVzW3RoZW1lSW5kZXhdKX1mdW5jdGlvbiBydWZmU2V0TmF2U2lkZShuYXZTaWRlKXtsb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZT1uYXZTaWRlO2J1dD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgicnVmZk5hdk1vdmUiKTtpZihuYXZTaWRlPT09InJpZ2h0Iil7Z3JpZEFyZWFzPScidG9wYXJlYSB0b3BhcmVhIiAibWFpbmFyZWEgbmF2YXJlYSIgImJvdGFyZWEgYm90YXJlYSInO2dyaWRDb2xzPSIxZnIgbWlubWF4KDIwMHB4LCBtaW4tY29udGVudCkiO2J1dC50ZXh0Q29udGVudD0iwCI7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdCIsIm5vbmUiKTtidXQuc3R5bGUuc2V0UHJvcGVydHkoImJvcmRlci1yaWdodC1zdHlsZSIsInNvbGlkIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItcmlnaHQtd2lkdGgiLCJ0aGljayIpfWVsc2V7Z3JpZEFyZWFzPScidG9wYXJlYSB0b3BhcmVhIiAibmF2YXJlYSBtYWluYXJlYSIgImJvdGFyZWEgYm90YXJlYSInO2dyaWRDb2xzPSJtaW5tYXgoMjAwcHgsIG1pbi1jb250ZW50KSAxZnIiO2J1dC50ZXh0Q29udGVudD0itiI7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItcmlnaHQiLCJub25lIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdC1zdHlsZSIsInNvbGlkIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdC13aWR0aCIsInRoaWNrIil9ZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LnN0eWxlLnNldFByb3BlcnR5KCItLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1hcmVhcyIsZ3JpZEFyZWFzKTtkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuc3R5bGUuc2V0UHJvcGVydHkoIi0tcnVmZi1ncmlkLXRlbXBsYXRlLWNvbHVtbnMiLGdyaWRDb2xzKX1mdW5jdGlvbiBydWZmTW92ZU5hdlBhbmUoKXtpZihsb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZT09PSJsZWZ0IilydWZmU2V0TmF2U2lkZSgicmlnaHQiKTtlbHNlIHJ1ZmZTZXROYXZTaWRlKCJsZWZ0Iil9KGZ1bmN0aW9uKCl7dGhlbWVOYW1lcz1bInYxIiwibGlnaHQiLCJkYXJrIiwic2xhdGUiLCJzb2xhciIsImNsb3VkcyIsIm1hcm9vbiJdO2xvY2FsU3RvcmFnZS5ydWZmX3RoZW1lcz1KU09OLnN0cmluZ2lmeSh0aGVtZU5hbWVzKTtuYXZTaWRlPWxvY2FsU3RvcmFnZS5ydWZmX25hdl9zaWRlO2lmKG5hdlNpZGUhPT0ibGVmdCImJm5hdlNpZGUhPT0icmlnaHQiKW5hdlNpZGU9ImxlZnQiO3dpbmRvdy5vbmxvYWQ9aW5pdDtmdW5jdGlvbiBpbml0KCl7Y3VycmVudFRoZW1lPWxvY2FsU3RvcmFnZS5ydWZmX3RoZW1lO2lmKGN1cnJlbnRUaGVtZT09PXVuZGVmaW5lZHx8dGhlbWVOYW1lcy5pbmRleE9mKGN1cnJlbnRUaGVtZSk8MCl7Y3VycmVudFRoZW1lPSJ2MSJ9cnVmZlNldFRoZW1lKGN1cnJlbnRUaGVtZSk7bmF2U2lkZT1sb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZTtpZihuYXZTaWRlIT09InJpZ2h0IiluYXZTaWRlPSJsZWZ0IjtydWZmU2V0TmF2U2lkZShuYXZTaWRlKX19KSgpOw=="
                #    ruff-index-min.js "ZnVuY3Rpb24gbXlJbmRleEluaXQoKXt2YXIgc2luZ2xlLG52aXNpYmxlLHVybHRleHQsYTtmaWx0ZXJFbGVtZW50PWRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCJmaWx0ZXJUZXh0Iik7dXJsdGV4dD1teUdldFVybFBhcmFtZXRlcigibG9va3VwIik7aWYodXJsdGV4dD09IiIpe3VybHRleHQ9bXlHZXRVcmxQYXJhbWV0ZXIoInNlYXJjaCIpfWlmKHVybHRleHQhPSIiKXtmaWx0ZXJFbGVtZW50LnZhbHVlPXVybHRleHQ7bXlSdW5GaWx0ZXIoKX1maWx0ZXJFbGVtZW50LmZvY3VzKCl9ZnVuY3Rpb24gbXlHZXRVcmxQYXJhbWV0ZXIobmFtZSl7bmFtZT1uYW1lLnJlcGxhY2UoL1tcW10vLCJcXFsiKS5yZXBsYWNlKC9bXF1dLywiXFxdIik7dmFyIHJlZ2V4PW5ldyBSZWdFeHAoIltcXD8mXSIrbmFtZSsiPShbXiYjXSopIik7dmFyIHJlc3VsdHM9cmVnZXguZXhlYyhsb2NhdGlvbi5zZWFyY2gpO3JldHVybiByZXN1bHRzPT09bnVsbD8iIjpkZWNvZGVVUklDb21wb25lbnQocmVzdWx0c1sxXS5yZXBsYWNlKC9cKy9nLCIgIikpfXZhciBteURlYm91bmNlRGVsYXk7dmFyIG15VXNlckFnZW50PW5hdmlnYXRvci51c2VyQWdlbnQudG9VcHBlckNhc2UoKTtpZihteVVzZXJBZ2VudC5pbmRleE9mKCJNU0lFIikhPS0xfHxteVVzZXJBZ2VudC5pbmRleE9mKCJUUklERU5UIikhPS0xKXtteURlYm91bmNlRGVsYXk9MzAwfWVsc2UgaWYobXlVc2VyQWdlbnQuaW5kZXhPZigiRURHRSIpIT0tMSl7bXlEZWJvdW5jZURlbGF5PTMwMH1lbHNle215RGVib3VuY2VEZWxheT0xMDB9ZnVuY3Rpb24gbXlEZWJvdW5jZShmdW5jLHdhaXQsaW1tZWRpYXRlKXt2YXIgdGltZW91dDtyZXR1cm4gZnVuY3Rpb24oKXt2YXIgY29udGV4dD10aGlzLGFyZ3M9YXJndW1lbnRzO3ZhciBsYXRlcj1mdW5jdGlvbigpe3RpbWVvdXQ9bnVsbDtpZighaW1tZWRpYXRlKWZ1bmMuYXBwbHkoY29udGV4dCxhcmdzKX07dmFyIGNhbGxOb3c9aW1tZWRpYXRlJiYhdGltZW91dDtjbGVhclRpbWVvdXQodGltZW91dCk7dGltZW91dD1zZXRUaW1lb3V0KGxhdGVyLHdhaXQpO2lmKGNhbGxOb3cpZnVuYy5hcHBseShjb250ZXh0LGFyZ3MpfX1mdW5jdGlvbiBteVNldFN0YXR1cyh0ZXh0KXt2YXIgc3RhdHVzO3N0YXR1cz1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiaW5kZXhTdGF0dXMiKTtzdGF0dXMuaW5uZXJUZXh0PXRleHR9ZnVuY3Rpb24gbXlSZXNldFN0YXR1cygpe3ZhciBzdGF0dXM7c3RhdHVzPWRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCJpbmRleFN0YXR1cyIpO3N0YXR1cy5pbm5lclRleHQ9IqAifWZ1bmN0aW9uIG15UnVuRmlsdGVyKCl7dmFyIGlucHV0LGZpbHRlcixmaWx0ZXIwLHVsLGxpLGEsaSx0eHRWYWx1ZSxtYXRjaFNlZW4sZmlyc3RNYXRjaDtpbnB1dD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiZmlsdGVyVGV4dCIpO2ZpbHRlcj1pbnB1dC52YWx1ZS50b1VwcGVyQ2FzZSgpO2ZpbHRlcjA9ZmlsdGVyLmNoYXJBdCgwKTt1bD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiaW5kZXhVTCIpO2xpPXVsLmdldEVsZW1lbnRzQnlUYWdOYW1lKCJsaSIpO2lmKGZpbHRlcj09IiIpe215UmVzZXRTdGF0dXMoKTtmb3IoaT0wO2k8bGkubGVuZ3RoO2krKyl7bGlbaV0uc3R5bGUuZGlzcGxheT0iIn19ZWxzZXttYXRjaFNlZW49MDtmaXJzdE1hdGNoPS0xO2ZvcihpPTA7aTxsaS5sZW5ndGg7aSsrKXthPWxpW2ldLmdldEVsZW1lbnRzQnlUYWdOYW1lKCJhIilbMF07dHh0VmFsdWU9YS50ZXh0Q29udGVudHx8YS5pbm5lclRleHQ7dHh0VmFsdWU9dHh0VmFsdWUudG9VcHBlckNhc2UoKTtpZih0eHRWYWx1ZS5jaGFyQXQoMCk9PWZpbHRlcjAmJnR4dFZhbHVlLnN0YXJ0c1dpdGgoZmlsdGVyKSl7aWYobXlMYXN0S2V5PT0xMyl7bXlSZXNldFN0YXR1cygpO2RvY3VtZW50LmxvY2F0aW9uLmhyZWY9YS5ocmVmO3JldHVybn1lbHNle2xpW2ldLnN0eWxlLmRpc3BsYXk9IiI7bWF0Y2hTZWVuKys7aWYoZmlyc3RNYXRjaD09LTEpZmlyc3RNYXRjaD1pfX1lbHNle2lmKG1hdGNoU2Vlbil7YnJlYWt9bGlbaV0uc3R5bGUuZGlzcGxheT0ibm9uZSJ9fWZvcig7aTxsaS5sZW5ndGg7aSsrKXtsaVtpXS5zdHlsZS5kaXNwbGF5PSJub25lIn1teVJlc2V0U3RhdHVzKCk7aWYobWF0Y2hTZWVuPT0xKXt9fX12YXIgbXlGaWx0ZXJGdW5jdGlvbj1teURlYm91bmNlKG15UnVuRmlsdGVyLG15RGVib3VuY2VEZWxheSk7dmFyIG15TGFzdEtleT0wO2Z1bmN0aW9uIG15RmlsdGVySG9vaygpe215TGFzdEtleT1ldmVudC5rZXlDb2RlO215U2V0U3RhdHVzKCJTZWFyY2hpbmcuLi4iKTtteUZpbHRlckZ1bmN0aW9uKCl9"
                #} {
                #    set fp [open [file join $outdir assets $fn] w]
                #    fconfigure $fp -encoding binary -translation binary
                #    puts -nonewline $fp [binary decode base64 $data]
                #    close $fp
                #}
            
    }

    method extension {} {
        # Returns the default file extension to be used for output files.
        return html
    }

    forward FormatInline my ToHtml
}

# Copyright (c) 2019, Ashok P. Nadkarni
# All rights reserved.
# See the file LICENSE in the source root directory for license.

# Ruff! formatter for markdown
# For compiling generated markdown to html using pandoc:
#   pandoc -s -o ruff.html -c ../ruff-md.css --metadata pagetitle="My package" ruff.md
# For compiling generated markdown to manpages using pandoc
#   pandoc ruff_ruff.md -s -t man -o ruff.man
#   dos2unix ruff.man
#   tbl ruff.man | man -l -

namespace eval ruff::formatter {}

oo::class create ruff::formatter::Markdown {
    superclass ::ruff::formatter::Formatter

    # Data members
    variable Document;        # Current document
    variable DocumentNamespace; # Namespace being documented
    variable Header;          # Common header
    variable Footer;          # Common footer
    variable HeaderLevels;    # Header levels for various headers

    # NOTE: NavigationLinks are currently recorded but not used since
    # there is no standard way to have a navigation pane or ToC in
    # Markdown without resorting to HTML.
    variable NavigationLinks; # Navigation links forming ToC

    constructor args {
        set HeaderLevels {
            class 3
            proc 4
            method 4
            nonav 5
            parameters 5
        }
        next {*}$args
    }

    method Anchor args {
        # Construct an anchor from the passed arguments.
        #  args - String from which the anchor is to be constructed.
        # The anchor is formed by joining the passed strings with separators.
        # Empty arguments are ignored.
        # Returns an HTML-escaped anchor without the `#` prefix.
        set parts [lmap arg $args {
            if {$arg eq ""} continue
            set arg
        }]

        return [regsub -all {[^-:\w_.]} [join $parts -] _]
    }

    method HeadingReference {ns heading} {
        # Implements the [Formatter.HeadingReference] method for Markdown.
        return "[ns_file_base $ns .html]#[my Anchor $ns $heading]"
    }

    method SymbolReference {ns symbol} {
        # Implements the [Formatter.SymbolReference] method for Markdown.
        set ref [ns_file_base $ns .html]
        # Reference to the global namespace is to the file itself.
        if {$ns eq "::" && $symbol eq ""} {
            return $ref
        }
        return [append ref "#[my Anchor $symbol]"]
    }

    method FigureReference {ns caption} {
        # Implements the [Formatter.FigureReference] method for Markdown.
        return "[ns_file_base $ns .html]#[my Anchor $ns $caption]"
    }

    method Begin {} {
        # Implements the [Formatter.Begin] method for HTML.

        next

        # Generate the header used by all files
        # Currently, it is empty but might change in the future with
        # support for specific dialects which implement metainformation.
        set Header ""
        set titledesc [my Option -title]

        # Generate the Footer used by all files
        set Footer ""
        if {[my Option? -copyright copyright]} {
            append Footer "\n\n---\n\\(c) [my Escape $copyright]\n"
        }
        return
    }

    method DocumentBegin {ns} {
        # See [Formatter.DocumentBegin].
        # ns - Namespace for this document.

        next $ns

        set    NavigationLinks [dict create]
        set    Document $Header
        set    DocumentNamespace $ns

        return
    }

    method DocumentEnd {} {
        # See [Formatter.DocumentEnd].

        # Add the navigation bits and footer
        my Navigation $DocumentNamespace
        append Document $Footer

        set doc $Document
        set Document ""

        next

        return $doc
    }

    method AddProgramElementHeading {type fqn {tooltip {}} {synopsis {}}} {
        # Adds heading for a program element like procedure, class or method.
        #  type - One of `proc`, `class` or `method`
        #  fqn - Fully qualified name of element.
        #  tooltip - The tooltip lines, if any, to be displayed in the navigation pane.
        # In addition to adding the heading to the document, a link
        # is also added to the collection of navigation links.

        set level    [dict get $HeaderLevels $type]
        set atx      [string repeat # $level]
        set ns       [namespace qualifiers $fqn]
        set anchor   [my Anchor $fqn]
        set linkinfo [dict create tag h$level href "#$anchor"]
        if {[llength $tooltip]} {
            set tip "[my ToMarkdown [string trim [join $tooltip { }]] $ns]\n"
            dict set linkinfo tip $tip
        }
        set name [namespace tail $fqn]
        dict set linkinfo label $name
        dict set NavigationLinks $anchor $linkinfo
        append Document "\n$atx <a name='$anchor'></a>"
        if {[string length $ns]} {
            set ns_link [my ToMarkdown [markup_reference $ns]]
            append Document \
                [my Escape [namespace tail $name]] \
                " \[${ns_link}\]\n"
        } else {
            append Document [my Escape $name] "\n"
        }
        return
    }

    method AddHeading {level text scope {tooltip {}}} {
        # See [Formatter.AddHeading].
        #  level   - The numeric or semantic heading level.
        #  text    - The heading text.
        #  scope   - The documentation scope of the content.
        #  tooltip - Tooltip to display in navigation link.

        if {![string is integer -strict $level]} {
            set level [dict get $HeaderLevels $level]
        }
        set do_link [expr {$level >= [dict get $HeaderLevels nonav] ? false : true}]
        set atx [string repeat # $level]

        if {$do_link} {
            set anchor [my Anchor $scope $text]
            set linkinfo [dict create tag h$level href "#$anchor"]
            if {$tooltip ne ""} {
                set tip "[my ToMarkdown [join $tooltip { }] $scope]\n"
                dict set linkinfo tip $tip
            }
            dict set linkinfo label $text
            dict set NavigationLinks $anchor $linkinfo
            # NOTE: <a></a> empty because the text itself may contain anchors.
            set heading "<a name='$anchor'></a>[my ToMarkdown $text $scope]"
        } else {
            set heading [my ToMarkdown $text $scope]
        }
        append Document "\n" $atx " " $heading "\n"
        return
    }

    method AddParagraph {lines scope} {
        # See [Formatter.AddParagraph].
        #  lines  - The paragraph lines.
        #  scope - The documentation scope of the content.
        append Document "\n" [my ToMarkdown [join $lines \n] $scope] "\n"
        return
    }

    method AddDefinitions {definitions scope {preformatted none}} {
        # See [Formatter.AddDefinitions].
        #  definitions  - List of definitions.
        #  scope        - The documentation scope of the content.
        #  preformatted - One of `none`, `both`, `term` or `definition`
        #                 indicating which fields of the definition are
        #                 are already formatted.

        if {0} {
            # This does not escape <> properly. Moreover, cmark seems
            # to handle `` within html tags differently depending on whether
            # the tag is (e.g.) <b> or <td>
            append Document "\n<table><tbody>\n"
            foreach item $definitions {
                set def [join [dict get $item definition] " "]
                # Note: since we are generating raw HTML here, we have to
                # use ToHtml and not ToMarkdown here. Huh? TBD
                if {$preformatted in {none term}} {
                    set def [my ToMarkdown $def $scope]
                }
                set term [dict get $item term]
                if {$preformatted in {none definition}} {
                    set term [my ToMarkdown $term $scope]
                }
                append Document "<tr><td>" \
                    $term \
                    "</td><td>" \
                    $def \
                    "</td></tr>\n"
            }
            append Document "</tbody></table>\n"
        } else {
            # Note: CommonMark does not recognize tables without a heading line
            # TBD - how do empty headers look in generated HTML?
            append Document "\n|||\n|----|----|\n"
            foreach item $definitions {
                set def [join [dict get $item definition] " "]
                if {[my Option -autopunctuate 0]} {
                    set def [string toupper $def 0 0]
                    if {[regexp {[[:alnum:]]} [string index $def end]]} {
                        append def "."
                    }
                }
                if {$preformatted in {none term}} {
                    set def [my ToMarkdown $def $scope]
                }
                set term [dict get $item term]
                if {$preformatted in {none definition}} {
                    set term [my ToMarkdown $term $scope]
                }
                append Document "|$term|$def|\n"
            }
            append Document "\n"
        }
        return
    }

    method AddBullets {bullets scope} {
        # See [Formatter.AddBullets].
        #  bullets  - The list of bullets.
        #  scope    - The documentation scope of the content.
        append Document "\n"
        foreach lines $bullets {
            append Document "- [my ToMarkdown [join $lines { }] $scope]\n"
        }
        append Document "\n"
        return
    }

    method AddPreformattedText {text scope} {
        # See [Formatter.AddPreformattedText].
        #  text  - Preformatted text.
        #  scope - The documentation scope of the content.
        append Document "\n```\n$text\n```\n"
        return
    }

    method AddFenced {lines fence_options scope} {
        # See [Formatter.AddFenced].
        # Adds a list of fenced lines to document content.
        #  lines - Preformatted text as a list of lines.
        #  fence_options - options specified with the fence, e.g. diagram ...
        #  scope - The documentation scope of the content.
        # Only obeys -caption option, ignores all else

        # Do not hardcode fence since the lines may themself contain something
        # that looks like a fence.
        set fence [dict get $fence_options Fence]
        append Document \n $fence \n [join $lines \n] \n $fence \n
        if {[dict exists $fence_options -caption]} {
            append Document \n\n* [dict get $fence_options -caption] *\n\n
        }

        return
    }

    method AddSynopsis {synopsis scope} {
        # Adds a Synopsis section to the document content.
        #  synopsis - List of alternating elements comprising the command portion
        #             and the parameter list for it.
        #  scope  - The documentation scope of the content.

        append Document \n
        foreach {cmds params} $synopsis {
            append Document "\n> `[join $cmds { }]` *`[join $params { }]`*<br>"
        }
        append Document \n
        return
    }

    method Navigation {{highlight_ns {}}} {
        # TBD - right now, no navigation for markdown.
        return
    }

    method Escape {s} {
        # Escapes special characters in markdown.
        #  s - string to be escaped
        # Protects characters in $s against interpretation as
        # markdown special characters.
        #
        # Returns the escaped string

        # TBD - fix this regexp
        return [regsub -all {[\\`*_\{\}\[\]\(\)#\+\-\.!<>|]} $s {\\\0}]
    }

    # Credits: tcllib/Caius markdown module
    method ToMarkdown {text {scope {}}} {
        # Returns $text marked up in markdown syntax
        #  text - Ruff! text with inline markup
        #  scope - namespace scope to use for symbol lookup

        # We cannot just pass through our marked-up text as is because
        # it is not really markdown but rather with some extensions:
        # - [xxx] treats xxx as potentially a link to documentation for
        # some programming element.
        # - _ is not treated as a special char
        # - $var is marked as a variable name
        # Moreover, we cannot use a simple regexp or subst because
        # whether this special processing will depend on where inside
        # the input these characters occur, whether a \ preceded etc.

        set text [regsub -all -lineanchor {[ ]{2,}$} $text <br/>]

        set index 0
        set result {}

        set re_backticks   {\A`+}
        set re_whitespace  {\s}
        set re_inlinelink  {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\]\s*\(\s*((?:[^\s\)]+|\([^\s\)]+\))+)?(\s+([\"'])(.*)?\4)?\s*\)}
        # Changed from markdown to require second optional [] to follow first []
        # without any intervening space. This is to allow consecutive symbol references
        # not to be interpreted as [ref] [text] instead of [ref] [ref]
        # set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\s*\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_htmltag     {\A</?\w+\s*>|\A<\w+(?:\s+\w+=(?:\"[^\"]*\"|\'[^\']*\'))*\s*/?>}
        set re_autolink    {\A<(?:(\S+@\S+)|(\S+://\S+))>}
        set re_comment     {\A<!--.*?-->}
        set re_entity      {\A\&\S+;}

        while {[set chr [string index $text $index]] ne {}} {
            switch $chr {
                "\\" {
                    # If the next character is a special markdown character
                    # that we do not treat as special, it should be treated
                    # as a backslash-prefixed ordinary character.
                    # So double the backslash and prefix the character.
                    set next_chr [string index $text [expr $index + 1]]
                    if {$next_chr eq "_"} {
                        append result "\\\\\\_"
                        incr index; # Move past \_
                        continue
                    }
                    # Other characters, special or not, are treated just
                    # like markdown would so pass through as is at bottom
                    # of loop.
                }
                {_} {
                    # Unlike Markdown, do not treat underscores as special char
                    append result \\; # Add an escape prefix
                    # $chr == _ will be added at bottom of loop
                }
                {*} {
                    # EMPHASIS
                    if {[regexp $re_whitespace [string index $result end]] &&
                        [regexp $re_whitespace [string index $text [expr $index + 1]]]} \
                        {
                            #do nothing (add character at bottom of loop)
                        } \
                        elseif {[regexp -start $index \
                                     "\\A(\\$chr{1,3})((?:\[^\\$chr\\\\]|\\\\\\$chr)*)\\1" \
                                     $text m del sub]} \
                        {
                            append result "$del[my ToMarkdown $sub $scope]$del"
                            incr index [string length $m]
                            continue
                        }
                }
                {`} {
                    # CODE
                    # Any marked code should not be escaped as above so
                    # look for it and pass it through as is.

                    # Collect the leading backtick sequence
                    regexp -start $index $re_backticks $text backticks
                    set start [expr $index + [string length $backticks]]

                    # Look for the matching backticks. If not found,
                    # we will not treat this as code. Otherwise pass through
                    # the entire match unchanged.
                    if {[regexp -start $start -indices $backticks $text terminating_indices]} {
                        set stop [lindex $terminating_indices 1]
                        # Copy the entire substring including leading and trailing
                        # backticks to output as is as we do not want those
                        # characters to undergo the special processing above.
                        set passthru [string range $text $index $stop]
                        append result $passthru
                        incr index [string length $passthru]
                        continue
                    }
                }
                {!} -
                {[} {
                    # LINKS AND IMAGES
                    if {$chr eq {!}} {
                        set ref_type img
                        set pre "!\["
                    } else {
                        set ref_type link
                        set pre "\["
                    }

                    set match_found 0
                    if {[regexp -start $index $re_inlinelink $text m txt url ign del title]} {
                        # INLINE
                        if {1} {
                            append result $m
                            set match_found 1
                        } else {
                            # Note: Do quotes inside $title need to be escaped?
                            append result $pre [my ToMarkdown $txt $scope] "\](" $url " " "\"[my ToMarkdown $title $scope]\"" ")"
                            set url [escape [string trim $url {<> }]]
                            set match_found 1
                        }
                    } elseif {[regexp -start $index $re_reflink $text m txt lbl]} {
                        if {$lbl eq {}} {
                            set lbl [regsub -all {\s+} $txt { }]
                            set display_text_specified 0
                        } else {
                            set display_text_specified 1
                        }

                        if {[my ResolvableReference? $lbl $scope code_link]} {
                            # RUFF CODE REFERENCE
                            set url [my Escape [dict get $code_link ref]]
                            if {! $display_text_specified} {
                                set txt [my Escape [dict get $code_link label]]
                            }
                            if {1} {
                                append result $pre $txt "\](" $url ")"
                            } else {
                                # Note: Do quotes inside $txt (SECOND occurence) need to be escaped?
                                append result $pre $txt "\](" $url " " "\"$txt\"" ")"
                            }
                            set match_found 1
                        } else {
                            # Not a Ruff! code link. Pass through as is.
                            # We do not pass text through ToMarkdown as it is
                            # treated as a markdown reference and will need
                            # to match the reference entry.
                            app::log_error "Warning: no target found for link \"$lbl\". Assuming markdown reference."
                            append result $m
                            set match_found 1
                        }
                    }
                    # PRINT IMG, A TAG
                    if {$match_found} {
                        incr index [string length $m]
                        continue
                    }
                }
                    {<} {
                        # HTML TAGS, COMMENTS AND AUTOLINKS
                        # HTML tags, pass through as is without processing
                        if {[regexp -start $index $re_comment $text m] ||
                            [regexp -start $index $re_autolink $text m email link] ||
                            [regexp -start $index $re_htmltag $text m]} {
                            append result $m
                            incr index [string length $m]
                            continue
                        }
                        # Else fall through to pass only the < character
                    }
                    {&} {
                        # ENTITIES
                        # Pass through entire entity without processing
                        if {[regexp -start $index $re_entity $text m]} {
                            append result $m
                            incr index [string length $m]
                            continue
                        }
                        # Else fall through to processing this single &
                    }
                    {$} {
                        # Ruff extension - treat $var as variables name
                        # Note: no need to escape characters but do so
                        # if you change the regexp below
                        if {[regexp -start $index {\$\w+} $text m]} {
                            append result "`$m`"
                            incr index [string length $m]
                            continue
                        }
                    }
                    {>} -
                    {'} -
                    "\"" {
                        # OTHER SPECIAL CHARACTERS
                        # Pass through
                    }
                    default {}
                }

                append result $chr
                incr index
            }

            return $result
        }

        method extension {} {
            # Returns the default file extension to be used for output files.
            return md
        }

        forward FormatInline my ToMarkdown
    }

# Copyright (c) 2021, Ashok P. Nadkarni
# All rights reserved.
# See the file LICENSE in the source root directory for license.

# Ruff! formatter for nroff

namespace eval ruff::formatter {}

oo::class create ruff::formatter::Nroff {
    superclass ::ruff::formatter::Formatter

    # Data members
    variable DocumentNamespace; # Namespace being documented
    variable Header;          # Common header for all pages
    variable PageTitle;       # Page title - first line of man page
    variable Synopsis;        # Holds the synopsis
    variable Body;            # Main content
    variable Footer;          # Common footer
    variable HeaderLevels;    # Header levels for various headers
    variable SeeAlso;         # The See also section

    variable Indentation; # How much to indent in nroff units

    constructor args {
        namespace path [linsert [namespace path] 0 ::ruff::formatter::nroff]
        set HeaderLevels {
            class 3
            proc 4
            method 4
            nonav 5
            parameters 5
        }
        set Indentation 4n
        next {*}$args
    }

    method CollectReferences args {}
    method CollectHeadingReference args {}
    method CollectFigureReference args {}
    export CollectFigureReference

    method Begin {} {
        # Implements the [Formatter.Begin] method for nroff.

        next

        # Generate the header used by all files
        # Currently, it is empty but might change in the future with
        # support for specific dialects which implement metainformation.
        set Header ""

        append Header [nr_comment "\n"]
        if {[my Option? -copyright copyright]} {
            append Header [nr_comment "Copyright (c) [my Escape $copyright]\n"]
        }

        # Generate the Footer used by all files
        set Footer ""
        return
    }

    method DocumentBegin {ns} {
        # See [Formatter.DocumentBegin].
        # ns - Namespace for this document.
        set ns [string trimleft $ns :]

        next $ns

        set DocumentNamespace $ns
        set Body ""
        set Synopsis ""
        set SeeAlso ""

        return
    }

    method DocumentEnd {} {
        # See [Formatter.DocumentEnd].

        set title [my Option -title]
        set section [my Option -section 3tcl]
        set version [my Option -version 0.0]
        set product [my Option -product $DocumentNamespace]
        if {$DocumentNamespace eq ""} {
            set header_left $product
        } else {
            set header_left $DocumentNamespace
        }
        set PageTitle [nr_title "\"$header_left\" $section $version \"$product\" \"$title\""]

        # Add the navigation bits and footer
        append doc $Header $PageTitle
        append doc [nr_section NAME] \n
        if {$DocumentNamespace eq ""} {
            append doc "Introduction - $title"
        } else {
            append doc "$DocumentNamespace - Commands in namespace $DocumentNamespace"
        }
        if {$Synopsis ne ""} {
            append doc [nr_section SYNOPSIS] \n $Synopsis
        }
        append doc $Body
        if {$SeeAlso ne ""} {
            append doc [nr_section "SEE ALSO"] $SeeAlso
        }
        append doc $Footer
        set doc [nroff_postprocess $doc]

        next

        return $doc
    }

    method AddProgramElementHeading {type fqn {tooltip {}} {synopsis {}}} {
        # Adds heading for a program element like procedure, class or method.
        #  type - One of `proc`, `class` or `method`
        #  fqn - Fully qualified name of element.
        #  tooltip - The tooltip lines, if any. Ignore for nroff output.

        set level [dict get $HeaderLevels $type]
        set ns    [namespace qualifiers $fqn]
        set name  [namespace tail $fqn]
        if {[string length $ns]} {
            append Body [nr_p] [nr_inn -$Indentation] \n [nr_bldr [namespace tail $name]] " ($ns)"
        } else {
            append Body [nr_p] [nr_inn -$Indentation] \n [nr_bldr $name]
        }
        append Body [nr_out]
        return
    }

    method AddHeading {level text scope {tooltip {}}} {
        # See [Formatter.AddHeading].
        #  level   - The numeric or semantic heading level.
        #  text    - The heading text.
        #  scope   - The documentation scope of the content.
        #  tooltip - Tooltip to display in navigation link.

        if {![string is integer -strict $level]} {
            set level [dict get $HeaderLevels $level]
        }

        # TBD - should $text really be passed through ToNroff? In particular do
        # commands like .SH accept embedded escapes ?
        set text [my ToNroff $text $scope]
        if {$level < 3} {
            append Body [nr_section $text]
        } elseif {$level == 3} {
            append Body [nr_subsection $text]
        } else {
            append Body [nr_p] [nr_bldr $text]
        }
        return
    }

    method AddParagraph {lines scope} {
        # See [Formatter.AddParagraph].
        #  lines  - The paragraph lines.
        #  scope - The documentation scope of the content.
        append Body [nr_p] [my ToNroff [join $lines \n] $scope]
        return
    }

    method AddDefinitions {definitions scope {preformatted none}} {
        # See [Formatter.AddDefinitions].
        #  definitions  - List of definitions.
        #  scope        - The documentation scope of the content.
        #  preformatted - One of `none`, `both`, `term` or `definition`
        #                 indicating which fields of the definition are
        #                 are already formatted.

        # Note: CommonMark does not recognize tables without a heading line
        # TBD - how do empty headers look in generated HTML?
        set autopunctuate [my Option -autopunctuate 0]
        append Body [nr_inn $Indentation]
        foreach item $definitions {
            set def [join [dict get $item definition] " "]
            if {$autopunctuate} {
                set def [string toupper $def 0 0]
                if {[regexp {[[:alnum:]]} [string index $def end]]} {
                    append def "."
                }
            }
            if {$preformatted in {none term}} {
                set def [my ToNroff $def $scope]
            }
            set term [dict get $item term]
            if {$preformatted in {none definition}} {
                set term [my ToNroff $term $scope]
            }
            append Body [nr_blt $term] "\n" $def
        }
        append Body [nr_out]

        return
    }

    method AddBullets {bullets scope} {
        # See [Formatter.AddBullets].
        #  bullets  - The list of bullets.
        #  scope    - The documentation scope of the content.
        foreach lines $bullets {
            append Body [nr_blt "\n\1\\(bu"] "\n" [my ToNroff [join $lines { }] $scope]
        }
        return
    }

    method AddPreformattedText {text scope} {
        # See [Formatter.AddPreformattedText].
        #  text  - Preformatted text.
        #  scope - The documentation scope of the content.

        append Body [nr_p] [nr_inn $Indentation] [nr_nofill]  \n
        append Body $text
        append Body [nr_fill] [nr_out]
        return
    }

    method AddFenced {lines fence_options scope} {
        # See [Formatter.AddFenced].
        # Adds a list of fenced lines to document content.
        #  lines - Preformatted text as a list of lines.
        #  fence_options - options specified with the fence, e.g. diagram ...
        #  scope - The documentation scope of the content.
        # Only obeys -caption option, ignores all else
        append Body [nr_p] [nr_inn $Indentation] [nr_nofill]  \n
        append Body [join $lines \n]
        if {[dict exists $fence_options -caption]} {
            append Body \n\n [nr_ulp [dict get $fence_options -caption]] \n
        }
        append Body [nr_fill] [nr_out]
        return
    }

    method AddSynopsis {synopsis scope} {
        # Adds a Synopsis section to the document content.
        #  synopsis - List of alternating elements comprising the command portion
        #             and the parameter list for it.
        #  scope  - The documentation scope of the content.

        append Body [nr_inn $Indentation]; # Indent the synopsis
        foreach {cmds params} $synopsis {
            set line "[nr_bldp [join $cmds { }]]"
            if {[llength $params]} {
                append line " " [nr_ulp [join $params { }]] 
            }
            append Synopsis $line [nr_br]
            append Body $line [nr_br]
        }
        append Body [nr_out] \n
        return
    }

    method Navigation {{highlight_ns {}}} {
        # TBD - right now, no navigation for markdown.
        return
    }

    method Escape {s} {
        # Escapes special characters in nroff.
        #  s - string to be escaped
        # Protects characters in $s against interpretation as
        # nroff special characters.
        #
        # Returns the escaped string

        # TBD - fix this?
        return [string map [list \\ \\\\] $s]
    }

    # Credits: tcllib/Caius markdown module
    method ToNroff {text {scope {}}} {
        # Returns $text marked up in nroff syntax
        #  text - Ruff! text with inline markup
        #  scope - namespace scope to use for symbol lookup

        # Passed in text is kinda markdown but with some extensions:
        # - [xxx] treats xxx as potentially a link to documentation for
        # some programming element.
        # - _ is not treated as a special char
        # - $var is marked as a variable name
        # Moreover, we cannot use a simple regexp or subst because
        # whether this special processing will depend on where inside
        # the input these characters occur, whether a \ preceded etc.

        set text [regsub -all -lineanchor {[ ]{2,}$} $text [nr_br]]

        set index 0
        set result {}

        set re_backticks   {\A`+}
        set re_whitespace  {\s}
        set re_inlinelink  {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\]\s*\(\s*((?:[^\s\)]+|\([^\s\)]+\))+)?(\s+([\"'])(.*)?\4)?\s*\)}
        # Changed from markdown to require second optional [] to follow first []
        # without any intervening space. This is to allow consecutive symbol references
        # not to be interpreted as [ref] [text] instead of [ref] [ref]
        # set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\s*\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_reflink     {\A\!?\[((?:[^\]]|\[[^\]]*?\])+)\](?:\[((?:[^\]]|\[[^\]]*?\])*)\])?}
        set re_htmltag     {\A</?\w+\s*>|\A<\w+(?:\s+\w+=(?:\"[^\"]*\"|\'[^\']*\'))*\s*/?>}
        set re_autolink    {\A<(?:(\S+@\S+)|(\S+://\S+))>}
        set re_comment     {\A<!--.*?-->}
        set re_entity      {\A\&\S+;}

        while {[set chr [string index $text $index]] ne {}} {
            switch $chr {
                "\\" {
                    # If next character is a special markdown char, set that as the
                    # the character. Otherwise just pass this \ as the character.
                    set next_chr [string index $text [expr $index + 1]]
                    if {[string first $next_chr {\`*_\{\}[]()#+-.!>|}] != -1} {
                        set chr $next_chr
                        incr index
                    }
                }
                {_} {
                    # Unlike Markdown, underscores are not treated as special char
                }
                {*} {
                    # EMPHASIS
                    if {[regexp $re_whitespace [string index $result end]] &&
                        [regexp $re_whitespace [string index $text [expr $index + 1]]]} \
                        {
                            #do nothing (add character at bottom of loop)
                        } elseif {[regexp -start $index \
                                       "\\A(\\$chr{1,3})((?:\[^\\$chr\\\\]|\\\\\\$chr)*)\\1" \
                                     $text m del sub]} {
                            switch [string length $del] {
                                1 {
                                    # * - Emphasis
                                    append result "[nr_ul][my ToNroff $sub $scope][nr_fpop]"
                                }
                                2 {
                                    # ** - Strong
                                    append result "[nr_bld][my ToNroff $sub $scope][nr_fpop]"
                                }
                                3 {
                                    # *** - Strong+emphasis - no way I think. Make bold
                                    append result "[nr_bld][my ToNroff $sub $scope][nr_fpop]"
                                }
                            }

                            incr index [string length $m]
                            continue
                    }
                }
                {`} {
                    # CODE
                    # Any marked code should not be escaped as above so
                    # look for it and pass it through as is.
                    # TBD - anything needed to pass text verbatim?

                    # Collect the leading backtick sequence
                    regexp -start $index $re_backticks $text backticks
                    set start [expr $index + [string length $backticks]]

                    # Look for the matching backticks. If not found,
                    # we will not treat this as code. Otherwise pass through
                    # the entire match unchanged.
                    if {[regexp -start $start -indices $backticks $text terminating_indices]} {
                        set stop [expr {[lindex $terminating_indices 0] - 1}]

                        set sub [string trim [string range $text $start $stop]]

                        append result "[my Escape $sub]"
                        set index [expr [lindex $terminating_indices 1] + 1]
                        continue
                    }
                }
                {!} -
                "\[" {
                    # LINKS AND IMAGES
                    if {$chr eq {!}} {
                        set ref_type img
                        set pre "!\["
                    } else {
                        set ref_type link
                        set pre "\["
                    }

                    set match_found 0
                    if {[regexp -start $index $re_inlinelink $text m txt url ign del title]} {
                        # INLINE
                        incr index [string length $m]

                        set url [my Escape [string trim $url {<> }]]
                        set txt [my ToNroff $txt $scope]
                        set title [my ToNroff $title $scope]

                        set match_found 1
                    } elseif {[regexp -start $index $re_reflink $text m txt lbl]} {
                        if {$lbl eq {}} {
                            # Be loose in whitespace
                            set lbl [regsub -all {\s+} $txt { }]
                            set display_text_specified 0
                        } else {
                            set display_text_specified 1
                        }

                        set code_link ""
                        if {[my ResolvableReference? $lbl $scope code_link]} {
                            # RUFF CODE REFERENCE
                            set url [my Escape [dict get $code_link ref]]
                        } else {
                            set url ""
                        }
                        if {! $display_text_specified && $code_link ne ""} {
                            set txt [my Escape [dict get $code_link label]]
                        }
                        set title $txt
                        incr index [string length $m]
                        set match_found 1
                    }
                    # PRINT IMG, A TAG
                    if {$match_found} {
                        if {$ref_type eq {link}} {
                            # TBD - some nroff version support urls using .UR
                            append result [nr_ulr $txt]
                            if {$url ne ""} {
                                append result " \[URL: $url\]"
                            }
                        } else {
                            app::log_error "Warning: Image URL $url found. Images are not supported for Nroff output."
                            append result $txt " \[Image: $url\]"
                        }

                        continue
                    }
                }
                {<} {
                    # HTML TAGS, COMMENTS AND AUTOLINKS
                    # HTML tags, pass through as is without processing

                    if {[regexp -start $index $re_comment $text m]} {
                        append result [nr_comment [string range $m 4 end-3]]
                        incr index [string length $m]
                        continue
                    } elseif {[regexp -start $index $re_autolink $text m email link]} {
                        if {$link ne {}} {
                            set link [my Escape $link]
                            append result " \[URL: $link\]"
                        } else {
                            set mailto_prefix "mailto:"
                            if {![regexp "^${mailto_prefix}(.*)" $email mailto email]} {
                                # $email does not contain the prefix "mailto:".
                                set mailto "mailto:$email"
                            }
                            append result "<a href=\"$mailto\">$email</a>"
                            append result " \[$mailto\]"
                        }
                        incr index [string length $m]
                        continue
                    } elseif {[regexp -start $index $re_htmltag $text m]} {
                        app::log_error "Warning: HTML tag $m skipped. HTML tags not supported by Nroff formatter."
                        incr index [string length $m]
                        continue
                    }
                    # Else fall through to pass only the < character
                }
                {&} {
                    # ENTITIES
                    # Pass through entire entity without processing
                    # TBD - add support for more entities
                    if {[regexp -start $index $re_entity $text m]} {
                        set html_mapping [list "&quot;" \" "&apos;" ' "&amp;" & "&lt;" <  "&gt;" >]
                        append result [string map $html_mapping $m]
                        incr index [string length $m]
                        continue
                    }
                    # Else fall through to processing this single &
                }
                {$} {
                    # Ruff extension - treat $var as variables name
                    # Note: no need to escape characters but do so
                    # if you change the regexp below
                    if {[regexp -start $index {\$\w+} $text m]} {
                        append result [nr_ulr $m]
                        incr index [string length $m]
                        continue
                    }
                }
                {>} -
                {'} -
                "\"" {
                    # OTHER SPECIAL CHARACTERS
                    # Pass through
                }
                default {}
            }

            append result $chr
            incr index
        }

        return $result
    }

    method extension {} {
        # Returns the default file extension to be used for output files.
        return 3tcl
    }

    forward FormatInline my ToNroff
}

# MODFIED/ADAPTED From tcllib - BSD license.]
namespace eval ruff::formatter::nroff {
    # -*- tcl -*-
    #
    # -- nroff commands
    #
    # Copyright (c) 2003-2019 Andreas Kupries <andreas_kupries@sourceforge.net>

    ################################################################
    # nroff specific commands
    #
    # All dot-commands (f.e. .PP) are returned with a leading \n\1,
    # enforcing that they are on a new line and will be protected as markup.
    # Any empty line created because of this is filtered out in the 
    # post-processing step.


    proc nr_lp      {}          {return \n\1.LP}
    proc nr_ta      {{text {}}} {return "\n\1.ta$text"}
    proc nr_bld     {}          {return \1\\fB}
    proc nr_bldt    {t}         {return "\n\1.B $t\n"}
    proc nr_bldr    {t}         {return \1\\fB$t[nr_rst]}
    proc nr_bldp    {t}         {return \1\\fB$t[nr_fpop]}
    proc nr_ul      {}          {return \1\\fI}
    proc nr_ulr     {t}         {return \1\\fI$t[nr_fpop]}
    proc nr_ulp     {t}         {return \1\\fI$t[nr_fpop]}
    proc nr_rst     {}          {return \1\\fR}
    proc nr_fpop    {}          {return \1\\fP}
    proc nr_p       {}          {return \n\1.PP\n}
    proc nr_comment {text}      {return "\1'\1\\\" [join [split $text \n] "\n\1'\1\\\" "]"} ; # "
    proc nr_enum    {num}       {nr_item " \[$num\]"}
    proc nr_item    {{text {}}} {return "\n\1.IP$text"}
    proc nr_vspace  {}          {return \n\1.sp\n}
    proc nr_br      {}          {return \n\1.br\n}
    proc nr_blt     {text}      {return "\n\1.TP\n$text"}
    proc nr_bltn    {n text}    {return "\n\1.TP $n\n$text"}
    proc nr_in      {}          {return \n\1.RS}
    proc nr_inn     {n}         {return "\n\1.RS $n"}
    proc nr_out     {}          {return \n\1.RE}
    proc nr_nofill  {}          {return \n\1.nf}
    proc nr_fill    {}          {return \n\1.fi}
    proc nr_title   {text}      {return "\n\1.TH $text"}
    proc nr_include {file}      {return "\n\1.so $file"}
    proc nr_bolds   {}          {return \n\1.BS}
    proc nr_bolde   {}          {return \n\1.BE}
    proc nr_read    {fn}        {return [nroffMarkup [dt_read $fn]]}
    proc nr_cs      {}          {return \n\1.CS\n}
    proc nr_ce      {}          {return \n\1.CE\n}

    proc nr_section {name} {
        if {![regexp {[ 	]} $name]} {
            return "\n\1.SH [string toupper $name]"
        }
        return "\n\1.SH \"[string toupper $name]\""
    }
    proc nr_subsection {name}   {
        if {![regexp {[ 	]} $name]} {
            return "\n\1.SS [string toupper $name]"
        }
        return "\n\1.SS \"[string toupper $name]\""
    }


    ################################################################

    # Handling of nroff special characters in content:
    #
    # Plain text is initially passed through unescaped;
    # internally-generated markup is protected by preceding it with \1.
    # The final PostProcess step strips the escape character from
    # real markup and replaces unadorned special characters in content
    # with proper escapes.
    #

    variable   markupMap
    set      markupMap [list \
                            "\\"   "\1\\" \
                            "'"    "\1'" \
                            "."    "\1." \
                            "\\\\" "\\"]
    variable   finalMap
    set      finalMap [list \
                           "\1\\" "\\" \
                           "\1'"  "'" \
                           "\1."  "." \
                           "."    "\\&." \
                           "\\"   "\\\\"]
    variable   textMap
    set      textMap [list "\\" "\\\\"]


    proc nroffEscape {text} {
        variable textMap
        return [string map $textMap $text]
    }

    # markup text --
    #	Protect markup characters in $text.
    #	These will be stripped out in PostProcess.
    #
    proc nroffMarkup {text} {
        variable markupMap
        return [string map $markupMap $text]
    }

    proc nroff_postprocess {nroff} {
        variable finalMap

        # Postprocessing final nroff text.
        # - Strip empty lines out of the text
        # - Remove leading and trailing whitespace from lines.
        # - Exceptions to the above: Keep empty lines and leading
        #   whitespace when in verbatim sections (no-fill-mode)

        set nfMode   [list \1.nf \1.CS]	; # commands which start no-fill mode
        set fiMode   [list \1.fi \1.CE]	; # commands which terminate no-fill mode
        set lines    [list]         ; # Result buffer
        set verbatim 0              ; # Automaton mode/state

        foreach line [split $nroff "\n"] {
            #puts_stderr |[expr {$verbatim ? "VERB" : "    "}]|$line|

            if {!$verbatim} {
                # Normal lines, not in no-fill mode.

                if {[lsearch -exact $nfMode [split $line]] >= 0} {
                    # no-fill mode starts after this line.
                    set verbatim 1
                }

                # Ensure that empty lines are not added.
                # This also removes leading and trailing whitespace.

                if {![string length $line]} {continue}
                set line [string trim $line]
                if {![string length $line]} {continue}

                if {[regexp {^\x1\\f[BI]\.} $line]} {
                    # We found confusing formatting at the beginning of
                    # the current line. We lift this line up and attach it
                    # at the end of the last line to remove this
                    # irregularity. Note that the regexp has to look for
                    # the special 0x01 character as well to be sure that
                    # the sequence in question truly is formatting.
                    # [bug-3601370] Only lift & attach if last line is not
                    # a directive

                    set last  [lindex   $lines end]
                    if { ! [string match "\1.*" $last] } {
                        #puts_stderr \tLIFT
                        set lines [lreplace $lines end end]
                        set line "$last $line"
                    }
                } elseif {[string match {[']*} $line]} {
                    # Apostrophes at the beginning of a line have to be
                    # quoted to prevent misinterpretation as comments.
                    # The true comments and are quoted with \1 already and
                    # will therefore not detected by the code here.
                    # puts_stderr \tQUOTE
                    set line \1\\$line
                } ; # We are not handling dots at the beginning of a line here.
                #   # We are handling them in the finalMap which will quote _all_
                #   # dots in a text with a zero-width escape (\&).
            } else {
                # No-fill mode. We remove trailing whitespace, but keep
                # leading whitespace and empty lines.

                if {[lsearch -exact $fiMode [split $line]] >= 0} {
                    # Normal mode resumes after this line.
                    set verbatim 0
                }
                set line [string trimright $line]
            }
            lappend lines $line
        }

        set lines [join $lines "\n"]

        # Now remove all superfluous .IP commands (empty paragraphs). The
        # first identity mapping is present to avoid smashing a man macro
        # definition.

        lappend map	\n\1.IP\n\1.\1.\n  \n\1.IP\n\1.\1.\n
        lappend map \n\1.IP\n\1.       \n\1.

        set lines [string map $map $lines]

        # Return the modified result buffer
        return [string trim [string map $finalMap $lines]]\n
    }
}

# Copyright (c) 2009-2019, Ashok P. Nadkarni
# All rights reserved.
# See the file LICENSE in the source root directory for license.

# Ruff! - RUntime Formatting Function
# ...a document generator using introspection
#

package require Tcl 8.6
if {[catch {
    package require textutil::adjust
    package require textutil::tabify
} msg ropts]} {
    puts stderr "Ruff! needs packages textutil::adjust and textutil::tabify from tcllib."
    return -options $ropts $msg
}
package require msgcat
msgcat::mcload [file join [file dirname [info script]] msgs]

namespace eval ruff {
    # If you change version here, change in pkgIndex.tcl as well
    variable version 2.3.0
    proc version {} {
        # Returns the Ruff! version.
        variable version
        return $version
    }

    variable _ruff_intro {
        # Introduction

        Ruff! (Runtime function formatter) is a documentation generation system
        for programs written in the Tcl programming language. Ruff! uses runtime
        introspection in conjunction with comment analysis to generate reference
        manuals for Tcl programs.

        Ruff! is covered by a liberal BSD open-source license that permits use
        for any purpose.

        ## Why Ruff!

        In comparison with other source code based documentation generators,
        Ruff! produces documentation that not only requires less duplication
        of effort from the programmer, but is also more complete, more
        accurate and more maintainable.

        * Comments in source code do not have to be
        reproduced for documentation purposes.

        * Ruff! requires minimal markup in the comments making it lightweight
        as well as reducing clutter.

        * Supports inline formatting using Markdown syntax.

        * Embedded diagrams in multiple formats

        * Program elements like command arguments, defaults and
        class relationships like inheritance are automatically derived.

        * Maintenance is less of a burden as documentation is automatically
        updated with source modification such as changes to defaults, addition of
        mix-ins etc.

        On the output side,

        * Ruff! supports multiple formats (currently HTML, Markdown and nroff).

        * Generated documentation can optionally be split across multiple pages.

        * Hyperlinks between program elements, and optionally source code,
        make navigation easy and efficient.

        * A table of contents and optional searchable index permits quick
        location of command and class documentation.

        * Class relationships are extracted
        and the full API for a class, with inherited and mixed-in methods, is
        flattened and summarized.

        * The HTML formatter includes multiple themes switchable by the end-user.

        The Ruff! documentation itself is produced with Ruff!. Examples of other
        packages documented with Ruff! include
        [iocp](https://iocp.magicsplat.com),
        [cffi](https://cffi.magicsplat.com),
        [CAWT](http://www.cawt.tcl3d.org/download/CawtReference.html),
        [apave](https://aplsimple.github.io/en/tcl/pave/apave.html),
        [baltip](https://aplsimple.github.io/en/tcl/baltip/baltip.html),
        [hl-tcl](https://aplsimple.github.io/en/tcl/hl_tcl/hl_tcl.html),
        [promise](https://tcl-promise.magicsplat.com),
        [obex](https://tcl-obex.magicsplat.com),
        [Woof!](http://woof.sourceforge.net/woof-ug-0.5/html/_woof/woof_manual.html)
        and
        [tcl-vix](https://tcl-vix.magicsplat.com/).

        ## Documentation

        The [::ruff] reference page describes the Ruff! documentation generation
        API. The [::ruff::sample] page shows some sample output for some of the
        Ruff! features along with the associated source code from which
        it was generated.

        ## Downloads

        Download the Ruff! distribution from
        <https://sourceforge.net/projects/magicsplat/files/ruff/>. The
        source code repository is at <https://github.com/apnadkarni/ruff>.

        ## Installation

        To install, extract the distribution to a directory listed in your
        Tcl `auto_path` variable.

        ## Credits

        Ruff! is authored by [Ashok P. Nadkarni](https://www.magicsplat.com).

        It uses the `textutil` package from
        [tcllib](https://core.tcl-lang.org/tcllib), a modified version of the
        Markdown inline parser from the
        [Caius](http://caiusproject.com/) project, and portions of the
        `nroff` generator from Tcllib's `doctools` package.
    }

    variable _ruff_preamble {

        ## Usage

        ### Usage from a script

        To document a package or packages, first load them into a Tcl
        interpreter. Then load `ruff` and invoke the [document] command to
        document classes and commands within one or more namespaces.

        For example, the following command will document the `NS` namespace using
        the built-in HTML formatter.
        ````
        package require ruff
        ::ruff::document ::NS
        ````
        The output will be written to `NS.html`. The [document] command takes
        a number of options which control what is documented, output formats,
        layouts etc.

        For example, the following will document the namespace `NS`, `NS2` and
        their children, splitting the output across multiple pages.

        ````
        ::ruff::document {::NS ::NS2} -outdir /path/to/docdir -recurse true -pagesplit namespace
        ````

        ### Usage from the command line

        For simpler cases, documentation can also be generated from the command
        line by invoking the `ruff.tcl` script. Assuming the `NS` and `NS2`
        namespaces were implemented by the `mypac` package,

        ````
        tclsh /path/to/ruff.tcl "::NS ::NS2" -preeval "package require mypac" \
            -outfile docs.html -recurse true -pagesplit none
        ````

        All arguments passed to the script are passed to the [document]
        command. The `-preeval` option is required to load the packages being
        documented, generally using the `package require` or `source`
        commands.

        ## Documenting procedures

        Ruff! generates documentation using Tcl's runtime system to gather
        proc and class definitions. Comments in procedure and method
        bodies are further parsed to extract the documentation.

        The structure Ruff! expects is described below. In practice,
        the structure is simple and intuitive though the description may be
        a bit long winded. You can simply look at the documentation
        of the [sample] namespace instead, and click on the **Show source**
        links for each procedure or method there to see the formatting.

        An example procedure may look as follows:
        ```
        proc ruff::sample::character_at {text {pos 0}} {
            # Get the character from a string.
            #  text - Text string.
            #  pos  - Character position. 
            # The command will treat negative values of $pos as offset from
            # the end of the string.
            #
            # Returns the character at index $pos in string $text.
            set n [string length $text]
            if {[tcl::mathfunc::abs $pos] >= [string length $text]} {
                #ruff
                # An error exception is raised if $pos is not within bounds.
                error "Index $pos out of bounds."
            }
            if {$pos < 0} {
                return [string index $text end$pos]
            } else {
                return [string index $text $pos]
            }
        }
        ```
        You can see the generated documentation for the above at 
        [sample::character_at].

        The first block of comments within a procedure *before
        the first line of code* are always processed by Ruff!. Note preceding
        blank lines are OK. We will refer to this block as the lead comment
        block. It is terminated by either a line of code or a blank line.

        Any comments appearing after the first line of code are not
        processed by Ruff! unless immediately preceded by a line beginning
        with `#ruff` which indicates the start of another Ruff! comment
        block.

        The lead comment block begins with a summary that will be used anywhere
        the document output inserts a procedure summary, for example, a tooltip.
        The summary is terminated with a blank comment or by the parameter
        block.

        The parameter block is a definition list (see below) and follows its
        syntactic structure. It only differs from definition lists in that
        it must directly follow the summary line and receives special
        treatment in that the default value, if any for the argument, is
        automatically inserted by Ruff!. Options and switches may also be
        documented here. The parameter block
        is terminated in the same fashion as definition blocks.

        Any blocks following the parameter block, whether part of the lead
        block or appearing in a subsequent comment block marked with a
        leading `#ruff`, are processed as follows.

        * All processed lines are stripped of the leading `#` character and a
        single following space if there is one.

        * A blank line (after the comment character is stripped) ends the
        previous block. Note in the case of lists, it ends the list element
        but not the list itself.

        * A line containing 3 or more consecutive backquote (\`) characters with
        only leading whitespace starts a fenced block. The block is terminated
        by the same sequence of backquotes. By default, all intervening lines
        are passed through to the output unchanged. However, fenced blocks may
        undergo specialized processing. See [Fenced blocks].

        * Lines starting with a `-` or a `*` character followed by at least one
        space begins a bulleted list item block. A list item may be continued
        across multiple lines and is terminated by another list item, a blank
        line or a line with lesser indentation. Note in particular that lines of
        other types will not terminate a list item unless they have less
        indentation.

        * Lines containing a `-` surrounded by whitespace begins a definition
        list element. The text before the `-` separator is the definition term
        and the text after is the description. Both the term and description are
        subject to inline formatting. Definition blocks follow the same rules
        for termination as bullet lists described above.

        * Parameter blocks have the same format as definition lists and are
        distinguished from them only by their presence in the lead block. Unlike
        definition blocks, the term is assumed to be the name of an argument and
        is automatically formatted and not subject to inline formatting.

        * If the line is indented 4 or more spaces, it is treated a
        preformatted line and passed through to the output with the
        the first 4 spaces stripped. No other processing is done on the line.

        * Any line beginning with the word `Returns` is treated as description
        of the return value. It follows the same rules as normal paragraphs
        below with one special case: if the `Returns` is followed by a colon,
        the word `Returns` is not treated as part of the text to be output. Only
        the rest of the text, which must be separated from the colon by at least
        one space, is treated as the paragraph content. The `Returns` is then
        treated only as a marker for the `Returns` section. This is primarily
        to aid in non-English documentation.

        * A line beginning with `See also:` (note the colon) is assumed to begin
        a reference block consisting of a list of program element names
        (such as procedures, classes etc.) and Markdown links. These
        are then automatically linked and listed in the **See also** section of a
        procedure documentation. The list may continue over multiple lines
        following normal paragraph rules. Each line must be parsable as a Tcl list.
        Note the program element names can,
        but need not be, explicitly marked as a program element reference
        using surrounding square brackets. For example, within a `See also:`
        section, both `document` and `[document]` will generate a cross-reference
        link to the documentation for the `document` procedure.

        * A line beginning with `Synopsis:` (note the colon) is assumed to be
        the parameter list in the synopsis to be documented for the procedure or
        method in lieu of the generated argument list. There may be multiple such
        synopses defined. Each synopsis may continue over multiple lines
        following normal paragraph rules. Each synopsis line must be parsable as
        a Tcl list. See the example at [sample::proc_with_custom_synopsis]. A
        custom synopsis is useful when a command takes several different
        argument list forms. The Tcl `socket` command is an example of this.

        * All other lines begin a normal paragraph. The paragraph ends with
        a line of one of the above types.

        ### Differences from Markdown

        Note that the block level parsing is similar but not identical to
        Markdown. Amongst other differences, Ruff! has

        * no nested blocks
        * no numbered lists or multi-paragraph list elements
        * no blockquotes

        Ruff! adds
        * definition lists
        * specialized processing for fenced blocks with diagramming
        support, captions and alignment

        ## Documenting classes

        Documentation for classes primarily concerns documentation of its methods.
        The format for method documentation is exactly as described above for
        procedures. Information about class relationships is automatically
        collected and need not be explicitly provided. Note that unlike for
        procedures and methods, Tcl does not provide a means to retrieve the
        body of the class so that comments can be extracted from them. Thus
        to document information about the class as a whole, you can either
        include it in the comments for the constructor, which is often a
        reasonable place for such information, or include it in the general
        information section as described in the next section.

        Classes created from user-defined metaclasses are also included
        in the generated documentation.

        ## Documenting namespaces

        In addition to procedures and classes within a namespace, there may be a
        need to document general information such as the sections you are
        currently reading. For this purpose, Ruff! looks for a variable
        `_ruff_preamble` within each namespace. The indentation of the first
        line of section content is stripped off from all subsequent lines before
        processing (This impacts what constitutes a preformatted line). The
        result is then processed in the same manner as procedure or method
        bodies except for the following differences:

        * There is (obviously) no summary or parameter block.

        * Additionally, content may contain Markdown ATX style
        headings indicated by a prefix of one or more `#` characters followed
        by at least one space.

        The documentation generated from the `_ruff_preamble` content is placed
        before the documentation of the commands in classes for that namespace.

        **Note**: Older versions supported the `_ruffdoc` variable. Though this
        will still work, it is deprecated.

        Content that should lie outside of any namespace can be passed through
        the `-preamble` option to [document]. When generating single page
        output, this is included at the top of the documentation. When
        generating multipage output this forms the content of the main
        documentation page.

        ## Inline formatting

        Once documentation blocks are parsed as above, their content is subject
        to inline formatting rules using Markdown syntax with some minor
        extensions. Markdown compatibility is only for inline elements noted
        below.

        \`   - `Text surrounded by backquotes is formatted as inline code`.
        `*`    - *Text surrounded by single asterisks is emphasized*.
        `**`   - **Text surrounded by double asterisks is bolded**.
        `***`  - ***Text surrounded by triple asterisks is bold emphasized***.
        `[]`   - Text surrounded by square brackets is treated as a link
        (more below).
        `<>`   - Text in angle brackets are treated as HTML tags and
        auto-links as in Markdown.
        `$`    - Words beginning with `$` are treated as variable names and
        shown as inline code similar to backquotes (non-standard Markdown).

        The default HTML formatter supports other Markdown inline elements
        but other formatters might not.

        Text enclosed in `[]` is checked whether it references a section heading
        or a program element name (namespaces, classes, methods, procedures). If

        so, it is replaced by a link to the section or documentation of that
        element. If the text is not a fully qualified name, it is treated
        relative to the namespace or class within whose documentation the link
        appears. If it is fully qualified, it is displayed relative to the
        namespace of the link location. For example,

        * `[document]` is displayed as [document]
        * `[::ruff::formatters]` is displayed as [::ruff::formatters] if
        referenced from within a section documenting the `::ruff` namespace.

        Alternatively, text different from the section heading or symbol
        can be shown by putting it in another `[]` pair immediately bfore
        the symbol or heading reference.
        For example, `[here][document]` will show as [here][document] and
        link to `document` as before.
        *Note: unlike Markdown, there must be no whitespace between the
        two pairs of `[]` else it will be treated as two separate symbol
        references. This is intentional.*

        If the text does not match a section heading or program element name, it
        is treated as a normal Markdown reference but a warning is emitted.

        ## Fenced blocks

        A line containing 3 or more consecutive backquote (\`) characters with
        only leading whitespace starts a fenced block. The block is terminated
        by the same sequence of backquotes. By default, formatters will pass
        all intervening lines through verbatim to the output. 

        However, the leading line of a fenced block can contain
        additional options for specialized processing. The general form
        of a fenced block is

        ````
        ``` ?option value...? ?transform arg...?
        some text
        lines
        ```
        ````

        The supported options are

        `-align ALIGNMENT` - Aligns the output as per `ALIGNMENT` which may
        be specified as `left`, `right` or `center`.
        `-caption CAPTION` - Adds a caption below the output.

        In addition, a transform can be specified which transforms
        the input lines into some other form as opposed to outputting them
        without modification. The only transform currently implemented is
        `diagram` and is described in [Embedding diagrams].

        Formatters that do not support the options or the transforms
        will silently ignore them and do the default processing on the
        block.

        The fenced block below illustrates use of the options.

        ````
        ``` -align center -caption "An example"
        This is a
        center-aligned
        fenced block
        with a caption
        ```
        ````

        This produces

        ``` -align center -caption "An example"
        This is a
        center-aligned
        fenced block
        with a caption
        ```

        The `-caption` option is optional. If specified, it is shown
        below the output and can be linked to using the value of the option.
        For example `[An example]` will link as [An example].


        ## Embedding diagrams

        Diagrams can be embedded in multiple textual description formats
        by specifying the `diagram` transform on [fenced blocks][Fenced blocks].
        The following marks the content as a `ditaa` textual description.

        ````
        ``` diagram
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```
        ````

        The above will produce

        ``` diagram
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```

        The general format of the `diagram` transform is

        ```
        ?fence options? diagram ?GENERATOR ARG ...?
        ```

        where `GENERATOR` is the diagram generator to use and is followed
        by generator-specific arguments. Currently Ruff! supports `kroki` and
        `ditaa` generators.

        If `GENERATOR` is not specified, as above, it defaults to
        `kroki ditaa`. This default can be changed with the `-diagrammer`
        option to the [::ruff::document] command.


        ### Formatter support

        Not all output formats support embedded diagrams. In such cases the
        fenced block is output as standard preformatted text. For this reason,
        it is best to use an ascii diagram format like `ditaa` so flowcharts
        etc. are still readable when displayed in their original text format.
        You can use tools like [asciiflow](https://asciiflow.com) for
        construction of ascii format diagrams.

        ### Diagrams with kroki

        The `kroki` generator is based on the online diagram converter
        at https://kroki.io which can convert multiple input formats.
        For example, the block below in `graphviz` format


        ````
        ``` diagram kroki graphviz
        digraph {
            "Tcl package" -> "HTML document" [label=" Ruff!"]
        }
        ```
        ````

        will produce

        ``` diagram kroki graphviz
        digraph {
            "Tcl package" -> "HTML document" [label=" Ruff!"]
        }
        ```

        The single argument following `diagram kroki` specifies the input
        format for the block and may be [any format](https://kroki.io/#support)
        supported by `kroki`.

        Use of `kroki` requires a network connection and any **one** of the
        following

        * The `kroki` command line executable that can be downloaded
        for several platforms from https://github.com/yuzutech/kroki-cli/releases/,
        **or**

        * The `twapi` extension (Windows only), **or**

        * The `tls` extension

        Ruff! will try each of the above in turn and use the first that is
        available.

        ### Diagrams with ditaa

        The [ditaa](http://ditaa.sourceforge.net/) generator produces images
        from ASCII text diagrams. Although the `kroki` generator also supports
        this format (using `ditaa` on the server side), the `ditaa` generator
        has the advantage of not requiring network access and allowing for
        more control over image generation. Conversely, it needs the `ditaa`
        Java application to be locally installed.

        Ruff! expects that the generator can be invoked by exec'ing `ditaa`.
        On most Linux programs this can be installed through the system package
        manager. On Windows `ditaa` needs to be downloaded from
        its [repository](https://github.com/stathissideris/ditaa/releases)
        as a `jar` file to a directory included in the `PATH` environment variable.
        Then create a batch file containing the following in that same directory.

        ```
        @echo off
        java -jar %~dp0\ditaa-0.11.0-standalone.jar %*
        ```

        You will need Java also installed and available through `PATH`.

        Similarly, on Unix and MacOS, a shell script needs to be placed in
        the path with equivalent content.

        A `ditaa` block is similar to `kroki` block except it does not need
        a generator argument as input format is always the same. Additional
        arguments specified are passed to the `ditaa` executable.
        For example,

        ````
        ``` diagram ditaa --round-corners --scale 0.8 --no-shadows
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```
        ````

        The above will produce

        ``` diagram ditaa --round-corners --scale 0.8 --no-shadows
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```

        Notice the options to control the generated image, something Ruff! cannot
        do with `kroki`.

        Only the following options or their short form equivalent should
        be used with `ditaa` : `--no-antialias`, `--no-separation`, `--round-corners`,
        `--scale`, and `--fixed-slope`. The `--background` and `--transparent`
        options may be specified but may not play well with all Ruff! themes.
        See the `ditaa` documentation for the meaning of these options.

        ### Diagram options

        The options allowed for [fenced blocks][Fenced blocks] may be used with
        `diagram`.

        Below is a captioned and centered version of the previous example.

        ````
        ``` -align center -caption "Centered diagram with caption" diagram ditaa --scale 0.8
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```
        ````

        The result is shown in [Centered diagram with caption].

        ``` -align center -caption "Centered diagram with caption" diagram ditaa --scale 0.8
        +------------+   Ruff!   +---------------+
        | Tcl script |---------->| HTML document |
        +------------+           +---------------+
        ```

        Note that not all formatters support these options. Those not
        understood by the formatter will be silently ignored.

        ## Output

        Ruff! is designed to support multiple output formats through pluggable
        formatters. The command [formatters] returns a list of supported
        formatters. Currently formatters for producing HTML and Markdown are
        implemented.

        In addition, the output may be produced in single or multipage format.

        ### Multipage output

        The generated documentation may be either in a single output file or
        spread across multiple files. This is controlled
        by the `-pagesplit` option to the [document] command. Some formatters
        may not support this feature.

        When generating multipage output, the toplevel generated page contains
        links to the other pages which contain per-namespace documentation.
        The preamble (passed as the `-preamble` option to the [document] command)
        is also placed in this page.

        ### HTML formatter

        The internal HTML formatter offers

        * A table of contents in a movable pane and tooltips
        * Cross referencing
        * Theming support
        * Optional compact output with expandable content for details
        * Toggles for source code display

        It is also the simplest to use as no other external tools are required.

        The following is a simple example of generating the documentation for
        Ruff! itself in a single page format.

        ```
        ruff::document ::ruff -title "Ruff! reference"
        ```

        To generate documentation, including private namespaces, in multipage
        format:

        ```
        ruff::document ::ruff -recurse true -pagesplit namespace -outdir ./docs -title "Ruff! internal reference"
        ```

        ### Markdown formatter

        The Markdown formatter generates output in generic Github-flavored
        Markdown syntax and expects support for tables in that format.
        It includes cross-linking but does not include a table of contents,
        tooltips or source code display. On the other hand, it allows conversion
        to other formats using external tools.

        The following generates Ruff! documentation in Markdown format and
        then uses `pandoc` to convert it to HTML.
        ```
        ruff::document ::ruff -format markdown -outfile ruff.md -title "Ruff! reference"
        ```
        Then from the shell or Windows command line,
        ```
        pandoc -s -o ruff.html -c ../ruff-md.css --metadata pagetitle="My package" ruff.md
        ```

        When generating HTML from Markdown, it is generally desirable to specify
        a CSS style file. The `ruff-md.css` file provides some *minimal* CSS
        for this purpose.

        ### Nroff formatter

        The Nroff formatter generates documentation in the format required
        for Unix manpages. It generates documentation as a single manpage
        or as a page per namespace with the `-pagesplit namespace` option.
        It does not support navigation links or table of contents.
    }

    namespace eval private {
        namespace path [namespace parent]

        variable ruff_dir
        set ruff_dir [file dirname [info script]]
        variable names
        set names(display) "Ruff!"
        set names(longdisplay) "Runtime Function Formatter"

        # Base file name for output. Needed for linking.
        variable output_file_base ""
        # Extension of base output file
        variable output_file_ext ""
    }
    namespace path private
}

proc ruff::private::ruff_dir {} {
    variable ruff_dir
    return $ruff_dir
}

proc ruff::private::read_asset_file {fn encoding} {

                if { [file tail $fn] eq "ruff-min.css" } {
                    return [binary decode base64 "OnJvb3R7LS1ydWZmLWdyaWQtdGVtcGxhdGUtcm93czptaW4tY29udGVudCAxZnI7LS1ydWZmLWdyaWQtdGVtcGxhdGUtY29sdW1uczptaW5tYXgoMjAwcHgsIG1pbi1jb250ZW50KSAxZnI7LS1ydWZmLWdyaWQtdGVtcGxhdGUtYXJlYXM6InRvcGFyZWEgdG9wYXJlYSIgIm5hdmFyZWEgbWFpbmFyZWEiICJib3RhcmVhIGJvdGFyZWEiOy0tcnVmZi10aXAtei1pbmRleDoxMDstLXJ1ZmYtbmF2LXRvYy1vZmZzZXQ6MGVtO3BhZGRpbmctbGVmdDpjYWxjKDEwMHZ3IC0gMTAwJSk7LS1ydWZmLXRoZW1lLWdyYWRpZW50OmxpbmVhci1ncmFkaWVudCg5MGRlZywgbGlnaHRibHVlLCBjb3JhbCwgbGlnaHRncmVlbil9LnJ1ZmYtdGhlbWUtbGlnaHR7LS1ydWZmLWNvbG9yOiM0NDQ7LS1ydWZmLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtbWlub3ItY29sb3I6Izg4ODstLXJ1ZmYtbGF5b3V0LWJhY2tncm91bmQtY29sb3I6I2ZlZmVmZTstLXJ1ZmYtaGQtY29sb3I6IzY2NjstLXJ1ZmYtaGQtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1oZC1mb250OmxhcmdlIGJvbGQ7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiNGRkY1RUE7LS1ydWZmLW5hdi1jb2xvcjojNjY2Oy0tcnVmZi1uYXYtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLW5hdi1oaWdobGlnaHQtYmFja2dyb3VuZC1jb2xvcjpjb3JhbDstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtaC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLWgtY29sb3I6Izk2OEM4MzstLXJ1ZmYtYmQtaDEtY29sb3I6IzY2NjstLXJ1ZmYtYmQtaDEtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtYS1jb2xvcjpibHVlOy0tcnVmZi1iZC1zb3VyY2VsaW5rLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYS1jb2xvcik7LS1ydWZmLWJkLXNvdXJjZWxpbmstYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1zeW5vcHNpcy1ib3JkZXI6bm9uZTstLXJ1ZmYtYmQtdGlwLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1mdC1jb2xvcjp2YXIoLS1ydWZmLWJkLW1pbm9yLWNvbG9yKTstLXJ1ZmYtZnQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1mdC1jb2xvcik7LS1ydWZmLWZ0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1jbG91ZHMsLnJ1ZmYtdGhlbWUtZGFyaywucnVmZi10aGVtZS1saWdodCwucnVmZi10aGVtZS1zb2xhcnstLXJ1ZmYtbmF2LXRpcC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWNvbG9yKTstLXJ1ZmYtYmQtY29sb3I6dmFyKC0tcnVmZi1jb2xvcil9LnJ1ZmYtdGhlbWUtY2xvdWRzey0tcnVmZi1jb2xvcjojMTExOy0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yOndoaXRlc21va2U7LS1ydWZmLW1pbm9yLWNvbG9yOiM4ODg7LS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yOmF6dXJlOy0tcnVmZi1oZC1jb2xvcjojNjY2Oy0tcnVmZi1oZC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWZvbnQ6bGFyZ2UgYm9sZDstLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3I6bGlnaHRibHVlOy0tcnVmZi1uYXYtY29sb3I6IzIxMjEyMTstLXJ1ZmYtbmF2LXRpcC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjp2YXIoLS1ydWZmLWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6IzE0YTdmZjstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6I2NmZWJmNzstLXJ1ZmYtYmQtY29kZS1iYWNrZ3JvdW5kLWNvbG9yOiNjZmZjZmY7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oLWNvbG9yOiM5NjhDODM7LS1ydWZmLWJkLWgxLWNvbG9yOiM2NjY7LS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLWEtY29sb3I6Ymx1ZTstLXJ1ZmYtYmQtc291cmNlbGluay1jb2xvcjp2YXIoLS1ydWZmLWJkLWEtY29sb3IpOy0tcnVmZi1iZC1zb3VyY2VsaW5rLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtc3lub3BzaXMtYm9yZGVyOm5vbmU7LS1ydWZmLWJkLXRpcC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtZnQtY29sb3I6dmFyKC0tcnVmZi1iZC1taW5vci1jb2xvcik7LS1ydWZmLWZ0LW1pbm9yLWNvbG9yOnZhcigtLXJ1ZmYtZnQtY29sb3IpOy0tcnVmZi1mdC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcil9LnJ1ZmYtdGhlbWUtZGFyaywucnVmZi10aGVtZS1zb2xhcnstLXJ1ZmYtbWlub3ItY29sb3I6I2FhYTstLXJ1ZmYtbmF2LWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWNvbG9yOmluaGVyaXR9LnJ1ZmYtdGhlbWUtZGFya3stLXJ1ZmYtY29sb3I6I2RkZDstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjojMjcyNDJjOy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMjEyMTIxOy0tcnVmZi1oZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1oZC1iYWNrZ3JvdW5kLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWZvbnQ6bGFyZ2UgYm9sZDstLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3I6IzIyMjcyZTstLXJ1ZmYtbmF2LXRpcC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtbmF2LWhpZ2hsaWdodC1jb2xvcjojZTZlZmY1Oy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6IzExNjRhMzstLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1taW5vci1jb2xvcik7LS1ydWZmLWJkLXRhYmxlLWJvcmRlcjojODA4MDgwOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oLWNvbG9yOmxpZ2h0Ymx1ZTstLXJ1ZmYtYmQtaDEtY29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpOy0tcnVmZi1iZC1oMS1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1hLWNvbG9yOiM0ODliZjU7LS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3I6dmFyKC0tcnVmZi1iZC1hLWNvbG9yKTstLXJ1ZmYtYmQtc291cmNlbGluay1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcjpub25lOy0tcnVmZi1iZC10aXAtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2xvcik7LS1ydWZmLWZ0LWNvbG9yOnZhcigtLXJ1ZmYtYmQtbWlub3ItY29sb3IpOy0tcnVmZi1mdC1taW5vci1jb2xvcjp2YXIoLS1ydWZmLWZ0LWNvbG9yKTstLXJ1ZmYtZnQtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLXRoZW1lLXNvbGFyey0tcnVmZi1jb2xvcjp3aGl0ZXNtb2tlOy0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yOiMwMTE7LS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yOiMwMDJiMzU7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiMwMDM2NDE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjojMDAzNjQxOy0tcnVmZi1iZC1oLWNvbG9yOmNvcm5zaWxrOy0tcnVmZi1iZC1hLWNvbG9yOnBhbGVncmVlbjstLXJ1ZmYtYmQtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29sb3IpOy0tcnVmZi1iZC10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1tYXJvb24sLnJ1ZmYtdGhlbWUtc2xhdGUsLnJ1ZmYtdGhlbWUtc29sYXIsLnJ1ZmYtdGhlbWUtdjF7LS1ydWZmLWhkLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWhkLWJhY2tncm91bmQtY29sb3I6aW5oZXJpdDstLXJ1ZmYtaGQtZm9udDpsYXJnZSBib2xkOy0tcnVmZi1uYXYtdGlwLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpOy0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbGF5b3V0LWJhY2tncm91bmQtY29sb3IpOy0tcnVmZi1iZC1taW5vci1jb2xvcjp2YXIoLS1ydWZmLW1pbm9yLWNvbG9yKTstLXJ1ZmYtYmQtdGFibGUtYm9yZGVyOiM4MDgwODA7LS1ydWZmLWJkLWgtYmFja2dyb3VuZC1jb2xvcjppbmhlcml0Oy0tcnVmZi1iZC1oMS1jb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3I6dmFyKC0tcnVmZi1iZC1hLWNvbG9yKTstLXJ1ZmYtYmQtc291cmNlbGluay1iYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7LS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcjpub25lOy0tcnVmZi1mdC1jb2xvcjp2YXIoLS1ydWZmLWJkLW1pbm9yLWNvbG9yKTstLXJ1ZmYtZnQtbWlub3ItY29sb3I6dmFyKC0tcnVmZi1mdC1jb2xvcik7LS1ydWZmLWZ0LWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1iYWNrZ3JvdW5kLWNvbG9yKX0ucnVmZi10aGVtZS1zbGF0ZXstLXJ1ZmYtY29sb3I6I2NjYzstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjojODI5YWIxOy0tcnVmZi1taW5vci1jb2xvcjojYWFhOy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMTgxYTI2Oy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjojMWEyMDJjOy0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcjpyZ2IoNDUsNTUsNzIpOy0tcnVmZi1uYXYtY29sb3I6dmFyKC0tcnVmZi1jb2xvcik7LS1ydWZmLW5hdi10aXAtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLWJkLWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWNvbG9yOiNlY2RiYmE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtaC1jb2xvcjpsaWdodHN0ZWVsYmx1ZTstLXJ1ZmYtYmQtYS1jb2xvcjpsaWdodHNreWJsdWU7LS1ydWZmLWJkLXRpcC1jb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWJhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcil9LnJ1ZmYtdGhlbWUtbWFyb29uLC5ydWZmLXRoZW1lLXYxey0tcnVmZi1taW5vci1jb2xvcjojODg4Oy0tcnVmZi1sYXlvdXQtYmFja2dyb3VuZC1jb2xvcjp3aGl0ZTstLXJ1ZmYtbmF2LWNvbG9yOndoaXRlOy0tcnVmZi1uYXYtdGlwLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtY29kZS1jb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTstLXJ1ZmYtYmQtaC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTstLXJ1ZmYtYmQtdGlwLWNvbG9yOmluaGVyaXQ7LS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yOndoaXRlc21va2V9LnJ1ZmYtdGhlbWUtdjF7LS1ydWZmLWNvbG9yOiMxMjEyMTI7LS1ydWZmLWJhY2tncm91bmQtY29sb3I6d2hpdGU7LS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yOiMwMDY2NjY7LS1ydWZmLWJkLWNvbG9yOnZhcigtLXJ1ZmYtY29sb3IpOy0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3I6d2hpdGVzbW9rZTstLXJ1ZmYtYmQtYS1jb2xvcjpibHVlfS5ydWZmLXRoZW1lLW1hcm9vbnstLXJ1ZmYtY29sb3I6Izg0NDstLXJ1ZmYtYmFja2dyb3VuZC1jb2xvcjp3aGl0ZXNtb2tlOy0tcnVmZi1uYXYtYmFja2dyb3VuZC1jb2xvcjptYXJvb247LS1ydWZmLWJkLWNvbG9yOiMyMTIxMjE7LS1ydWZmLWJkLWNvZGUtYmFja2dyb3VuZC1jb2xvcjojZmZmMGYwOy0tcnVmZi1iZC1hLWNvbG9yOiM0NGZ9I3J1ZmZCdXR0b25CYXJ7ZmxvYXQ6cmlnaHR9I3J1ZmZOYXZNb3ZlLCNydWZmVG9nZ2xlVGhlbWV7aGVpZ2h0OjIwcHg7Ym9yZGVyOjA7Y3Vyc29yOnBvaW50ZXI7dmVydGljYWwtYWxpZ246dGV4dC10b3B9I3J1ZmZUb2dnbGVUaGVtZXtiYWNrZ3JvdW5kLWltYWdlOnZhcigtLXJ1ZmYtdGhlbWUtZ3JhZGllbnQpO3RyYW5zaXRpb246LjI1cztiYWNrZ3JvdW5kLXNpemU6MjAwJSBhdXRvO3dpZHRoOjIwcHh9I3J1ZmZUb2dnbGVUaGVtZTpob3ZlcntiYWNrZ3JvdW5kLXBvc2l0aW9uOnJpZ2h0IGNlbnRlcjt2ZXJ0aWNhbC1hbGlnbjp0ZXh0LXRvcH0jcnVmZk5hdk1vdmV7Y29sb3I6I2FkZDhlNjtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7dGV4dC1hbGlnbjpjZW50ZXJ9Kiw6OmFmdGVyLDo6YmVmb3Jle2JveC1zaXppbmc6Ym9yZGVyLWJveH1ib2R5e2NvbG9yOnZhcigtLXJ1ZmYtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iYWNrZ3JvdW5kLWNvbG9yKX1hLGE6dmlzaXRlZHtjb2xvcjppbmhlcml0O2JhY2tncm91bmQtY29sb3I6aW5oZXJpdH0ucnVmZi1sYXlvdXR7ZGlzcGxheTpncmlkO2dyaWQtdGVtcGxhdGUtcm93czp2YXIoLS1ydWZmLWdyaWQtdGVtcGxhdGUtcm93cyk7Z3JpZC10ZW1wbGF0ZS1jb2x1bW5zOnZhcigtLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1jb2x1bW5zKTtncmlkLXRlbXBsYXRlLWFyZWFzOnZhcigtLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1hcmVhcyk7Y29sdW1uLWdhcDoxcmVtO21pbi1oZWlnaHQ6MTAwdmg7bWF4LXdpZHRoOjYwcmVtO21hcmdpbjowIGF1dG87YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWxheW91dC1iYWNrZ3JvdW5kLWNvbG9yKTtwYWRkaW5nOjAgMTBweH0ucnVmZi1sYXlvdXQtaGVhZGVye2dyaWQtYXJlYTp0b3BhcmVhfS5ydWZmLWxheW91dC1tYWlue2dyaWQtYXJlYTptYWluYXJlYX0ucnVmZi1sYXlvdXQtbmF2e2dyaWQtYXJlYTpuYXZhcmVhfS5ydWZmLWxheW91dC1mb290ZXJ7Z3JpZC1hcmVhOmJvdGFyZWF9aDEsaDIsaDMsaDQsaDUsaDZ7bWFyZ2luLWJvdHRvbTouNWVtO21hcmdpbi10b3A6MH1saXttYXJnaW4tdG9wOi41ZW19c3Bhbi5uc19zY29wZXtjb2xvcjp2YXIoLS1ydWZmLW1pbm9yLWNvbG9yKTtmb250LXNpemU6ODUlfXNwYW4ubnNfc2NvcGUgYVtocmVmXTpsaW5rLHNwYW4ubnNfc2NvcGUgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjpub25lO2NvbG9yOnZhcigtLXJ1ZmYtbWlub3ItY29sb3IpfXNwYW4ubnNfc2NvcGUgYVtocmVmXTpob3Zlcnt0ZXh0LWRlY29yYXRpb246dW5kZXJsaW5lfS5ydWZmLXRpcHtwb3NpdGlvbjpyZWxhdGl2ZX0ucnVmZi10aXA6aG92ZXIgLnJ1ZmYtdGlwdGV4dHt2aXNpYmlsaXR5OnZpc2libGV9LnJ1ZmYtdGlwdGV4dCBwcmV7bWFyZ2luLXRvcDowfS5ydWZmLXRpcHRleHR7bWluLXdpZHRoOjIwZW07dGV4dC1hbGlnbjpsZWZ0O2JvcmRlcjowO3Bvc2l0aW9uOmFic29sdXRlO3otaW5kZXg6dmFyKC0tcnVmZi10aXAtei1pbmRleCk7bWFyZ2luLWxlZnQ6NHB4O3BhZGRpbmc6MnB4IDNweDt2aXNpYmlsaXR5OmhpZGRlbn0ucnVmZi1oZHtmb250LWZhbWlseToiVGltZXMgTmV3IFJvbWFuIixzZXJpZjtmb250LXNpemU6MjAwJTtwYWRkaW5nOjVweCAwIDEwcHg7Y29sb3I6dmFyKC0tcnVmZi1oZC1jb2xvcik7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWhkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWZ0e3RleHQtYWxpZ246bGVmdDtib3JkZXItdG9wOjFweCBzb2xpZCB2YXIoLS1ydWZmLWZ0LWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLWZ0LWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtZnQtYmFja2dyb3VuZC1jb2xvcik7bWFyZ2luOjEwcHggMH0ucnVmZi1mdCBkaXZ7cGFkZGluZzo1cHggMH0ucnVmZi1iZCwucnVmZi1uYXZ7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZn0ucnVmZi1uYXZ7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLW5hdi1jb2xvcik7LS1ydWZmLW5hdi1wYWRkaW5nLXg6NHB4O3BhZGRpbmc6M3B4IHZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCkgMnB4IHZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCk7b3ZlcmZsb3c6dmlzaWJsZTtmb250LXNpemU6ODUlO21hcmdpbjowfS5ydWZmLW5hdiB1bHtsaXN0LXN0eWxlOm5vbmU7bWFyZ2luOjA7cGFkZGluZzowfS5ydWZmLW5hdiBsaSxib2R5e21hcmdpbjowfS5ydWZmLW5hdiAucnVmZi10b2MxLHNwYW4ubnNfc2NvcGV7Zm9udC13ZWlnaHQ6NzAwfS5ydWZmLW5hdiAucnVmZi10b2Mye3BhZGRpbmctbGVmdDpjYWxjKDJlbSArIHZhcigtLXJ1ZmYtbmF2LXRvYy1vZmZzZXQsMCkpO3RleHQtaW5kZW50Oi0yZW19LnJ1ZmYtbmF2IC5ydWZmLXRvYzN7cGFkZGluZy1sZWZ0OmNhbGMoM2VtICsgdmFyKC0tcnVmZi1uYXYtdG9jLW9mZnNldCwwKSk7dGV4dC1pbmRlbnQ6LTJlbX0ucnVmZi1uYXYgLnJ1ZmYtdG9jNHtwYWRkaW5nLWxlZnQ6Y2FsYyg0ZW0gKyB2YXIoLS1ydWZmLW5hdi10b2Mtb2Zmc2V0LDApKTt0ZXh0LWluZGVudDotMmVtfS5ydWZmLW5hdiAucnVmZi10b2M1e3BhZGRpbmctbGVmdDpjYWxjKDVlbSArIHZhcigtLXJ1ZmYtbmF2LXRvYy1vZmZzZXQsMCkpO3RleHQtaW5kZW50Oi0yZW19LnJ1ZmYtbmF2IGhye2NvbG9yOmluaGVyaXQ7bWFyZ2luLXRvcDouMmVtO21hcmdpbi1ib3R0b206LjJlbX0ucnVmZi1uYXYgYTpob3ZlciwucnVmZi1uYXYgYTpsaW5rLC5ydWZmLW5hdiBhOnZpc2l0ZWR7dGV4dC1kZWNvcmF0aW9uOm5vbmU7Y29sb3I6dmFyKC0tcnVmZi1uYXYtY29sb3IpO2JhY2tncm91bmQtY29sb3I6aW5oZXJpdH0ucnVmZi1uYXYgYTpob3Zlcntjb2xvcjp2YXIoLS1ydWZmLW5hdi1iYWNrZ3JvdW5kLWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtbmF2LWNvbG9yKX0ucnVmZi1uYXYgYS5ydWZmLWhpZ2hsaWdodHtjb2xvcjp2YXIoLS1ydWZmLW5hdi1oaWdobGlnaHQtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtaGlnaGxpZ2h0LWJhY2tncm91bmQtY29sb3IpO21hcmdpbi1sZWZ0OmNhbGMoLTEqdmFyKC0tcnVmZi1uYXYtcGFkZGluZy14KSk7cGFkZGluZy1sZWZ0OnZhcigtLXJ1ZmYtbmF2LXBhZGRpbmcteCk7cGFkZGluZy1yaWdodDp2YXIoLS1ydWZmLW5hdi1wYWRkaW5nLXgpfS5ydWZmLW5hdiAucnVmZi10aXB0ZXh0e2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1uYXYtdGlwLWJhY2tncm91bmQtY29sb3IpO2NvbG9yOnZhcigtLXJ1ZmYtbmF2LXRpcC1jb2xvcik7dGV4dC1pbmRlbnQ6MH0ucnVmZi1iZHtjb2xvcjp2YXIoLS1ydWZmLWJkLWNvbG9yKTtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtYmFja2dyb3VuZC1jb2xvcik7Zm9udC1zaXplOjkzJTtsaW5lLWhlaWdodDoxLjN9LnJ1ZmYtYmQgLnJ1ZmYtdXBsaW5re2ZvbnQtc2l6ZTp4LXNtYWxsO2ZvbnQtdmFyaWFudDpub3JtYWw7Zm9udC1mYW1pbHk6QXJpYWwsc2Fucy1zZXJpZjtmbG9hdDpyaWdodDtwYWRkaW5nOjJweH0ucnVmZi1iZCAucnVmZi11cGxpbmsgYVtocmVmXSwucnVmZi1iZCAucnVmZi11cGxpbmsgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjp1bmRlcmxpbmU7Y29sb3I6dmFyKC0tcnVmZi1iZC1oLWNvbG9yKX0ucnVmZi1iZCBoMSwucnVmZi1iZCBoMSAucnVmZi11cGxpbmsgYVtocmVmXSwucnVmZi1iZCBoMSAucnVmZi11cGxpbmsgYVtocmVmXTp2aXNpdGVke2NvbG9yOnZhcigtLXJ1ZmYtYmQtaDEtY29sb3IpfS5ydWZmLWJkIHRhYmxlLnJ1ZmZfZGVmbGlzdHttYXJnaW46LjVlbSAxZW0gMWVtO2JvcmRlcjp0aGluIHNvbGlkO2JvcmRlci1jb2xsYXBzZTpjb2xsYXBzZTtib3JkZXItY29sb3I6dmFyKC0tcnVmZi1iZC10YWJsZS1ib3JkZXIpO3BhZGRpbmc6NHB4fS5ydWZmLWJkIC5ydWZmX2RlZmxpc3QgdGQsLnJ1ZmYtYmQgLnJ1ZmZfZGVmbGlzdCB0aHtib3JkZXI6dGhpbiBzb2xpZDtib3JkZXItY29sb3I6Z3JheTtwYWRkaW5nOi4xZW0gLjNlbSAuM2VtfS5ydWZmLWJkIC5ydWZmX2RlZmxpc3QgdGR7dmVydGljYWwtYWxpZ246dG9wO2ZvbnQtc2l6ZTo5MyV9LnJ1ZmYtYmQgLnJ1ZmZfZGVmbGlzdCB0aHtiYWNrZ3JvdW5kLWNvbG9yOiNjY2N9LnJ1ZmYtYmQgaDF7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWgxLWJhY2tncm91bmQtY29sb3IpO3BhZGRpbmctbGVmdDoycHg7bWFyZ2luLWxlZnQ6LTJweH0ucnVmZi1iZCBoMiwucnVmZi1iZCBoMywucnVmZi1iZCBoNCwucnVmZi1iZCBoNSwucnVmZi1iZCBoNntjb2xvcjp2YXIoLS1ydWZmLWJkLWgtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1oLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWJkIGgxLC5ydWZmLWJkIGgye2ZvbnQtZmFtaWx5OiJUaW1lcyBOZXcgUm9tYW4iLHNlcmlmfS5ydWZmLWJkIGgye2ZvbnQtdmFyaWFudDpzbWFsbC1jYXBzfS5ydWZmLWJkIGgzLC5ydWZmLWJkIGg0LC5ydWZmLWJkIGg1LC5ydWZmLWJkIGg2e21hcmdpbi1ib3R0b206LjJlbX0ucnVmZi1iZCBoNXtmb250LXN0eWxlOml0YWxpY30ucnVmZi1iZCBoNSwucnVmZi1iZCBoNntmb250LXdlaWdodDo0MDA7Zm9udC1zaXplOmluaGVyaXR9LnJ1ZmYtYmQgaDMucnVmZmNsYXNzLC5ydWZmLWJkIGgzLnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDMucnVmZnByb2MsLnJ1ZmYtYmQgaDQucnVmZmNsYXNzLC5ydWZmLWJkIGg0LnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDQucnVmZnByb2MsLnJ1ZmYtYmQgaDUucnVmZmNsYXNzLC5ydWZmLWJkIGg1LnJ1ZmZtZXRob2QsLnJ1ZmYtYmQgaDUucnVmZnByb2N7Ym9yZGVyLWJvdHRvbTp0aGluIHNvbGlkO21hcmdpbi1ib3R0b206LjJlbTttYXJnaW4tdG9wOjJlbX0ucnVmZi1iZCAucnVmZl9jbWQsLnJ1ZmYtYmQgY29kZXtiYWNrZ3JvdW5kLWNvbG9yOnZhcigtLXJ1ZmYtYmQtY29kZS1iYWNrZ3JvdW5kLWNvbG9yKTtib3JkZXItcmFkaXVzOjRweDtwYWRkaW5nLWxlZnQ6MnB4O3BhZGRpbmctcmlnaHQ6MnB4fS5ydWZmLWJkIC5ydWZmX3N5bm9wc2lzLC5ydWZmLWJkIHByZXtjb2xvcjp2YXIoLS1ydWZmLWJkLWNvZGUtY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWJkIHByZXtwYWRkaW5nOjVweDtmb250LWZhbWlseTpDb25zb2xhcywiQ291cmllciBOZXciLG1vbm9zcGFjZTtmb250LXNpemU6c21hbGxlcjtsaW5lLWhlaWdodDoxLjJlbTt3aGl0ZS1zcGFjZTpwcmUtd3JhcDtvdmVyZmxvdy13cmFwOmJyZWFrLXdvcmQ7ZGlzcGxheTppbmxpbmUtYmxvY2s7dGV4dC1hbGlnbjpsZWZ0fS5ydWZmLWJkIGFbaHJlZl0sLnJ1ZmYtYmQgYVtocmVmXTp2aXNpdGVke3RleHQtZGVjb3JhdGlvbjpub25lO2NvbG9yOnZhcigtLXJ1ZmYtYmQtYS1jb2xvcil9LnJ1ZmYtYmQgYVtocmVmXTpob3ZlciwucnVmZi1leHBhbmQ+c3Bhbnt0ZXh0LWRlY29yYXRpb246dW5kZXJsaW5lfS5ydWZmX2R5bl9zcmN7ZGlzcGxheTpub25lfS5ydWZmLWJkIC5ydWZmX3N5bm9wc2lze2JvcmRlcjp2YXIoLS1ydWZmLWJkLXN5bm9wc2lzLWJvcmRlcik7bWFyZ2luOjAgMmVtIDFlbTtwYWRkaW5nOi41ZW19LnJ1ZmYtYmQgLnJ1ZmZfYXJnLC5ydWZmLWJkIC5ydWZmX2NtZCwucnVmZi1iZCAucnVmZl9jb25zdCwucnVmZi1iZCAucnVmZl9zeW5vcHNpcywucnVmZi1iZCBjb2Rle2ZvbnQtZmFtaWx5OkNvbnNvbGFzLCJDb3VyaWVyIE5ldyIsbW9ub3NwYWNlfS5ydWZmLWJkIC5ydWZmX2FyZ3tmb250LXN0eWxlOml0YWxpYztmb250LXNpemU6c21hbGxlcn0ucnVmZi1iZCAucnVmZl9zb3VyY2VfbGlua3tmb250LXNpemU6c21hbGx9LnJ1ZmYtYmQgLnJ1ZmZfc291cmNlX2xpbmsgYVtocmVmXXtjb2xvcjp2YXIoLS1ydWZmLWJkLXNvdXJjZWxpbmstY29sb3IpO2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1zb3VyY2VsaW5rLWJhY2tncm91bmQtY29sb3IpO3RleHQtZGVjb3JhdGlvbjp1bmRlcmxpbmV9LnJ1ZmZfaW5kZXh7Zm9udC1zaXplOnNtYWxsZXJ9LnJ1ZmZfaW5kZXggdWwgbGl7bGlzdC1zdHlsZS10eXBlOm5vbmV9LnJ1ZmZfaW5kZXggdWwgbGkgYXt0ZXh0LWRlY29yYXRpb246bm9uZX0jaW5kZXhVTCwjcnVmZk5hdk1vdmV7bGluZS1oZWlnaHQ6MX0ucnVmZi1iZCAjaW5kZXhVTCAucnVmZi10aXB0ZXh0LC5ydWZmLWJkICNpbmRleFVMIC5ydWZmLXRpcHRleHQgcHJlLC5ydWZmLWJkICNpbmRleFVMIC5ydWZmLXRpcHRleHQgcHJlIC5ydWZmX2FyZywucnVmZi1iZCAjaW5kZXhVTCAucnVmZi10aXB0ZXh0IHByZSAucnVmZl9jbWR7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLXRpcC1iYWNrZ3JvdW5kLWNvbG9yKTtjb2xvcjp2YXIoLS1ydWZmLWJkLXRpcC1jb2xvcil9LnJ1ZmYtZmlndXJle21hcmdpbjouNWVtIDFlbX0ucnVmZi1zbmlwcGV0e2JhY2tncm91bmQtY29sb3I6dmFyKC0tcnVmZi1iZC1jb2RlLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWxlZnR7dGV4dC1hbGlnbjpsZWZ0fS5ydWZmLWNlbnRlcnt0ZXh0LWFsaWduOmNlbnRlcn0ucnVmZi1yaWdodHt0ZXh0LWFsaWduOnJpZ2h0fS5ydWZmLWNhcHRpb257Zm9udC1zdHlsZTppdGFsaWM7Zm9udC1zaXplOnNtYWxsZXI7YmFja2dyb3VuZC1jb2xvcjp2YXIoLS1ydWZmLWJkLWJhY2tncm91bmQtY29sb3IpfS5ydWZmLWZpZ3VyZSBpbWd7bWF4LXdpZHRoOjEwMCU7aGVpZ2h0OmF1dG99LnJ1ZmYtZXhwYW5kPnNwYW57Zm9udC1zaXplOnNtYWxsfXN1bW1hcnkucnVmZi1leHBhbmR7bWFyZ2luLWJvdHRvbToxZW19"]
                }
                if { [file tail $fn] eq "ruff-min.js" } {
                    return [binary decode base64 "ZnVuY3Rpb24gdG9nZ2xlU291cmNlKGlkKXt2YXIgZWxlbTt2YXIgbGluaztpZihkb2N1bWVudC5nZXRFbGVtZW50QnlJZCl7ZWxlbT1kb2N1bWVudC5nZXRFbGVtZW50QnlJZChpZCk7bGluaz1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgibF8iK2lkKX1lbHNlIGlmKGRvY3VtZW50LmFsbCl7ZWxlbT1ldmFsKCJkb2N1bWVudC5hbGwuIitpZCk7bGluaz1ldmFsKCJkb2N1bWVudC5hbGwubF8iK2lkKX1lbHNlIHJldHVybiBmYWxzZTtpZihlbGVtLnN0eWxlLmRpc3BsYXk9PSJibG9jayIpe2VsZW0uc3R5bGUuZGlzcGxheT0ibm9uZSI7bGluay5pbm5lckhUTUw9IlNob3cgc291cmNlIn1lbHNle2VsZW0uc3R5bGUuZGlzcGxheT0iYmxvY2siO2xpbmsuaW5uZXJIVE1MPSJIaWRlIHNvdXJjZSJ9fWZ1bmN0aW9uIHJ1ZmZTZXRUaGVtZSh0aGVtZU5hbWUpe2xvY2FsU3RvcmFnZS5ydWZmX3RoZW1lPXRoZW1lTmFtZTtkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuY2xhc3NOYW1lPSJydWZmLXRoZW1lLSIuY29uY2F0KHRoZW1lTmFtZSl9ZnVuY3Rpb24gcnVmZk5leHRUaGVtZSgpe3RoZW1lTmFtZXM9SlNPTi5wYXJzZShsb2NhbFN0b3JhZ2UucnVmZl90aGVtZXMpO2N1cnJlbnRUaGVtZT1sb2NhbFN0b3JhZ2UucnVmZl90aGVtZTtpZihjdXJyZW50VGhlbWU9PT11bmRlZmluZWQpe3RoZW1lSW5kZXg9MH1lbHNle3RoZW1lSW5kZXg9dGhlbWVOYW1lcy5pbmRleE9mKGN1cnJlbnRUaGVtZSk7Kyt0aGVtZUluZGV4O2lmKHRoZW1lSW5kZXg+PXRoZW1lTmFtZXMubGVuZ3RoKXt0aGVtZUluZGV4PTB9fXJ1ZmZTZXRUaGVtZSh0aGVtZU5hbWVzW3RoZW1lSW5kZXhdKX1mdW5jdGlvbiBydWZmU2V0TmF2U2lkZShuYXZTaWRlKXtsb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZT1uYXZTaWRlO2J1dD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgicnVmZk5hdk1vdmUiKTtpZihuYXZTaWRlPT09InJpZ2h0Iil7Z3JpZEFyZWFzPScidG9wYXJlYSB0b3BhcmVhIiAibWFpbmFyZWEgbmF2YXJlYSIgImJvdGFyZWEgYm90YXJlYSInO2dyaWRDb2xzPSIxZnIgbWlubWF4KDIwMHB4LCBtaW4tY29udGVudCkiO2J1dC50ZXh0Q29udGVudD0iwCI7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdCIsIm5vbmUiKTtidXQuc3R5bGUuc2V0UHJvcGVydHkoImJvcmRlci1yaWdodC1zdHlsZSIsInNvbGlkIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItcmlnaHQtd2lkdGgiLCJ0aGljayIpfWVsc2V7Z3JpZEFyZWFzPScidG9wYXJlYSB0b3BhcmVhIiAibmF2YXJlYSBtYWluYXJlYSIgImJvdGFyZWEgYm90YXJlYSInO2dyaWRDb2xzPSJtaW5tYXgoMjAwcHgsIG1pbi1jb250ZW50KSAxZnIiO2J1dC50ZXh0Q29udGVudD0itiI7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItcmlnaHQiLCJub25lIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdC1zdHlsZSIsInNvbGlkIik7YnV0LnN0eWxlLnNldFByb3BlcnR5KCJib3JkZXItbGVmdC13aWR0aCIsInRoaWNrIil9ZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LnN0eWxlLnNldFByb3BlcnR5KCItLXJ1ZmYtZ3JpZC10ZW1wbGF0ZS1hcmVhcyIsZ3JpZEFyZWFzKTtkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuc3R5bGUuc2V0UHJvcGVydHkoIi0tcnVmZi1ncmlkLXRlbXBsYXRlLWNvbHVtbnMiLGdyaWRDb2xzKX1mdW5jdGlvbiBydWZmTW92ZU5hdlBhbmUoKXtpZihsb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZT09PSJsZWZ0IilydWZmU2V0TmF2U2lkZSgicmlnaHQiKTtlbHNlIHJ1ZmZTZXROYXZTaWRlKCJsZWZ0Iil9KGZ1bmN0aW9uKCl7dGhlbWVOYW1lcz1bInYxIiwibGlnaHQiLCJkYXJrIiwic2xhdGUiLCJzb2xhciIsImNsb3VkcyIsIm1hcm9vbiJdO2xvY2FsU3RvcmFnZS5ydWZmX3RoZW1lcz1KU09OLnN0cmluZ2lmeSh0aGVtZU5hbWVzKTtuYXZTaWRlPWxvY2FsU3RvcmFnZS5ydWZmX25hdl9zaWRlO2lmKG5hdlNpZGUhPT0ibGVmdCImJm5hdlNpZGUhPT0icmlnaHQiKW5hdlNpZGU9ImxlZnQiO3dpbmRvdy5vbmxvYWQ9aW5pdDtmdW5jdGlvbiBpbml0KCl7Y3VycmVudFRoZW1lPWxvY2FsU3RvcmFnZS5ydWZmX3RoZW1lO2lmKGN1cnJlbnRUaGVtZT09PXVuZGVmaW5lZHx8dGhlbWVOYW1lcy5pbmRleE9mKGN1cnJlbnRUaGVtZSk8MCl7Y3VycmVudFRoZW1lPSJ2MSJ9cnVmZlNldFRoZW1lKGN1cnJlbnRUaGVtZSk7bmF2U2lkZT1sb2NhbFN0b3JhZ2UucnVmZl9uYXZfc2lkZTtpZihuYXZTaWRlIT09InJpZ2h0IiluYXZTaWRlPSJsZWZ0IjtydWZmU2V0TmF2U2lkZShuYXZTaWRlKX19KSgpOw=="]
                }
                if { [file tail $fn] eq "ruff-index-min.js" } {
                    return [binary decode base64 "ZnVuY3Rpb24gbXlJbmRleEluaXQoKXt2YXIgc2luZ2xlLG52aXNpYmxlLHVybHRleHQsYTtmaWx0ZXJFbGVtZW50PWRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCJmaWx0ZXJUZXh0Iik7dXJsdGV4dD1teUdldFVybFBhcmFtZXRlcigibG9va3VwIik7aWYodXJsdGV4dD09IiIpe3VybHRleHQ9bXlHZXRVcmxQYXJhbWV0ZXIoInNlYXJjaCIpfWlmKHVybHRleHQhPSIiKXtmaWx0ZXJFbGVtZW50LnZhbHVlPXVybHRleHQ7bXlSdW5GaWx0ZXIoKX1maWx0ZXJFbGVtZW50LmZvY3VzKCl9ZnVuY3Rpb24gbXlHZXRVcmxQYXJhbWV0ZXIobmFtZSl7bmFtZT1uYW1lLnJlcGxhY2UoL1tcW10vLCJcXFsiKS5yZXBsYWNlKC9bXF1dLywiXFxdIik7dmFyIHJlZ2V4PW5ldyBSZWdFeHAoIltcXD8mXSIrbmFtZSsiPShbXiYjXSopIik7dmFyIHJlc3VsdHM9cmVnZXguZXhlYyhsb2NhdGlvbi5zZWFyY2gpO3JldHVybiByZXN1bHRzPT09bnVsbD8iIjpkZWNvZGVVUklDb21wb25lbnQocmVzdWx0c1sxXS5yZXBsYWNlKC9cKy9nLCIgIikpfXZhciBteURlYm91bmNlRGVsYXk7dmFyIG15VXNlckFnZW50PW5hdmlnYXRvci51c2VyQWdlbnQudG9VcHBlckNhc2UoKTtpZihteVVzZXJBZ2VudC5pbmRleE9mKCJNU0lFIikhPS0xfHxteVVzZXJBZ2VudC5pbmRleE9mKCJUUklERU5UIikhPS0xKXtteURlYm91bmNlRGVsYXk9MzAwfWVsc2UgaWYobXlVc2VyQWdlbnQuaW5kZXhPZigiRURHRSIpIT0tMSl7bXlEZWJvdW5jZURlbGF5PTMwMH1lbHNle215RGVib3VuY2VEZWxheT0xMDB9ZnVuY3Rpb24gbXlEZWJvdW5jZShmdW5jLHdhaXQsaW1tZWRpYXRlKXt2YXIgdGltZW91dDtyZXR1cm4gZnVuY3Rpb24oKXt2YXIgY29udGV4dD10aGlzLGFyZ3M9YXJndW1lbnRzO3ZhciBsYXRlcj1mdW5jdGlvbigpe3RpbWVvdXQ9bnVsbDtpZighaW1tZWRpYXRlKWZ1bmMuYXBwbHkoY29udGV4dCxhcmdzKX07dmFyIGNhbGxOb3c9aW1tZWRpYXRlJiYhdGltZW91dDtjbGVhclRpbWVvdXQodGltZW91dCk7dGltZW91dD1zZXRUaW1lb3V0KGxhdGVyLHdhaXQpO2lmKGNhbGxOb3cpZnVuYy5hcHBseShjb250ZXh0LGFyZ3MpfX1mdW5jdGlvbiBteVNldFN0YXR1cyh0ZXh0KXt2YXIgc3RhdHVzO3N0YXR1cz1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiaW5kZXhTdGF0dXMiKTtzdGF0dXMuaW5uZXJUZXh0PXRleHR9ZnVuY3Rpb24gbXlSZXNldFN0YXR1cygpe3ZhciBzdGF0dXM7c3RhdHVzPWRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCJpbmRleFN0YXR1cyIpO3N0YXR1cy5pbm5lclRleHQ9IqAifWZ1bmN0aW9uIG15UnVuRmlsdGVyKCl7dmFyIGlucHV0LGZpbHRlcixmaWx0ZXIwLHVsLGxpLGEsaSx0eHRWYWx1ZSxtYXRjaFNlZW4sZmlyc3RNYXRjaDtpbnB1dD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiZmlsdGVyVGV4dCIpO2ZpbHRlcj1pbnB1dC52YWx1ZS50b1VwcGVyQ2FzZSgpO2ZpbHRlcjA9ZmlsdGVyLmNoYXJBdCgwKTt1bD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCgiaW5kZXhVTCIpO2xpPXVsLmdldEVsZW1lbnRzQnlUYWdOYW1lKCJsaSIpO2lmKGZpbHRlcj09IiIpe215UmVzZXRTdGF0dXMoKTtmb3IoaT0wO2k8bGkubGVuZ3RoO2krKyl7bGlbaV0uc3R5bGUuZGlzcGxheT0iIn19ZWxzZXttYXRjaFNlZW49MDtmaXJzdE1hdGNoPS0xO2ZvcihpPTA7aTxsaS5sZW5ndGg7aSsrKXthPWxpW2ldLmdldEVsZW1lbnRzQnlUYWdOYW1lKCJhIilbMF07dHh0VmFsdWU9YS50ZXh0Q29udGVudHx8YS5pbm5lclRleHQ7dHh0VmFsdWU9dHh0VmFsdWUudG9VcHBlckNhc2UoKTtpZih0eHRWYWx1ZS5jaGFyQXQoMCk9PWZpbHRlcjAmJnR4dFZhbHVlLnN0YXJ0c1dpdGgoZmlsdGVyKSl7aWYobXlMYXN0S2V5PT0xMyl7bXlSZXNldFN0YXR1cygpO2RvY3VtZW50LmxvY2F0aW9uLmhyZWY9YS5ocmVmO3JldHVybn1lbHNle2xpW2ldLnN0eWxlLmRpc3BsYXk9IiI7bWF0Y2hTZWVuKys7aWYoZmlyc3RNYXRjaD09LTEpZmlyc3RNYXRjaD1pfX1lbHNle2lmKG1hdGNoU2Vlbil7YnJlYWt9bGlbaV0uc3R5bGUuZGlzcGxheT0ibm9uZSJ9fWZvcig7aTxsaS5sZW5ndGg7aSsrKXtsaVtpXS5zdHlsZS5kaXNwbGF5PSJub25lIn1teVJlc2V0U3RhdHVzKCk7aWYobWF0Y2hTZWVuPT0xKXt9fX12YXIgbXlGaWx0ZXJGdW5jdGlvbj1teURlYm91bmNlKG15UnVuRmlsdGVyLG15RGVib3VuY2VEZWxheSk7dmFyIG15TGFzdEtleT0wO2Z1bmN0aW9uIG15RmlsdGVySG9vaygpe215TGFzdEtleT1ldmVudC5rZXlDb2RlO215U2V0U3RhdHVzKCJTZWFyY2hpbmcuLi4iKTtteUZpbHRlckZ1bmN0aW9uKCl9"]
                }
                error "Unknown asset: $fn"
            
}

proc ruff::private::ns_canonicalize {name} {
    return [regsub -all {:::*} $name ::]
}

proc ruff::private::fqn? {name} {
    # Returns `1` if $name is fully qualified, else `0`.
    return [string match ::* $name]
}

proc ruff::private::fqn! {name} {
    # Raises an error if $name is not a fully qualified name.
    if {![fqn? $name]} {
        error "\"name\" is not fully qualified."
    }
}

proc ruff::private::ns_qualifiers {fqn} {
    # This differs from namespace qualifiers in that
    # - it expects fully qualified names
    # - for globals it returns "::", not "" 
    fqn! $fqn
    set fqn [ns_canonicalize $fqn]
    set quals [namespace qualifiers $fqn]
    if {$quals ne "" || $fqn eq "::"} {
        return $quals
    }
    return ::
}

proc ruff::private::ns_member! {fqns name} {
    fqn! $fqns
    fqn! $name
    set parent [ns_qualifiers $name]
    if {$parent ne [ns_canonicalize $fqns]} {
        error "Name \"$name\" does not belong to the \"$fqns\" namespace."
    }
}

proc ruff::private::program_option {opt} {
    variable ProgramOptions
    return $ProgramOptions($opt)
}

proc ruff::private::sanitize_filename {s} {
    return [regsub -all {[^-\w_]} $s -]
}

proc ruff::private::ns_file_base {ns_or_class {ext {}}} {
    # Returns the file name to use for documenting namespace $ns.
    # ns_or_class - the namespace or class for the file
    # ext - if non-empty, this is used as the file extension.
    #  It should include the initial period.
    variable output_file_base
    variable output_file_ext
    variable ns_file_base_cache
    variable ProgramOptions

    # Methods can also be represented as Class::method so this is a
    # hack to get the real namespace and not the class name
    if {[info object isa class $ns_or_class]} {
        set ns [namespace qualifiers $ns_or_class]
    } else {
        set ns $ns_or_class
    }
    if {![info exists ns_file_base_cache($ns)]} {
        if {$ProgramOptions(-pagesplit) eq "none" || $ns eq "::"} {
            set fn "$output_file_base$output_file_ext"
        } else {
            set fn "${output_file_base}[regsub -all {:+|[^-\w_.]} $ns -]$output_file_ext"
        }
        set ns_file_base_cache($ns) $fn
    }
    if {$ext eq ""} {
        return $ns_file_base_cache($ns)
    } else {
        return "[file rootname $ns_file_base_cache($ns)]$ext"
    }
}

proc ruff::private::markup_escape {text} {
    # Escapes any characters that might be treated as special for markup.
    #  text - The text to escape.
    return [regsub -all {[\\\[\]`*_{}()#]} $text {\\\0}]
}

proc ruff::private::markup_emphasis {text} {
    # Returns the markup text for emphasis.
    #  text - The text to emphasize.

    return "*[markup_escape $text]*"

}
proc ruff::private::markup_reference {symbol} {
    # Returns the markup text for cross-referencing a symbol.
    #  symbol - the symbol to reference
    return "\[$symbol\]"
}

proc ruff::private::markup_code {text} {
    # Returns $text marked up as code using Ruff! syntax.
    #  text - String to markup.

    # If text contains backticks, markup is more complicated.
    if {[string first "`" $text] < 0} {
        return "`$text`"
    }

    # Find the longest consecutive sequence of `
    set matches [regexp -all -inline {`+} $text]
    set n 0
    foreach match $matches {
        if {[string length $match] > $n} {
            set n [string length $match]
        }
    }
    # Number of backticks required is one more than max length of matches
    set sep [string repeat ` [incr n]]
    # Need to separate with spaces.
    return "$sep $text $sep"
}

proc ruff::private::regexp_escape {s} {
    return [string map {
        \\ \\\\ $ \\$ ^ \\^ . \\. ? \\? + \\+ * \\*
        | \\| ( \\( ) \\) [ \\[ ] \\] \{ \\\{ \} \\\}
    } $s]
}

proc ruff::private::namespace_tree {nslist} {
    # Return list of namespaces under the specified namespaces
    array set done {}
    while {[llength $nslist]} {
        set ns [lindex $nslist 0]
        set nslist [lrange $nslist 1 end]
        if {[info exists done($ns)]} {
            # Already recursed this namespace
            continue
        }
        set done($ns) true
        lappend nslist {*}[namespace children $ns]
    }

    return [array names done]
}

proc ruff::private::trim_namespace {name ns} {
    # Removes a namespace (::) or class qualifier (.) from the specified name.
    # name - name from which the namespace is to be removed
    # ns - the namespace to be removed. If empty, $name
    #  is returned as is. To trim the root namespace
    #  pass :: as the value
    #
    # Returns the remaining string after removing $ns
    # from $name. If $name does not begin with $ns, returns
    # it as is.

    if {$ns eq ""} {
        # Note this check must come BEFORE the trim below
        return $name
    }

    # The "namespace" may be either a Tcl namespace or a class
    # in which case the separator is a "." and not ::
    set ns [string trimright $ns :.]
    set nslen [string length $ns]
    if {[string equal ${ns} [string range $name 0 [expr {$nslen-1}]]]} {
        # Prefix matches.
        set tail [string range $name $nslen end]
        # See if next chars are :: or .
        if {[string range $tail 0 1] eq "::"} {
            # Namespace
            return [string range $tail 2 end]
        }
        if {[string index $tail 0] eq "."} {
            # Class
            return [string range $tail 1 end]
        }
    }

    return $name
}

proc ruff::private::trim_namespace_multi {namelist ns} {
    # See trim_namespace for a description. Only difference
    # is that this command takes a list of names instead
    # of a single name.
    set result {}
    foreach name $namelist {
        lappend result [trim_namespace $name $ns]
    }
    return $result
}

proc ruff::private::symbol_ref {word} {
    # Wraps $word with `[]` to mark it as a markdown reference.
    # word - the word to be marked as reference.
    # Returns the marked-up word.
    return "\[$word\]"
}

proc ruff::private::symbol_refs {words} {
    # Wraps elements of a list with `[]` to mark them as markdown references.
    # words - the list of words to be marked as references.
    # Returns the list of marked-up words.
    return [lmap word $words {
        set word "\[$word\]"
    }]
}

proc ruff::private::symbol_refs_string {words {separator {, }}} {
    # Constructs a string from elements of a list with `[]` to mark them as markdown references
    # words - the list of words to be marked as references.
    # separator - string to use as separator between elements
    # Returns a string containing the marked-up words separated by $separator.
    return [join [symbol_refs $words] $separator]
}

proc ruff::private::ensembles {pattern} {
    # Returns list of ensembles matching the pattern
    # pattern - fully namespace qualified pattern to match

    return [lmap cmd [info commands $pattern] {
        if {![namespace ensemble exists $cmd]} {
            continue
        }
        set cmd
    }]
}


proc ruff::private::sift_names {names} {
    # Given a list of names, separates and sorts them based on their namespace
    # names - a list of names
    #
    # Returns a dictionary indexed by namespace names with corresponding
    # values being a sorted list of names belong to that namespace.

    set namespaces [dict create]
    foreach name [lsort -dictionary $names] {
        set ns [namespace qualifiers $name]
        dict lappend namespaces $ns $name
    }

    return $namespaces
}

proc ruff::private::parse_line {line mode current_indent}  {
    # Parses a documentation line and returns its meta information.
    # line - line to be parsed
    # mode - parsing mode, must be one of `proc`, `method`, `docstring`
    # current_indent   - the indent of the containing block

    if {![regexp -indices {^\s*(\S.*)$} $line -> indices]} {
        return [list Type blank Indent $current_indent Text ""]
    }
    # indices contains the indices of text after leading whitespace

    set indent [lindex $indices 0]
    set text   [string trimright [string range $line $indent end]]

    # Indent exceeds more than 4 beyond current indent plus the
    # continuation indent if any, it is preformatted.
    set preformatted_min_indent [expr {$current_indent+4}]
    if {$indent >= $preformatted_min_indent} {
        # Note we use $line here, not $text as we want to preserve trailing
        # and leading spaces except for the 4 spaces that mark it as preformatted.
        return [list Type preformatted \
                    Indent $indent \
                    Text [string range $line $preformatted_min_indent end]]
    }

    # Note that $text starts AND ends with a non-whitespace character.
    # Also note order of match cases in switch is importent.
    switch -regexp -matchvar matches -indexvar indices -- $text {
        {^(#+)\s+(\S.*)} {
            # = A Header
            return [list Type heading \
                        Indent $indent \
                        Level [string length [lindex $matches 1]] \
                        Text [lindex $matches 2]]
        }
        {^[-\*]\s+(.*)$} {
            # - a bullet list element
            # Return: bullet lineindent relativetextindent marker text
            return [list Type bullet \
                        Indent $indent \
                        RelativeIndent [lindex $indices 1 0] \
                        Marker [string index $text 0] \
                        Text [lindex $matches 1]]
        }
        {^(\S+)(\s+\S+)?\s+-\s+(.*)$} {
            # term ?term2? - description
            return [list Type definition \
                        Indent $indent \
                        RelativeIndent [lindex $indices 2 0] \
                        Term "[lindex $matches 1][lindex $matches 2]" \
                        Text [lindex $matches 3]]
        }
        {^(`{3,})(.*)$} {
            # ```` Fenced code block
            set fence_options [string trim [lindex $matches 2]]
            return [list Type fence Indent $indent \
                        Text [lindex $matches 1] \
                        Options $fence_options]
        }
        default {
            # Normal text line
            if {$mode ne "docstring"} {
                # Within procs and methods, look for special
                # proc-specific keywords
                if {[regexp {^See also\s*:\s*(.*)$} $line -> match]} {
                    return [list Type seealso Indent $indent Text $match]
                }
                if {[regexp {^Returns(\s*:)?($|\s.*$)} $line -> colon match]} {
                    if {$colon eq ""} {
                        # English text like
                        #   Returns some value
                        return [list Type returns Indent $indent Text $text]
                    } else {
                        # Possibly localized. The "Return" should not be part of text
                        return [list Type returns Indent $indent Text $match]
                    }
                }
                if {[regexp {^Synopsis\s*:\s*(.*)$} $line -> match]} {
                    return [list Type synopsis Indent $indent Text $match]
                }
            }
            if {$indent > $current_indent} {
                return [list Type continuation \
                            Indent $indent \
                            Text $text]
            } else {
                return [list Type normal\
                            Indent $indent \
                            Text $text]
            }
        }
    }
}

proc ruff::private::parse_preformatted_state {statevar} {
    upvar 1 $statevar state

    # Gobble up all lines that are indented starting
    # with the current line. Blank lines are included
    # even if they have fewer than the required indent.
    # However, leading blank lines and trailing blank
    # lines are not included even if they start
    # with leading 4 spaces.

    # Note the Text dictionary element already has
    # the leading 4 spaces stripped.

    set text [dict get $state(parsed) Text]
    unset state(parsed);    # Since we do not maintain this for further lines

    # If a blank line, do not treat as start of
    # preformatted section (Markdown compatibility).
    if {[regexp {^\s*$} $text]} {
        incr state(index)
        return
    }

    set code_block          [list $text]
    set intermediate_blanks [list ]
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        regexp -indices {^(\s*)} $line -> leader
        set nspaces [expr {[lindex $leader 1]+1}]
        if {$nspaces == [string length $line]} {
            # Empty or all blanks
            # Intermediate empty lines do not terminate block
            # even if not prefixed by 4 spaces. Collect them
            # to add later if more preformatted lines follow.
            lappend intermediate_blanks [string range $line 4 end]
        } elseif {$nspaces < 4} {
            # Not preformatted
            break
        } else {
            lappend code_block {*}$intermediate_blanks
            set intermediate_blanks {}
            lappend code_block [string range $line 4 end]
        }
    }
    set state(state) body
    lappend state(body) preformatted $code_block
}

proc ruff::private::parse_fence_options {option_line} {
    # Parses options for a fenced block
    #  option_line - the line containing fence options
    #
    # The line is of the form
    # ```
    # [option value ...] [command [arg ...]]
    # ```
    #
    # An option begins with the character `-`. The options end at the
    # first word that does not begin with `-` (skipping option values).
    # The returned dictionary maps each specified option to a value
    # with a special key -command holding the rest of the line, i.e. the
    # command and its arguments
    # 
    # Returns a dictionary of the option values.

    set n [llength $option_line]
    set options [dict create]
    for {set i 0} {$i < $n} {incr i} {
        set option [lindex $option_line $i]
        if {[string index $option 0] ne "-"} {
            # End of options
            break
        }
        if {[incr i] == $n} {
            error "Missing value to go with option \"[lindex $option_line $i]\" in diagram."
        }
        dict set options $option [lindex $option_line $i]
    }

    dict set options Command [lrange $option_line $i end]
    return $options
}

proc ruff::private::parse_fence_state {statevar} {
    upvar 1 $statevar state
    set marker [dict get $state(parsed) Text]
    set marker_indent  [dict get $state(parsed) Indent]
    set options_line [dict get $state(parsed) Options]
    set code_block {}

    # Gobble up any lines until the matching fence
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        # Explicit check is faster than calling parse_line
        # Note neither pandoc not cmark require marker indentation
        # to be the same.
        if {[regexp "^\s*$marker\s*$" $line]} {
            incr state(index);  # Inform caller which line to look at next
            unset state(parsed); # To indicate next line has not been parsed
            break;  # Found end marker
        } else {
            # Remove the indentation of original marker if present.
            # Smaller indentation is reduced to 0.
            regexp -indices {^(\s*)} $line -> spaces
            set start [lindex $spaces 1]; # Could be -1 also
            incr start
            if {$start < $marker_indent} {
                set line [string range $line $start end]
            } else {
                set line [string range $line $marker_indent end]
            }
            lappend code_block $line
        }
    }

    set fence_options [parse_fence_options $options_line]
    dict set fence_options Fence $marker
    # If there is a caption, create anchor for it
    if {[dict exists $fence_options -caption]} {
        $ruff::gFormatter CollectFigureReference $state(scope) [dict get $fence_options -caption]
    }

    lappend state(body) fenced [list $code_block $fence_options]
    set state(state) body
}

proc ruff::private::extract_seealso_symbols {symbols} {
    # symbols - text line with symbols optionally separated by commas and optional
    #   surrounding square brackets
    return [lmap symbol $symbols {
        set symbol [string trim $symbol ,]; # Permit commas between elements
        if {$symbol eq ""} {
            continue
        }
        set symbol
    }]
}

proc ruff::private::parse_seealso_state {statevar} {
    upvar 1 $statevar state

    if {$state(mode) eq "docstring"} {
        # parse_line should not have returned this for docstrings
        error "Internal error: Got seealso in docstring mode."
    }
    set block_indent [dict get $state(parsed) Indent]
    # The text is a list of symbols separated by spaces
    # and optionally commas.
    lappend state(seealso) {*}[extract_seealso_symbols [dict get $state(parsed) Text]]
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        switch -exact -- [dict get $state(parsed) Type] {
            heading -
            fence -
            bullet -
            definition -
            blank -
            synopsis -
            seealso -
            returns {
                break
            }
            normal {
                # If the indent is less than the block indent,
                # treat as a new paragraph.
                if {[dict get $state(parsed) Indent] < $block_indent} {
                    break
                }
                # Append symbols at bottom of loop
            }
            preformatted -
            continuation {
                # Append symbols at bottom of loop
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
        lappend state(seealso) {*}[extract_seealso_symbols [dict get $state(parsed) Text]]
    }
    set state(state) body
}

proc ruff::private::parse_synopsis_state {statevar} {
    upvar 1 $statevar state

    if {$state(mode) eq "docstring"} {
        # parse_line should not have returned this for docstrings
        error "Internal error: Got synopsis in docstring mode."
    }
    set block_indent [dict get $state(parsed) Indent]
    # The text is a list of parameter names separated by spaces.
    set param_names [dict get $state(parsed) Text]
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        switch -exact -- [dict get $state(parsed) Type] {
            heading -
            fence -
            bullet -
            definition -
            blank -
            synopsis -
            seealso -
            returns {
                break
            }
            normal {
                # If the indent is less than the block indent,
                # treat as a new paragraph.
                if {[dict get $state(parsed) Indent] < $block_indent} {
                    break
                }
                # Append symbols at bottom of loop
            }
            preformatted -
            continuation {
                # Append symbols at bottom of loop
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
        set text [dict get $state(parsed) Text]
        if {[llength $text]} {
            lappend param_names {*}$text
        }
    }
    if {[llength $param_names]} {
        lappend state(synopsis) $param_names
    }
    set state(state) body
}


proc ruff::private::parse_returns_state {statevar} {
    upvar 1 $statevar state

    set block_indent [dict get $state(parsed) Indent]
    set lines [list [dict get $state(parsed) Text]]
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        set text [dict get $state(parsed) Text]
        switch -exact -- [dict get $state(parsed) Type] {
            heading -
            fence -
            bullet -
            definition -
            blank -
            seealso -
            synopsis -
            preformatted -
            returns {
                # All special lines terminate normal paragraphs
                break
            }
            continuation {
                lappend lines $text
            }
            normal {
                # If the indent is less than the block indent,
                # treat as a new paragraph.
                if {[dict get $state(parsed) Indent] < $block_indent} {
                    break
                }
                lappend lines $text
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
    }
    if {[llength $lines]} {
        if {$state(mode) eq "docstring"} {
            lappend state(body) paragraph $lines
        } else {
            lappend state(returns) {*}$lines
        }
    }
    if {$state(mode) ne "docstring" && $state(state) eq "init"} {
        set state(state) postsummary
    } else {
        set state(state) body
    }
}

proc ruff::private::parse_bullets_state {statevar} {
    upvar 1 $statevar state

    set list_block {}
    set list_elem [list [dict get $state(parsed) Text]]
    set marker    [dict get $state(parsed) Marker]
    set block_indent [dict get $state(parsed) Indent]

    # between_bullets keeps track of blank lines. If a list item follow
    # a sequence of blank lines, it continues the list. Any other line
    # type will terminate the list.
    set between_bullets false
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        set text [dict get $state(parsed) Text]
        switch -exact -- [dict get $state(parsed) Type] {
            heading -
            returns -
            fence -
            definition -
            synopsis -
            seealso {
                # If we are between bullets, this does not continue the list.
                if {$between_bullets} {
                    break
                }
                if {[dict get $state(parsed) Indent] <= $block_indent} {
                    # List element and list terminated if a block starter
                    # appears at the same or less indentation. Note this is
                    # DIFFERENT from normal lines which add to the list
                    # item if at the same indent level.
                    break
                }
                # Note cannot use $text here since that will not contain
                # the full line for these elements
                lappend list_elem [string trimleft $line]
            }
            continuation {
                lappend list_elem $text
                set between_bullets false
            }
            normal {
                # If we are between bullets, this does not continue the list.
                if {$between_bullets} {
                    break
                }

                # If the indent is less than that of list element
                # treat as a new paragraph. This differs from Markdown
                # which treats it as part of the list item.
                if {[dict get $state(parsed) Indent] < $block_indent} {
                    break
                }
                lappend list_elem $text
            }
            preformatted {
                # If we are between bullets, this does not continue the list.
                if {$between_bullets} {
                    break
                }
                # As in markdown list continuation prioritized over preformatted
                lappend list_elem [string trim $text]
            }
            blank {
                # Current list item is terminated but not the list.
                # The check for list_elem length is to deal with
                # multiple consecutive blank lines. These should not
                # result in spurious list items.
                if {[llength $list_elem]} {
                    lappend list_block $list_elem
                    set list_elem {}
                }
                set between_bullets true
            }
            bullet {
                if {[dict get $state(parsed) Marker] ne $marker} {
                    break;      # Different list item type
                }
                if {[llength $list_elem]} {
                    lappend list_block $list_elem
                }
                set list_elem [list $text]
                set between_bullets false
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
    }

    if {[llength $list_elem]} {
        lappend list_block $list_elem
    }

    lappend state(body) bullets $list_block
    set state(state) body
}

proc ruff::private::parse_definitions_state {statevar} {
    upvar 1 $statevar state

    set definition_block {}
    set term         [dict get $state(parsed) Term]
    set definition   [list [dict get $state(parsed) Text]]
    set block_indent [dict get $state(parsed) Indent]

    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        set type [dict get $state(parsed) Type]
        # If $term is empty, then this line just followed a blank line.
        # In that case, we continue with the definition list only
        # if the line is a definition format or is itself blank.
        if {$type ni { definition blank } && ![info exists term]} {
            break
        }
        set text [dict get $state(parsed) Text]
        switch -exact -- $type {
            heading -
            returns -
            fence -
            bullet -
            synopsis -
            seealso {
                if {[dict get $state(parsed) Indent] <= $block_indent} {
                    # List element and list terminated if a block starter
                    # appears at the same or less indentation. Note this is
                    # DIFFERENT from normal lines which add to the list
                    # item if at the same indent level.
                    break
                }
                # Note cannot use $text here since that will not contain
                # the full line for these elements
                lappend definition [string trimleft $line]
            }
            continuation {
                lappend definition $text
            }
            normal {
                # If the indent is less than that of list element
                # treat as a new paragraph. This differs from Markdown
                # which treats it as part of the list item.
                if {[dict get $state(parsed) Indent] < $block_indent} {
                    break
                }
                lappend definition $text
            }
            preformatted {
                # As in markdown list continuation prioritized over preformatted
                lappend definition [string trim $text]
            }
            blank {
                # Current definition is terminated but not the list.
                # The check for term is to deal with
                # multiple consecutive blank lines. These should not
                # result in spurious items.
                if {[info exists term]} {
                    lappend definition_block [dict create term $term definition $definition]
                    unset term
                }
            }
            definition {
                if {[info exists term]} {
                    lappend definition_block [dict create term $term definition $definition]
                }
                set term       [dict get $state(parsed) Term]
                set definition [list [dict get $state(parsed) Text]]
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
    }

    if {[info exists term]} {
        lappend definition_block [dict create term $term definition $definition]
    }

    if {$state(mode) ne "docstring" && $state(state) in {init postsummary}} {
        set state(state) body
        set state(parameters) $definition_block
    } else {
        lappend state(body) definitions $definition_block
    }
}

proc ruff::private::parse_normal_state {statevar} {
    upvar 1 $statevar state

    set block_indent [dict get $state(parsed) Indent]
    set paragraph [list [dict get $state(parsed) Text]]
    while {[incr state(index)] < $state(nlines)} {
        set line [lindex $state(lines) $state(index)]
        set state(parsed) [parse_line $line $state(mode) $block_indent]
        switch -exact -- [dict get $state(parsed) Type] {
            heading -
            fence -
            bullet -
            definition -
            blank -
            synopsis -
            seealso -
            preformatted -
            returns {
                # All special lines terminate normal paragraphs
                break
            }
            continuation -
            normal {
                # Append text at bottom
            }
            default {
                error "Unexpected type [dict get $state(parsed) Type]"
            }
        }
        lappend paragraph [string trim [dict get $state(parsed) Text]]
    }
    if {$state(mode) ne "docstring" && $state(state) eq "init"} {
        set state(summary) $paragraph
        set state(state) postsummary
    } else {
        set state(state) body
        lappend state(body) paragraph $paragraph
    }
}

proc ruff::private::parse_lines {lines scope {mode proc}} {
    # Creates a documentation parse structure.
    # lines - List of lines comprising the documentation
    # scope - scope (generally fqns)
    # mode - Parsing mode, must be one of `proc`, `method`, `docstring`
    #
    # Returns a dictionary representing the documentation.
    #
    # The parse structure is a dictionary with the following keys:
    # summary - Contains the summary paragraph.
    #           Not applicable if $mode is `docstring`.
    # parameters - List of parameter name and description paragraph pairs.
    #           Not applicable if $mode is `docstring`.
    # body - The main body stored as a list of alternating type and
    #        content elements. The type may be one of `heading`,
    #        `paragraph`, `list`, `definitions` or `preformatted`.
    # seealso - The *See also* section containing a list of program element
    #           references. Not applicable if $mode is `docstring`.
    # returns - A paragraph describing the return value.
    #           Not applicable if $mode is `docstring`.
    # synopsis - a list of alternating procname and parameter list
    #           definitions to be used as synopsis instead of the generated
    #           one.
    #            
    # Not all elements may be present in the dictionary.
    # A paragraph is returned as a list of lines.

    # The parsing engine is distributed among procedures that carry
    # state around in the state array.

    set state(state)  init
    set state(scope)  $scope
    set state(mode)   $mode
    set state(lines)  $lines
    set state(nlines) [llength $lines]
    set state(index)  0
    set state(body)  {};        # list of alternating type and content
    # Following may be set during parsing
    # set state(summary) {};      # Summary paragraph
    # set state(returns)  {};     # list of paragraphs
    # set state(seealso) {};      # list of symbol references
    # set state(synopsis) {};     # list of command and param list elements
    # set state(parameters) {};   # Parameter definition list

    while {$state(index) < $state(nlines)} {
        # The loop is structured such that the outer loop detects block
        # starts and then for each block type the state function
        # slurps in all lines for that block.
        if {![info exists state(parsed)]} {
            set state(parsed) [parse_line \
                                   [lindex $state(lines) $state(index)] \
                                   $state(mode) \
                                   0]
        }
        set state(block_indent) [dict get $state(parsed) Indent]
        # All state procs expect state(parsed) to contain the
        # parsed format of the line at position state(index) that causes
        # transition to that state.
        switch -exact -- [dict get $state(parsed) Type] {
            blank {
                incr state(index)
                unset state(parsed)
            }
            heading {
                # Headings have to be on a single line
                lappend state(body) heading [list [dict get $state(parsed) Level] [dict get $state(parsed) Text]]
                incr state(index)
                unset state(parsed)
                set state(state) body
            }
            bullet       { parse_bullets_state state }
            definition   { parse_definitions_state state }
            returns      { parse_returns_state state }
            seealso      { parse_seealso_state state }
            synopsis      { parse_synopsis_state state }
            continuation {
                # TBD - See if we can get rid of continuation state
                # we do not really use this state.
                parse_normal_state state
            }
            normal       { parse_normal_state state }
            preformatted { parse_preformatted_state state }
            fence        { parse_fence_state state }
            default {
                error "Internal error: Unknown or unexpected line type\
                       \"[dict get $state(parsed) Type]\" returned in top-level\
                       parse of line \"[lindex $state(lines) $state(index)]\"."
            }
        }
    }

    set result [dict create body $state(body)]
    foreach elem {summary parameters seealso synopsis returns} {
        if {[info exists state($elem)]} {
            dict set result $elem $state($elem)
        }
    }
    return $result
}

proc ruff::private::distill_docstring {text} {
    # Splits a documentation string to return the documentation lines
    # as a list.
    # text - documentation string to be parsed
    #
    # If any tabs are present, they are replaced with spaces assuming
    # a tab stop width of 8.
    
    set lines {}
    set state init
    foreach line [split $text \n] {
        set line [textutil::tabify::untabify2 $line]
        if {[regexp {^\s*$} $line]} {
            #ruff
            # Initial blank lines are skipped and 
            # multiple empty lines are compressed into one empty line.
            if {$state eq "collecting"} {
                lappend lines ""
                set state empty
            }
            continue
        }
        #ruff
        # The very first non empty line determines the margin. This will
        # be removed from all subsequent lines. Note that this assumes that
        # if tabs are used for indentation, they are used on all lines
        # in consistent fashion.
        if {$state eq "init"} {
            regexp {^(\s*)\S} $line dontcare prefix
            set prefix_len [string length $prefix]
        }
        set state collecting

        # Remove the prefix if it exists from the line
        if {[string match ${prefix}* $line]} {
            set line [string range $line $prefix_len end]
        }

        lappend lines $line
    }

    # Returns a list of lines.
    return $lines
}

proc ruff::private::distill_body {text} {
    # Given a procedure or method body,
    # returns the documentation lines as a list.
    # text - text to be processed to collect all documentation lines.
    #
    # The first block of contiguous comment lines preceding the 
    # first line of code are treated as documentation lines.
    # If any tabs are present, they are replaced with spaces assuming
    # a tab stop width of 8.
    set lines {}
    set state init;             # init, collecting or searching
    foreach line [split $text \n] {
        set line [textutil::tabify::untabify2 $line]
        set line [string trim $line]; # Get rid of whitespace
        if {$line eq ""} {
            # Blank lines.
            # If in init state, we will stay in init state
            if {$state ne "init"} {
                set state searching
            }
            continue
        }

        if {[string index $line 0] ne "#"} {
            # Not a comment
            set state searching
            continue
        }

        # At this point, the line is a comment line
        if {$state eq "searching"} {
            #ruff
            # The string #ruff at the beginning of a comment line
            # anywhere in the passed in text is considered the start
            # of a documentation block. All subsequent contiguous
            # comment lines are considered documentation lines.
            if {[string match "#ruff*" $line]} {
                set state collecting
                #ruff
                # Note a #ruff on a line by itself will terminate
                # the previous text block.
                set line [string trimright $line]
                if {$line eq "#ruff"} {
                    lappend lines {}
                } else {
                    #ruff If #ruff is followed by additional text
                    # on the same line, it is treated as a continuation
                    # of the previous text block.
                    lappend lines [string range $line 6 end]
                }
            }
        } else {
            # State is init or collecting

            if {$line eq "#"} {
                # Empty comment line
                lappend lines {}
                continue;       # No change in state
            }

            #ruff
            # The leading comment character and a single space (if present)
            # are trimmed from the returned lines.
            if {[string index $line 1] eq " "} {
                lappend lines [string range $line 2 end]
            } else {
                lappend lines [string range $line 1 end]
            }
            set state collecting
            continue
        }
    }

    # Returns a list of lines that comprise the raw documentation.
    return $lines
}

proc ruff::private::extract_docstring {text scope} {
    # Parses a documentation string to return a structured text representation.
    # text - documentation string to be parsed
    # scope - the scope of the text (generally fqns)
    #
    # The command extracts structured text from the given string
    # as described in the documentation for the distill_docstring
    # and parse commands. The result is further processed to
    # return a list of type and value elements described below:
    #
    # heading   - The corresponding value is a list comprising the heading level
    #             and text.
    # paragraph - The corresponding values is a list containing the lines
    #             for that paragraph.
    # list      - The corresponding value is a list of lists with the outer
    #             list elements being the list items the contents of which
    #             are the lines.
    # definitions - The corresponding value is a list of dictionaries, each
    #             with the keys `term` and `definition`, the latter being
    #             the list of lines making up the definition.
    # preformatted - The corresponding value is a list of lines that should
    #             not be formatted.
    #
    # Each element may occur multiple times and are expected to be displayed
    # in the order of their occurence.

    set doc [parse_lines [distill_docstring $text] $scope docstring]
    set result [dict get $doc body]
    # Just error checking - should not have anykeys other than body
    dict unset doc body
    if {[llength [dict keys $doc]]} {
        app::log_error "Internal error: docstring contains unexpected keys [join [dict keys $doc]]."
    }
    return $result
}

proc ruff::private::extract_proc {procname} {

    # Extracts meta information from a Tcl procedure.
    # procname - name of the procedure
    #
    # The command retrieves metainformation about
    # a Tcl procedure. See the command extract_proc_or_method
    # for details.
    #
    # Returns a dictionary containing metainformation for the command.
    #

    set param_names [info args $procname]
    set param_defaults {}
    foreach name $param_names {
        if {[info default $procname $name val]} {
            lappend param_defaults $name $val
        }
    }
    return [extract_proc_or_method proc \
                $procname \
                [info args $procname] \
                $param_defaults \
                [info body $procname]]
}

proc ruff::private::extract_ensemble {ens} {
    # Extracts metainformation for all subcommands in an ensemble command
    # ens - fully qualified names of the ensemble command
    #
    # Only ensemble commands that satisfy the following are supported:
    # - the ensemble implementation must be in the form of Tcl procedures
    # - the ensemble must not have been configured with the `-parameters`
    #   option as that changes location of arguments
    #
    # Each element of the returned list is of the form returned by [extract_proc]
    # with two changes. The `name` key in the dictionary element is the
    # includes the ensemble name. Secondly, an additional key `ensemble` is
    # added to indicate which ensemble the element belongs to.
    #
    # Returns a list of elements each of the form returned by [extract_proc].

    array set ens_config [namespace ensemble configure $ens]
    if {[llength $ens_config(-parameters)]} {
        app::log_error "Skipping ensemble command $ens (non-empty -parameters attribute)."
    }

    if {[llength $ens_config(-subcommands)]} {
        set cmds $ens_config(-subcommands)
    } elseif {[dict size $ens_config(-map)]} {
        set cmds [dict keys $ens_config(-map)]
    } else {
        set exported [namespace eval $ens_config(-namespace) {namespace export}]
        set cmds {}
        foreach pat $exported {
            foreach cmd [info commands ${ens_config(-namespace)}::$pat] {
                lappend cmds [namespace tail $cmd]
            }
        }
    }
    set ens_subcmds {}
    set ens_cmds [lmap cmd $cmds {
        if {[dict exists $ens_config(-map) $cmd]} {
            set real_cmd [dict get $ens_config(-map) $cmd]
        } else {
            set real_cmd $cmd
        }
        if {![string match ::* $real_cmd]} {
            set real_cmd "${ens_config(-namespace)}::$real_cmd"
        }
        if {[info procs $real_cmd] ne "$real_cmd"} {
            app::log_error "Skipping subcommand \"$cmd\" for ensemble \"$ens\"\
                            because it is not a procedure."
            continue
        }
        if {[catch {extract_proc $real_cmd} result]} {
            app::log_error "Could not retrieve information for \"$real_cmd\"\
                            implementing ensemble command \"$ens $cmd\": $result"
            continue
        }
        dict set ens_subcmds $cmd real_cmd "$ens $cmd"
        if {[dict exists $result summary]} {
            dict set ens_subcmds $cmd summary [dict get $result summary]
        } elseif {[dict exists $result returns]} {
            dict set ens_subcmds $cmd summary [dict get $result returns]
        } else {
            dict set ens_subcmds $cmd summary "Subcommand"
        }
        dict set result name "$ens $cmd"
        dict set result ensemble $ens
        set result
    }]

    # Construct the documentation for the main ensemble command

    set subcmds [lsort -dictionary [dict keys $ens_subcmds]]
    set subcmd_list [join [lmap subcmd $subcmds {
        return -level 0 "\[$subcmd\]\[[dict get $ens_subcmds $subcmd real_cmd]\]"
    }] ", "]

    set body [list ]
    set definitions [lmap subcmd $subcmds {
        list term "\[$subcmd\]\[[dict get $ens_subcmds $subcmd real_cmd]\]" definition [dict get $ens_subcmds $subcmd summary]
    }]
    lappend body paragraph "The ensemble supports the following subcommands:"
    lappend body definitions $definitions
    lappend body paragraph "Refer to the documentation of each subcommand for details."

    dict set ens_info name $ens
    dict set ens_info body $body
    dict set ens_info summary "A command ensemble."
    dict set ens_info parameters \
        [list \
             [list term subcmd definition "One of $subcmd_list" type parameter] \
             [list term args definition "Subcommand arguments" type parameter]]
    dict set ens_info parameters {}
    dict set ens_info synopsis [list "subcommand ..."]
    dict set ens_info class {}
    dict set ens_info proctype proc
    #dict set ens_info source "# $ens ensemble command"

    return [linsert $ens_cmds[set ens_cmds {}] 0 $ens_info]

}

proc ruff::private::extract_ooclass_method {class method} {

    # Extracts metainformation for the method in oo:: class
    # class - name of the class
    #
    # The command retrieves metainformation about
    # a Tcl class method. See the command extract_proc_or_method
    # for details.
    #
    # Returns a dictionary containing documentation related to the command.
    #


    switch -exact -- $method {
        constructor {
            foreach {params body} [info class constructor $class] break
        }
        destructor  {
            set body [info class destructor $class]
            set params {}
        }
        default {
            foreach {params body} [info class definition $class $method] break
        }
    }


    set param_names {}
    set param_defaults {}
    foreach param $params {
        lappend param_names [lindex $param 0]
        if {[llength $param] > 1} {
            lappend param_defaults [lindex $param 0] [lindex $param 1]
        }
    }

    return [extract_proc_or_method method $method $param_names $param_defaults $body $class]
}


proc ruff::private::extract_proc_or_method {proctype procname param_names
                                            param_defaults body {class ""}} {
    # Helper procedure used by extract_proc and extract_ooclass_method to
    # construct metainformation for a method or proc.
    #  proctype - should be either 'proc' or 'method'
    #  procname - name of the proc or method
    #  param_names - list of parameter names in order
    #  param_defaults - list of parameter name and default values
    #  body - the body of the proc or method
    #  class - the name of the class to which the method belongs. Not used
    #   for proc types.
    #
    # The command parses the $body parameter as described by the distill_body
    # and parse commands and then constructs the metainformation for
    # the proc or method using this along with the other passed arguments.
    # The metainformation is returned as a dictionary with the following keys:
    #  name - name of the proc or method
    #  parameters - a list of parameters. Each element of the
    #   list is a dictionary with keys term, definition and optionally default.
    #  body - a list of paragraphs describing the command. The
    #   list contains heading, preformatted, paragraph, list and definitions
    #   as described for the [extract_docstring] command.
    #  returns - a description of the return value of the command (optional)
    #  summary - a copy of the first paragraph if it was present (optional)
    #  source - the source code of the command (optional)
    #  seealso - the corresponding value is a list of symbols (optional).
    #  synopsis - the synopsis to use instead of the generated one for the proc

    variable ProgramOptions

    array set param_default $param_defaults
    array set params {}
    array set options {}
    set paragraphs {}

    # XXX
    set doc [parse_lines [distill_body $body] :: $proctype]
    # doc -> dictionary with keys summary, body, parameters, returns, seealso
    # and synopsis
    dict set doc name $procname
    dict set doc class $class
    dict set doc proctype $proctype

    # Match up the parameter docs with the passed in parameter info.
    # First collect the documented parameters
    if {[dict exists $doc parameters]} {
        foreach param [dict get $doc parameters] {
            # param is a dict with keys term and definition
            set name [dict get $param term]
            set params($name) $param
        }
    }

    # Construct parameter descriptions. Note those not listed in the
    # actual proc definition are left out even if they are in the params
    # table
    set parameters {}
    foreach name $param_names {
        if {[info exists params($name)]} {
            set paramdata $params($name)
            unset params($name)
        } else {
            if {$name eq "args"} {
                set definition ""
            } else {
                set definition "Not documented."
            }
            set paramdata [dict create term $name definition $definition type parameter]
        }

        # Check if there is a default
        if {[info exists param_default($name)]} {
            dict set paramdata default $param_default($name)
        }

        dict set paramdata type parameter
        lappend parameters $paramdata
    }

    # Add any left over parameters from the documentation in sorted
    # order.
    foreach name [lsort -dictionary [array names params]] {
        dict set params($name) type "option"; # Assume option since not in proc definition
        lappend parameters $params($name)
    }
    dict set doc parameters $parameters

    # TBD - do we need to extract source even if -includesource is not specified
    set source "$proctype $procname "
    set param_list {}
    foreach name $param_names {
        if {[info exists param_default($name)]} {
            lappend param_list [list $name $param_default($name)]
        } else {
            lappend param_list $name
        }
    }


    append source "{$param_list} {\n"
    # We need to reformat the body. If nested inside a namespace eval
    # for example, the body will be indented too much. So we undent the
    # least indented line to 0 spaces and then add 4 spaces for each line.
    append source [::textutil::adjust::indent [::textutil::adjust::undent $body] "    "]
    append source "\n}"
    if {$ProgramOptions(-hidesourcecomments)} {
        regsub -line -all {^\s*#.*$} $source "" source
        regsub -all {\n{2,}} $source "\n" source
    }
    dict set doc source $source
    return $doc
}


proc ruff::private::extract_ooclass {classname args} {
    # Extracts metainformation about the specified class
    # classname - name of the class to be documented
    # -includeprivate BOOLEAN - if true private methods are also included
    #  in the metainformation. Default is false.
    #
    # The metainformation. returned is in the form of a dictionary with
    # the following keys:
    # name - name of the class
    # methods - a list of method definitions for this class in the form
    #  returned by extract_ooclass_method with the additional key
    #  'visibility' which may have values 'public' or 'private'.
    # external_methods - a list of names of methods that are
    #  either inherited or mixed in
    # filters - a list of filters defined by the class
    # forwards - a list of forwarded methods, each element in the
    #  list being a dictionary with keys 'name' and 'forward'
    #  corresponding to the forwarded method name and the forwarding command.
    # mixins - a list of names of classes mixed into the class
    # superclasses - a list of names of classes which are direct
    #   superclasses of the class
    # subclasses - a list of classes which are direct subclasses of this class
    # constructor - method definition for the constructor in the format
    #   returned by extract_ooclass_method
    # destructor - method definition for the destructor
    #   returned by extract_ooclass_method
    #
    # Each method definition is in the format returned by the
    # extract_ooclass_method command with an additional keys:
    # visibility - indicates whether the method is 'public' or 'private'

    array set opts {-includeprivate false}
    array set opts $args

    set result [dict create methods {} external_methods {} \
                    filters {} forwards {} \
                    mixins {} superclasses {} subclasses {} \
                    name $classname \
                   ]

    if {$opts(-includeprivate)} {
        set all_local_methods [info class methods $classname -private]
        set all_methods [info class methods $classname -all -private]
    } else {
        set all_local_methods [info class methods $classname]
        set all_methods [info class methods $classname -all]
    }
    set public_methods [info class methods $classname -all]
    set external_methods {}

    foreach name [lsort -dictionary $all_methods] {
        set implementing_class [locate_ooclass_method $classname $name]
        if {[lsearch -exact $all_local_methods $name] < 0} {
            # Skip the destroy method which is standard and
            # appears in all classes.
            if {$implementing_class ne "::oo::object" &&
                $name ne "destroy"} {
                lappend external_methods [list $name $implementing_class]
            }
            continue
        }

        # Even if a local method, it may be hidden by a mixin
        if {$implementing_class ne $classname} {
            # TBD - should we make a note in the documentation somewhere ?
            app::log_error "Method $name in class $classname is hidden by class $implementing_class."
        }

        if {[lsearch -exact $public_methods $name] >= 0} {
            set visibility public
        } else {
            set visibility private
        }

        if {! [catch {
            set method_info [extract_ooclass_method $classname $name]
        } msg]} {
            dict set method_info visibility $visibility
            #dict set method_info name $name
            dict lappend result methods $method_info
        } else {
            # Error, may be it is a forwarded method
            if {! [catch {
                set forward [info class forward $classname $name]
            }]} {
                dict lappend result forwards [dict create name $name forward $forward]
            } else {
                ruff::app::log_error "Could not introspect method $name in class $classname"
            }
        }
    }

    foreach name {constructor destructor} {
        if {[info class $name $classname] ne ""} {
            # Class has non-empty constructor or destructor
            dict set result $name [extract_ooclass_method $classname $name]
        }
    }

    dict set result name $classname;   # TBD - should we fully qualify this?
    dict set result external_methods $external_methods
    dict set result filters [info class filters $classname]
    dict set result mixins [info class mixins $classname]
    dict set result subclasses [info class subclasses $classname]
    # We do not want to list ::oo::object which is a superclass
    # of all classes.
    set classes {}
    foreach class [info class superclasses $classname] {
        if {$class ne "::oo::object"} {
            lappend classes $class
        }
    }
    dict set result superclasses $classes

    return $result
}

proc ruff::private::get_metaclasses {} {
    set metaclasses [list ]
    set pending [list ::oo::class]
    while {[llength $pending]} {
        set pending [lassign $pending metaclass]
        lappend metaclasses $metaclass
        # Add subclasses of the metaclass as metaclasses
        lappend pending {*}[info class subclasses $metaclass]
    }
    return $metaclasses
}

proc ruff::private::extract_procs_and_classes {pattern args} {
    # Extracts metainformation for procs and classes 
    #
    # pattern - glob-style pattern to match against procedure and class names
    # -excludeclasses REGEXP - If specified, any classes whose names
    #  match `REGEXPR` will not be included in the documentation.
    # -excludeprocs REGEXP - If specified, any procedures whose names
    #  match `REGEXPR` will not be included in the documentation.
    # -include LIST - `classes` and / or `procs` depending on whether one
    #     or both are to be collected.
    # -includeprivate BOOLEAN - if true private methods are also included.
    #  Default is false.
    # -includeimports BOOLEAN - if true commands imported from other
    #  namespaces are also included. Default is false.
    #
    # The value of the classes key in the returned dictionary is
    # a dictionary whose keys are class names and whose corresponding values
    # are in the format returned by extract_ooclass.
    # Similarly, the procs key contains a dictionary whose keys
    # are proc names and whose corresponding values are in the format
    # as returned by extract_proc.
    #
    # Note that only the program elements in the same namespace as
    # the namespace of $pattern are returned.
    #
    # Returns a dictionary with keys 'classes' and 'procs'
    array set opts {
        -excludeclasses {}
        -excludeprocs {}
        -include {procs classes}
        -includeprivate false
        -includeimports false
    }
    array set opts $args

    set classes [dict create]
    if {"classes" in $opts(-include)} {
        set class_names [list ]
        foreach metaclass [get_metaclasses] {
            lappend class_names {*}[info class instances $metaclass $pattern]
        }
        foreach class_name $class_names {
            # This covers child namespaces as well which we do not want
            # so filter those out. The differing pattern interpretations in
            # Tcl commands 'info class instances' and 'info procs'
            # necessitates this.
            if {[namespace qualifiers $class_name] ne [namespace qualifiers $pattern]} {
                # Class is in not in desired namespace
                # TBD - do we need to do -includeimports processing here?
                continue
            }
            if {$opts(-excludeclasses) ne "" &&
                [regexp $opts(-excludeclasses) [namespace tail $class_name]]} {
                continue
            }

            if {[catch {
                set class_info [extract_ooclass $class_name -includeprivate $opts(-includeprivate)]
            } msg]} {
                app::log_error "Could not document class $class_name: $msg"
            } else {
                dict set classes $class_name $class_info
            }
        }
    }

    set procs [dict create]
    if {"procs" in $opts(-include)} {
        # Collect procs
        foreach proc_name [info procs $pattern] {
            if {$opts(-excludeprocs) ne "" &&
                [regexp $opts(-excludeprocs) [namespace tail $proc_name]]} {
                continue
            }
            if {(! $opts(-includeimports)) &&
                [namespace origin $proc_name] ne $proc_name} {
                continue;       # Do not want to include imported commands
            }

            if {[catch {
                set proc_info [extract_proc $proc_name]
            } msg]} {
                app::log_error "Could not document proc $proc_name: $msg"
            } else {
                dict set procs $proc_name $proc_info
            }
        }
        # Collect ensembles
        foreach ens_name [ensembles $pattern] {
            if {$opts(-excludeprocs) ne "" &&
                [regexp $opts(-excludeprocs) [namespace tail $ens_name]]} {
                continue
            }
            if {(! $opts(-includeimports)) &&
                [namespace origin $ens_name] ne $ens_name} {
                continue;       # Do not want to include imported commands
            }

            if {[catch {
                set ens_cmds [extract_ensemble $ens_name]
            } msg]} {
                app::log_error "Could not document ensemble command $ens_name: $msg"
            } else {
                foreach ens_info $ens_cmds {
                    dict set procs [dict get $ens_info name] $ens_info
                }
            }
        }
    }

    return [dict create classes $classes procs $procs]
}


proc ruff::private::extract_namespace {ns args} {
    # Extracts metainformation for procs and objects in a namespace
    # ns - namespace to examine
    #
    # Any additional options are passed on to the extract command.
    #
    # Returns a dictionary containing information for the namespace.

    # The returned dictionary has keys `preamble`, `classes` and `procs`.
    # See [extract_docstring] for format of the `preamble` value
    # and [extract_procs_and_classes] for the others.

    # Note the canonicalize is required to handle ns == "::" which
    # will create :::: in matching pattern otherwise
    set pattern [ns_canonicalize ${ns}::*]
    set result [extract_procs_and_classes $pattern {*}$args]
    set preamble [list ]
    if {[info exists ${ns}::_ruff_preamble]} {
        set preamble [extract_docstring [set ${ns}::_ruff_preamble] $ns]
    } elseif {[info exists ${ns}::_ruffdoc]} {
        foreach {heading text} [set ${ns}::_ruffdoc] {
            lappend preamble {*}[extract_docstring "## $heading" $ns]
            lappend preamble {*}[extract_docstring $text $ns]
        }
    }
    dict set result preamble $preamble
    return $result
}

proc ruff::private::extract_namespaces {namespaces args} {
    # Extracts metainformation for procs and objects in one or more namespace
    # namespaces - list of namespace to examine
    #
    # Any additional options are passed on to the extract_namespace command.
    #
    # The dictionary returned is keyed by namespace with nested
    # keys 'classes' and 'procs'. See [extract] for details.
    #
    # Returns a dictionary with the namespace information.

    set result [dict create]
    foreach ns $namespaces {
        dict set result $ns [extract_namespace $ns {*}$args]
    }
    return $result
}


proc ruff::private::get_ooclass_method_path {class_name method_name} {
    # Calculates the class search order for a method of the specified class
    # class_name - name of the class to which the method belongs
    # method_name - method name being searched for
    #
    # A method implementation may be provided by the class itself,
    # a mixin or a superclass.
    # This command calculates the order in which these are searched
    # to locate the method. The primary purpose is to find exactly
    # which class actually implements a method exposed by the class.
    #
    # If a class occurs multiple times due to inheritance or
    # mixins, the LAST occurence of the class is what determines
    # the priority of that class in method selection. Therefore
    # the returned search path may contain repeated elements.
    #
    # Note that this routine only applies to a class and cannot be
    # used with individual objects which may have their own mix-ins.


    # TBD - do we need to distinguish private/public methods

    set method_path {}
    #ruff
    # Search algorithm:
    #  - Filters are ignored. They may be invoked but are not considered
    #    implementation of the method itself.
    #  - The mixins of a class are searched even before the class itself
    #    as are the superclasses of the mixins.
    foreach mixin [info class mixins $class_name] {
        # We first need to check if the method name is in the public interface
        # for this class. This step is NOT redundant since a derived
        # class may unexport a method from an inherited class in which
        # case we should not have the inherited classes in the path
        # either.
        if {[lsearch -exact [info class methods $mixin -all -private] $method_name] < 0} {
            continue
        }

        set method_path [concat $method_path [get_ooclass_method_path $mixin $method_name]]
    }

    #ruff - next in the search path is the class itself
    if {[lsearch -exact [info class methods $class_name -private] $method_name] >= 0} {
        lappend method_path $class_name
    }

    #ruff - Last in the search order are the superclasses (in recursive fashion)
    foreach super [info class superclasses $class_name] {
        # See comment in mixin code above.
        if {[lsearch -exact [info class methods $super -all -private] $method_name] < 0} {
            continue
        }
        set method_path [concat $method_path [get_ooclass_method_path $super $method_name]]
    }


    #ruff
    # Returns an ordered list containing the classes that are searched
    # to locate a method for the specified class.
    return $method_path
}

proc ruff::private::locate_ooclass_method {class_name method_name} {
    # Locates the classe that implement the specified method of a class
    # class_name - name of the class to which the method belongs
    # method_name - method name being searched for
    #
    # The matching class may implement the method itself or through
    # one of its own mix-ins or superclasses.
    #
    # Returns the name of the implementing class or an empty string
    # if the method is not implemented.

    # Note: we CANNOT just calculate a canonical search path for a
    # given class and then search along that for a class that
    # implements a method. The search path itself will depend on the
    # specific method being searched for due to the fact that a
    # superclass may not appear in a particular search path if a
    # derived class hides a method (this is just one case, there may
    # be others). Luckily, get_ooclass_method_path does exactly this.


    set class_path [get_ooclass_method_path $class_name $method_name]

    if {[llength $class_path] == 0} {
        return "";              # Method not found
    }

    # Now we cannot just pick the first element in the path. We have
    # to find the *last* occurence of each class - that will decide
    # the priority order
    set order [dict create]
    set pos 0
    foreach path_elem $class_path {
        dict set order $path_elem $pos
        incr pos
    }

    return [lindex $class_path [lindex [lsort -integer [dict values $order]] 0] 0]
}


proc ruff::private::load_formatters {} {
    # Loads all available formatter implementations
    foreach formatter [formatters] {
        load_formatter $formatter
    }
}

proc ruff::private::load_formatter {formatter} {
    # Loads the specified formatter implementation
    variable ruff_dir
    set class [namespace parent]::formatter::[string totitle $formatter]
    if {![info object isa class $class]} {
        uplevel #0 [list source [file join $ruff_dir formatter_${formatter}.tcl]]
    }
    return $class
}

proc ruff::document {namespaces args} {
    # Generates documentation for commands and classes.
    # namespaces - list of namespaces for which documentation is to be generated.
    # args - Options described below.
    # -autopunctuate BOOLEAN - If `true`, the first letter of definition
    #  descriptions (including parameter descriptions) is capitalized
    #  and a period added at the end if necessary.
    # -compact BOOLEAN - If `true`, documentation is generated in a more
    #  compact form if supported by the formatter. For the built-in HTML formatter
    #  this results in procedure and method details being placed in collapsed
    #  sections that can be expanded on demand.
    # -diagrammer `DIAGRAMARGS` - arguments to pass to `diagram` processor
    #  if none are specified in the diagram block header. Defaults to
    #  `kroci ditaa`
    # -excludeclasses REGEXP - If specified, any classes whose names
    #  match `REGEXPR` will not be included in the documentation.
    # -excludeprocs REGEXP - If specified, any procedures whose names
    #  match `REGEXPR` will not be included in the documentation.
    # -format FORMAT - The output format. `FORMAT` defaults to `html`.
    # -hidenamespace NAMESPACE - By default, documentation generated by Ruff!
    #  includes namespace qualifiers in all class and proc names. It is possible
    #  to have the generated output leave out the namespace qualifers by adding
    #  the `-hidenamespace NAMESPACE` qualifier to the document generation
    #  commands. This will omit `NAMESPACE` in displayed program element names
    #  and provides a more visually pleasing output with less noise. However,
    #  it may result in ambiguities in case of names being present in more than
    #  one namespace. In particular, some formatters may not cross-link correctly
    #  in such cases.
    # -include LIST - Specifies which program elements are to be documented.
    #  `LIST` must be a list from one or both amongst `classes` or `procs`.
    #  Defaults to both.
    # -includeprivate BOOLEAN - if true private methods are also included
    #  in the generated documentation. Default is false.
    # -includesource BOOLEAN - if true, the source code of the
    #  procedure is also included. Default value is false.
    # -linkassets - if true, CSS and Javascript assets are linked. If false,
    #  they are embedded inline. If unspecified, defaults to `false` if the
    #  `-pagesplit` option is `none` and `true` otherwise. Only supported by the
    #  HTML formatter.
    # -locale STRING - sets the locale of the pre-defined texts in the generated
    #  outputs such as **Description** or **Return value** (Default `en`). To add a
    #  locale for a language, create a message catalog file in the `msgs`
    #  directory using the provided `de.msg` as a template. Only supported by the
    #  HTML formatter.
    # -makeindex BOOLEAN - if true, an index page is generated for classes
    #  and methods. Default value is true. Not supported by all formatters.
    # -navigation OPT - Controls navigation box behaviour when
    #  scrolling. If `scrolled`, the navigation box will scroll vertically
    #  along with the page. Thus it may not visible at all times. If
    #  `sticky`, the navigation box remains visible at all times.
    #  However, this requires the number of links in the box to fit on
    #  the page as they are never scrolled. Note that older browsers
    #  do not support stickiness and will resort to scrolling behaviour.
    #  box (see below). Only supported by the `html` formatter.
    #  (Default `scrolled`)
    # -outdir DIRPATH - Specifies the output directory path. Defaults to the
    #  current directory.
    # -outfile FILENAME - Specifies the name of the output file.
    #  If the output is to multiple files, this is the name of the
    #  documentation main page. Other files will named accordingly by
    #  appending the namespace. Defaults to a name constructed from the first
    #  namespace specified.
    # -pagesplit SPLIT - if `none`, a single documentation file is produced.
    #  If `namespace`, a separate file is output for every namespace.
    # -preamble TEXT - Any text that should be appear at the beginning
    #  outside of any namespace documentation, for example an introduction
    #  or overview of a package. `TEXT` is assumed to be in Ruff! syntax.
    # -preeval SCRIPT - a script to run before generating documentation. This
    #  is generally used from the command line to load the packages being
    #  documented.
    # -product PRODUCTNAME - the short name of the product. If unspecified, this
    #  defaults to the first element in $namespaces. This should be a short name
    #  and is used by formatters to identify the documentation set as a whole
    #  when documenting multiple namespaces.
    # -recurse BOOLEAN - if true, child namespaces are recursively
    #  documented.
    # -section SECTION - the section of the documentation where the pages should
    #  be located. Currently only used by the `nroff` formatter and defaults to
    #  `3tcl`.
    # -sortnamespaces BOOLEAN - If `true` (default) the namespaces are
    #  sorted in the navigation otherwise they are in the order passed in.
    # -title TITLE - This text is shown in a formatter-specific area on every
    #  generated page. The `nroff` formatter for manpages has only a limited
    #  space to display this so `TITLE` should be limited to roughly 50 characters
    #  if that formatter is to be used. If unspecified, it is constructed from
    #  the `-product`.
    # -version VERSION - The version of product being documented.
    #
    # The command generates documentation for one or more namespaces
    # and writes it out to file(s) as per the options shown above.
    # See [Documenting procedures], [Documenting classes] and
    # [Documenting namespaces] for details of the expected source
    # formats and the generation process.
    #

    variable gFormatter

    array set opts {
        -compact 0
        -excludeprocs {}
        -excludeclasses {}
        -format html
        -hidesourcecomments false
        -include {procs classes}
        -includeprivate false
        -includesource false
        -preamble ""
        -recurse false
        -pagesplit none
        -sortnamespaces true
        -locale en
        -section 3tcl
        -preeval ""
        -diagrammer "kroki ditaa"
    }

    array set opts $args

    if {[info exists opts(-output)]} {
        error "Option -output is obsolete. Use -outdir and/or -outfile instead."
    }

    # Load any dependencies
    uplevel #0 $opts(-preeval)

    if {![info exists opts(-makeindex)]} {
        set opts(-makeindex) [expr {$opts(-pagesplit) ne "none"}]
    }
    if {$opts(-pagesplit) eq "none" && $opts(-makeindex)} {
        app::log_error "Option -makeindex ignored when -pagesplit is specified as none."
        set opts(-makeindex) false
    }
    if {![info exists opts(-linkassets)]} {
        set opts(-linkassets) [expr {$opts(-pagesplit) ne "none"}]
    }
    lappend args -linkassets $opts(-linkassets)

    if {![info exists opts(-product)]} {
        set opts(-product) [string trim [lindex $namespaces 0] :]
        lappend args -product $opts(-product)
    }
    if {![info exists opts(-title)]} {
        set opts(-title) [string totitle $opts(-product)]
        lappend args -title $opts(-title)
    }

    ::msgcat::mclocale $opts(-locale)

    namespace upvar private ProgramOptions ProgramOptions
    set ProgramOptions(-hidesourcecomments) $opts(-hidesourcecomments)
    if {$opts(-pagesplit) ni {none namespace}} {
        error "Option -pagesplit must be \"none\" or \"namespace\" "
    }
    set ProgramOptions(-pagesplit) $opts(-pagesplit)
    set ProgramOptions(-makeindex) $opts(-makeindex)
    set ProgramOptions(-diagrammer) $opts(-diagrammer)

    # Fully qualify namespaces
    set namespaces [lmap ns $namespaces {
        if {![string match ::* $ns]} {
            set ns "[string trimright [uplevel 1 {namespace current}] ::]::$ns"
        }
        if {![namespace exists $ns]} {
            error "Namespace $ns does not exist."
        }
        set ns
    }]
    if {[llength $namespaces] == 0} {
        error "At least one namespace needs to be specified."
    }

    set formatter [[load_formatter $opts(-format)] new]
    set gFormatter $formatter

    # Determine output file paths
    array unset private::ns_file_base_cache
    if {![info exists opts(-outdir)]} {
        set opts(-outdir) [pwd]
    } else {
        set opts(-outdir) [file normalize $opts(-outdir)]
    }
    set ProgramOptions(-outdir) $opts(-outdir)

    if {![info exists opts(-outfile)]} {
        # Special cases  - :: -> "", ::foo::bar:: -> ::foo::bar
        set ns [string trimright [lindex $namespaces 0] :]
        if {$ns eq ""} {
            error "Option -outfile must be specified for namespace ::."
        }
        set opts(-outfile) [namespace tail $ns]
    }
    if {[file tail $opts(-outfile)] ne $opts(-outfile)} {
        error "Option -outfile must not include a path."
    }
    set private::output_file_base [file root $opts(-outfile)]
    set private::output_file_ext [file extension $opts(-outfile)]
    if {$private::output_file_ext in {{} .}} {
        set private::output_file_ext .[$formatter extension]
    }

    if {$opts(-recurse)} {
        set namespaces [namespace_tree $namespaces]
    }

    if {$opts(-preamble) ne ""} {
        # TBD - format of -preamble argument passed to formatters
        # is different so override what was passed in.
        lappend args -preamble [extract_docstring $opts(-preamble) ::]
    }
    set classprocinfodict [extract_namespaces $namespaces \
                               -excludeprocs $opts(-excludeprocs) \
                               -excludeclasses $opts(-excludeclasses) \
                               -include $opts(-include) \
                               -includeprivate $opts(-includeprivate)]

    set docs [$formatter generate_document $classprocinfodict {*}$args]
    if {$opts(-makeindex)} {
        set docindex [$formatter generate_document_index]
        if {$docindex ne ""} {
            lappend docs -docindex $docindex
        }
    }

    $formatter copy_assets $ProgramOptions(-outdir)

    $formatter destroy

    file mkdir $opts(-outdir)
    foreach {ns doc} $docs {
        set fn [private::ns_file_base $ns]
        set fd [open [file join $opts(-outdir) $fn] w]
        fconfigure $fd -encoding utf-8
        if {[catch {
            puts $fd $doc
        } msg]} {
            close $fd
            error $msg
        }
        close $fd
    }
    return
}

proc ruff::formatters {} {
    # Gets the available output formatters.
    #
    # The returned values can be passed to [document] to generate
    # documentation in that format.
    #
    # Returns a list of available formatters.
    return {html markdown nroff}
}

# TBD - where is this used
proc ruff::private::wrap_text {text args} {
    # Wraps a string such that each line is less than a given width
    # and begins with the specified prefix.
    # text - the string to be reformatted
    # The following options may be specified:
    # -width INTEGER - the maximum width of each line including the prefix 
    #  (defaults to 60)
    # -prefix STRING - a string that every line must begin with. Defaults
    #  to an empty string.
    # -prefix1 STRING - prefix to be used for the first line. If unspecified
    #  defaults to the value for the -prefix option if specified
    #  and an empty string otherwise.
    #
    # The given text is transformed such that it consists of
    # a series of lines separated by a newline character
    # where each line begins with the specified prefix and
    # is no longer than the specified width.
    # Further each line is filled with as many characters
    # as possible without breaking a word across lines.
    # Blank lines and leading and trailing spaces are removed.
    #
    # Returns the wrapped and indented text

    set opts [dict merge [dict create -width 60 -prefix ""] $args]

    if {![dict exists $opts -prefix1]} {
        dict set opts -prefix1 [dict get $opts -prefix]
    }

    set prefix [dict get $opts -prefix]
    set prefix1 [dict get $opts -prefix1]

    set width [dict get $opts -width]
    # Reduce the width by the longer prefix length
    if {[string length $prefix] > [string length $prefix1]} {
        incr width  -[string length $prefix]
    } else {
        incr width  -[string length $prefix1]
    }

    # Note the following is not optimal in the sense that
    # it is possible some lines could fit more words but it's
    # simple and quick.

    # First reformat
    set text [textutil::adjust::indent \
                  [::textutil::adjust::adjust $text -length $width] \
                  $prefix]

    # Replace the prefix for the first line. Note that because of
    # the reduction in width based on the longer prefix above,
    # the max specified width will not be exceeded.
    return [string replace $text 0 [expr {[string length $prefix]-1}] $prefix1]
}


uplevel #0 {
}
uplevel #0 {
# Plug-ins for processing diagrams

namespace eval ruff::diagram {
    namespace path [list [namespace parent] [namespace parent]::private]

    namespace eval generators {
        namespace path [namespace eval [namespace parent] {namespace path}]
    }

}

proc ruff::diagram::OBSOLETEparse_command {command} {
    # Parses a diagram command
    #  command - the diagram command line
    #
    # The first word of the command line is expected to be
    # the word "diagram". Following is a list of option value pairs that begin
    # character `-` followed by the diagrammer command.
    #
    # If the diagrammer command is not present, a default is supplied.
    #
    # Returns a pair consisting of a (possibly empty) option dictionary and the
    # diagrammer command.

    set command [lassign $command first]
    if {$first ne "diagram"} {
        error "Internal error: command is not a diagram."
    }
    if {[llength $command] == 0} {
        return [list [dict create] [program_option -diagrammer]]
    }

    set n [llength $command]
    set options [dict create]
    for {set i 0} {$i < $n} {incr i} {
        set option [lindex $command $i]
        if {[string index $option 0] ne "-"} {
            # End of options
            break
        }
        if {[incr i] == $n} {
            error "Missing value to go with option \"[lindex $command $i]\" in diagram."
        }
        dict set options $option [lindex $command $i]
    }
    if {$i == $n} {
        set diagrammer [program_option -diagrammer]
    } else {
        set diagrammer [lrange $command $i end]
    }
    return [list $options $diagrammer]
}

proc ruff::diagram::generate {text filename generator args} {
    variable diagram_counter

    if {$filename eq ""} {
        set filename diagram[incr diagram_counter]
    }
    set url "assets/$filename.svg"
    set fd [open [file join [program_option -outdir] $url] wb]
    try {
        set commands [info commands generators::$generator]
        if {[llength $commands] == 1} {
            [lindex $commands 0] $fd $text {*}$args
            return $url
        }
    } finally {
        close $fd
    }
    error "Unknown diagram generator \"$generator\"."
}

###
# kroki diagrammer
proc ruff::diagram::generators::kroki_init {} {
    # If a command line kroki exists, we will use it
    if {[llength [auto_execok kroki]]} {
        interp alias {} [namespace current]::kroki_generate {} [namespace current]::kroki_generate_cli
        proc kroki_init {} {}
        return
    }

    # If no command line kroki, need to use HTTP over TLS to the online server
    uplevel #0 package require http

    # For Windows try twapi first

    if {$::tcl_platform(platform) eq "windows" &&
        ![catch { uplevel #0 package require twapi_crypto }]} {
        http::register https 443 twapi::tls_socket
    } else {
        uplevel #0 package require tls
        tls::init -autoservername true
        http::register https 443 tls::socket
    }

    # Not windows or no twapi
    interp alias {} [namespace current]::kroki_generate {} [namespace current]::kroki_generate_http
    proc kroki_init {} {}
    return
}

proc ruff::diagram::generators::kroki_generate_cli {text input_format fd} {
    set kroki_fd [open |[list {*}[auto_execok kroki] convert - -f svg -t $input_format -o -] r+]
    puts $kroki_fd $text
    close $kroki_fd w
    puts $fd [read $kroki_fd]
    close $kroki_fd
}

proc ruff::diagram::generators::kroki_generate_http {text input_format fd} {
    # See https://wiki.tcl-lang.org/page/dia2kroki
    set b64 [string map {+ - / _ = ""}  [binary encode base64 [zlib compress $text]]]
    set uri https://kroki.io/$input_format/svg/$b64
    set tok [http::geturl $uri]
    if {[http::status $tok] ne "ok"} {
        error "Failed to get image from $uri"
    }
    puts $fd [http::data $tok]
    return
}

proc ruff::diagram::generators::kroki {fd text {input_format ditaa} args} {
    variable kroki_image_counter
    kroki_init
    kroki_generate $text $input_format $fd
}

###
# ditaa diagrammer

proc ruff::diagram::generators::ditaa {fd text args} {
    variable ditaa_image_counter

    set image_fd [open |[list {*}[auto_execok ditaa] - - --svg {*}$args] r+]
    puts $image_fd $text
    close $image_fd w
    puts $fd [read $image_fd]
    close $image_fd
}
}

################################################################
#### Application overrides

# The app namespace is for commands the application might want to
# override
namespace eval ruff::app {
}


proc ruff::app::log_error {msg} {
    # Stub function to log Ruff! errors.
    # msg - the message to be logged
    #
    # When Ruff! encounters errors, it calls this command to
    # notify the user. By default, the command writes $msg
    # to stderr output. An application using the ruff package
    # can redefine this command after loading ruff.
    puts stderr "$msg"
}

package provide ruff $::ruff::version

# If we are the main script, accept commands.
if {[info exists argv0] &&
    [file dirname [file normalize [info script]/...]] eq [file dirname [file normalize $argv0/...]]} {
    if {[catch {
        ruff::document {*}$::argv
    } result]} {
        puts stderr $result
        puts stderr $::errorInfo
    } else {
        if {$result ne ""} {
            puts stdout $result
        }
    }
}

