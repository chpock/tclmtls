# gen.tcl --
#
# This file generates standalone ruff.tcl tool from the sources
# at https://github.com/apnadkarni/ruff
#
# Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

set sources [dict create {*}{
    textutil::tabify {
        url "https://raw.githubusercontent.com/tcltk/tcllib/tcllib-1-21/modules/textutil/tabify.tcl"
        requires {textutil::repeat}
    }
    textutil::adjust {
        url "https://raw.githubusercontent.com/tcltk/tcllib/tcllib-1-21/modules/textutil/adjust.tcl"
        requires {textutil::repeat textutil::string}
    }
    textutil::repeat {
        url "https://raw.githubusercontent.com/tcltk/tcllib/tcllib-1-21/modules/textutil/repeat.tcl"
    }
    textutil::string {
        url "https://raw.githubusercontent.com/tcltk/tcllib/tcllib-1-21/modules/textutil/string.tcl"
    }
    ruff {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/ruff.tcl"
        requires {textutil::adjust textutil::tabify formatter_html.tcl formatter_markdown.tcl formatter_nroff.tcl}
        sources {formatter.tcl diagram.tcl}
        binaries {ruff-min.css ruff-min.js ruff-index-min.js}
        replacements {
            ruff::private::read_asset_file {
                if { [file tail $fn] eq "ruff-min.css" } {
                    return [binary decode base64 "@@@@ruff-min.css@@@@"]
                }
                if { [file tail $fn] eq "ruff-min.js" } {
                    return [binary decode base64 "@@@@ruff-min.js@@@@"]
                }
                if { [file tail $fn] eq "ruff-index-min.js" } {
                    return [binary decode base64 "@@@@ruff-index-min.js@@@@"]
                }
                error "Unknown asset: $fn"
            }
        }
    }
    formatter.tcl {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/formatter.tcl"
    }
    diagram.tcl {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/diagram.tcl"
    }
    formatter_html.tcl {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/formatter_html.tcl"
        requires {formatter.tcl}
        binaries {ruff-min.css ruff-min.js ruff-index-min.js}
        replacements {
            copy_assets {
                #file mkdir [file join $outdir assets]
                #foreach { fn data } {
                #    ruff-min.css "@@@@ruff-min.css@@@@"
                #    ruff-min.js "@@@@ruff-min.js@@@@"
                #    ruff-index-min.js "@@@@ruff-index-min.js@@@@"
                #} {
                #    set fp [open [file join $outdir assets $fn] w]
                #    fconfigure $fp -encoding binary -translation binary
                #    puts -nonewline $fp [binary decode base64 $data]
                #    close $fp
                #}
            }
        }

    }
    formatter_markdown.tcl {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/formatter_markdown.tcl"
        requires {formatter.tcl}
    }
    formatter_nroff.tcl {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/formatter_nroff.tcl"
        requires {formatter.tcl}
    }
    ruff-min.css {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/assets/ruff-min.css"
    }
    ruff-min.js {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/assets/ruff-min.js"
    }
    ruff-index-min.js {
        url "https://raw.githubusercontent.com/apnadkarni/ruff/v2.3.0/src/assets/ruff-index-min.js"
    }
}]

proc get_url { id } {
    return [exec curl -sL [dict get $::sources $id url]]
#    if { [file exists $id] } {
#        set f [open $id r]
#        fconfigure $f -encoding binary -translation binary
#        set data [read $f]
#        close $f
#    } {
#        set data [exec curl -sL [dict get $::sources $id url]]
#        set f [open $id w]
#        fconfigure $f -encoding binary -translation binary
#        puts -nonewline $f $data
#        close $f
#    }
#    return $data
}

proc gen { id {loaded -} } {
    if { $loaded eq "-" } {
        set provided [list]
    } {
        set provided $loaded
    }
    if { [dict exists $::sources $id requires] } {
        foreach pkg [dict get $::sources $id requires] {
            if { [lsearch -exact $provided $pkg] == -1 } {
                lassign [gen $pkg $provided] _src provided
                lappend provided $pkg
                append src $_src \n
            }
        }
    }
    if { [dict exists $::sources $id sources] } {
        foreach file [dict get $::sources $id sources] {
            if { [lsearch -exact $provided $file] == -1 } {
                lassign [gen $file $provided] _src provided
                lappend provided $file
                dict set ::sources $file source $_src
            } {
                # already sources by other script, cleanup source code
                dict set ::sources $file source ""
            }
        }
    }
    set mysrc [get_url $id]
    if { [dict exists $::sources $id replacements] } {
        dict for { proc body } [dict get $::sources $id replacements] {
            regsub "(\\n\[ \\t\]*?)(proc|method)(\\s+$proc\\s+\\{\[^\}\]+\\}\\s+\\{\\n).+?(\\1\\})" \
                $mysrc "\\1\\2\\3\000\\4" mysrc
            set mysrc [string map [list "\000" $body] $mysrc]
        }
    }
    if { [dict exists $::sources $id binaries] } {
        foreach bin [dict get $::sources $id binaries] {
            if { [lsearch -exact $provided $bin] == -1 } {
                lappend provided $bin
                dict set ::sources $bin binary \
                    [binary encode base64 [get_url $bin]]
            }
            set mysrc [string map [list "@@@@${bin}@@@@" [dict get $::sources \
                $bin binary]] $mysrc]
        }
    }
    if { [dict exists $::sources $id sources] } {
        foreach file [dict get $::sources $id sources] {
            regsub "source \[^\n\]*$file\]*" $mysrc "\000" mysrc
            set mysrc [string map [list "\000" \
                "uplevel #0 {\n[dict get $::sources $file source]}"] $mysrc]
        }
    }
    append src $mysrc \n
    return [expr { $loaded eq "-" ? $src : [list $src $provided] }]
}


puts [gen "ruff"]

