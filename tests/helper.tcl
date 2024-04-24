# Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

testConstraint server [::mtls::pkgconfig get server]
testConstraint client [::mtls::pkgconfig get client]
testConstraint notDarwin [expr { $::tcl_platform(os) ne "Darwin" }]

proc result { id } {
    set type [::mtls::pkgconfig get type,backend]
    set ver  [::mtls::pkgconfig get version,backend]
    set os [expr { $::tcl_platform(platform) eq "windows" ? "windows" :
        [expr { $::tcl_platform(os) eq "Darwin" ? "mac" : "unix" }] }]
    if { [dict exists $::results "$type-$os-$ver" $id] } {
        set res [dict get $::results "$type-$os-$ver" $id]
    } elseif { [dict exists $::results "$type-$os" $id] } {
        set res [dict get $::results "$type-$os" $id]
    } elseif { [dict exists $::results "$type-$ver" $id] } {
        set res [dict get $::results "$type-$ver" $id]
    } elseif { [dict exists $::results $type $id] } {
        set res [dict get $::results $type $id]
    } elseif { [dict exists $::results "*" $id] } {
        set res [dict get $::results "*" $id]
    } else {
        set res [list ok unknown]
    }
    return [list -returnCodes [lindex $res 0] -result [lindex $res 1] -match glob]
}

proc request { url } {
    set script [list]
    lappend script [list set url $url]
    lappend script {
        set token [http::geturl "https://$url"]
        if { ![regexp {<title>([^>]+)</title>} [http::data $token] -> res] } {
            set res ""
        }
        set code [http::code $token]
        http::cleanup $token
        set res "$code:$res"
    }
    return [join $script "\n"]
}

proc test_request { id url } {
    return [list test $id $url -body [request $url] -constraints client {*}[result $id]]
}

proc localIP { ip } {
    if { $ip eq "::1" } {
        return "127.0.0.1"
    }
    return $ip
}