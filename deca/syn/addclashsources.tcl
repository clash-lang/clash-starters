package require json

namespace eval clash {
    variable options
    variable db

    namespace export options buildDB addClashFiles

    proc buildDB {} {
        variable options
        variable db

        proc parseManifest {manifestF} {
            variable db

            set manC [open $manifestF r]
            set manifest [json::json2dict [read $manC]]
            close $manC
            set top [dict get $manifest top_component name]
            if [dict exists $db $top] {
                return
            }
            dict set db $top hdlFiles {}
            dict set db $top qsysFiles {}
            dict set db $top sdcFiles {}
            foreach fileEntry [dict get $manifest files] {
                set name [dict get $fileEntry name]
                if [string match {*.vhdl} $name] {
                    dict with db $top {
                        lappend hdlFiles "[file dirname $manifestF]/$name"
                    }
                }
                if [string match {*.qsys} $name] {
                    dict with db $top {
                        lappend qsysFiles "[file dirname $manifestF]/$name"
                    }
                }

                if [string match {*.sdc} $name] {
                    dict with db $top {
                        lappend sdcFiles "[file dirname $manifestF]/$name"
                    }
                }
            }

            foreach dependency [dict get $manifest dependencies transitive] {
                parseManifest "[file dirname \
                        $manifestF]/../$dependency/clash-manifest.json" false
            }
        }

        set db [dict create]
        foreach manifestF \
                [glob -type f -directory $options(clash-hdldir) \
                    $options(top-qual-name)/clash-manifest.json] {
            parseManifest $manifestF
        }
    }

    proc addClashFiles {} {
        variable db

        dict for {top topDict} $db {
            foreach hdlFile [dict get $topDict hdlFiles] {
              set_global_assignment -name VHDL_FILE $hdlFile
              post_message "Adding VHDL file: $hdlFile"
            }

            foreach qsysFile [dict get $topDict qsysFiles] {
              set_global_assignment -name QSYS_FILE $qsysFile
              post_message "Adding QSYS file: $qsysFile"
            }

            foreach sdcFile [dict get $topDict sdcFiles] {
              set_global_assignment -name SDC_FILE $sdcFile
              post_message "Adding QSYS file: $sdcFile"
            }
        }
    }
}

set project  [lindex $quartus(args) 1]
set revision [lindex $quartus(args) 2]
project_open $project -revision $revision

set clash::options(clash-hdldir) {../vhdl}
set clash::options(top-qual-name) {DECA.deca}
clash::buildDB
clash::addClashFiles
