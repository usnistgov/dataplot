#  send command to Dataplot and expect certain strings
#
#  2019/04: Small edit in spawnDataplot to reflect name change
#           of xdp to xdataplot.wish

proc sendDP {cmd} {
   global spawnDP nsavCmd savCmd xdp timeout pix
   global iRcl nRcl rclCmd resetTimeout echoInTextout DS GS canVas dsExp

   if {$xdp(DEBUG)} {puts "\nENTER sendDP"}
   if {![info exists timeout]} {set timeout -1}
   if {![info exists nRcl]} {set nRcl 0}
   if {![info exists rclCmd(0)]} {set rclCmd(0) ""}
   if {![info exists canVas(multi)]} {set canVas(multi) 0}

   if {$timeout != 0} {cursorConfig watch}

#  if dataplot is already spawned
   if {($xdp(COMM) == "expect" && [info exists spawnDP]) || $xdp(COMM) != "expect"} {
   
#  check for certain commands
      set slow 0
      set sendVar 0
      set sendGrf NULL
      set cmdstr [split $cmd " "]
      set cmdlen [llength $cmdstr]
      set cmd1 [string tolower [lindex $cmdstr 0]]
      set cmd2 [string tolower [lindex $cmdstr 1]]
      set cmd3 [string tolower [lindex $cmdstr 2]]
      set cmd4 [string tolower [lindex $cmdstr 3]]
      set cmd5 [string tolower [lindex $cmdstr 4]]
      set xdp(cmd1) $cmd1

#  check for multiple commands on one line
      set cmd12 ""
      set semi [string first ";" $cmd]
      if {$semi != -1} {
         set cmd12 [string tolower [string range $cmd [expr {$semi+1}] end]]
         set cmd12 [lindex $cmd12 0]
      }
	 
#  check for REPEAT GRAPH command and handle outside of Dataplot
      if {$cmd1 == "rg" || ($cmd1 == "repeat" && $cmd2 == "graph")} {
	 if {$cmd1 == "rg"} {set pix(file) [string range $cmd 3 end]}
	 if {$cmd1 == "repeat"} {set pix(file) [string range $cmd 13 end]}
	 if {[file exists $pix(file)]} {
	    wm deiconify .pixmap
	    focus .pixmap.f1.kill
	    .pixmap.f2.canvas create bitmap 0 0 -bitmap @$pix(file) -anchor nw \
	       -background white -foreground black
	    set pix(label) $pix(file)
	    set pix(file) [string trim [file extension $pix(file)] "."]
	 } else {
            errorMsg "ERROR: File not found $pix(file)"
	 }
	 set cmd ""
      }
      
#  check for SAVE command
      if {$cmd1 == "save"} {
         if {[string first "." $cmd2] == -1 || \
            [regexp -nocase {[a-z]} [string range $cmd 4 end]] == 0 || \
            $cmd3 == "to"} {

#  create list of command indices
            set clist ""
            if {$cmdlen == 1} {
               set clist 1
            } elseif {$cmdlen == 4 && $cmd3 == "to"} {
               set inc 1
               if {$cmd2 > $cmd4} {set inc -1}
               set nc [expr {$cmd2-$cmd4}]
               if {$nc < 0} {set nc [expr {-$nc}]}
               set nc [expr {$nc+1}]
               set j $cmd2
               for {set i 1} {$i <= $nc} {incr i} {
                  lappend clist $j
                  incr j $inc
               } 
            } else {
               set clist [string range $cmd 4 end]
            } 

#  loop through last 20 commands and store matches in savCmd
            set cmds [.cmdhist.f2.list get 0 end]
            set ncmds [llength $cmds]
            set nsavCmd 0
            set min [expr {$ncmds-20}]
            if {$min < 0} {set min 0}
            for {set j 0} {$j < [llength $clist]} {incr j} {
               set k [expr {$ncmds-$min}]
               for {set i $min} {$i < $ncmds} {incr i} {
                  if {$k == [lindex $clist $j]} {
                     incr nsavCmd
                     set savCmd($nsavCmd) [lindex $cmds $i]
                  }
        	  incr k -1
               }
            }
         }
      }
      
#  check for a SERIAL READ command before READ
      if {$cmd1 == "serial" && $cmd2 == "read"} {
         set cmd1 $cmd2; set cmd2 $cmd3; set cmd3 $cmd4
      }
      
#  check for a READ command
      if {$cmd1 == "read" && $cmdlen > 2} {
         if {[string first "." $cmd2] != -1} {set sendVar 1}
         if {$cmd2 == "matrix" && [string first "." $cmd3] != -1} {set sendVar 1}
      }
      if {$cmd12 == "read"} {set sendVar 1}
         
#  check for READ that reads data from the command line
      if {[info exists resetTimeout]} {
         unset resetTimeout
         set timeout -1
      }
      if {$cmd1 == "read" && [string first "." $cmd] == -1} {
         set timeout 0
         cursorConfig arrow
         foreach c {function parameter string} {
            if {$cmd2 == $c} {set resetTimeout 1}
         }
      } 
      
#  check for a END DATA command
      if {$cmd1 == "end" && ($cmd2 == "data" || $cmd2 == "of")} {
         set timeout -1
         set sendVar 1
      }
      
#  check for a LET command, store expressions in dsExp for use in the spreadsheet
      if {$cmd1 == "let"} {
         set sendVar 1
         foreach c {function parameter string} {if {$cmd2 == $c} {set sendVar 0}}
	 if {$sendVar && $cmd4 != "data"} {
            set pe [string first "=" $cmd]
            set ind [string toupper [string trim [string range $cmd 3 [expr {$pe-1}]]]]
            set dsExp($ind) [string trim [string range $cmd [expr {$pe+1}] end]]
	 }
      }
      if {$cmd12 == "let"} {set sendVar 1}
      
#  check for a CALL, DELETE command
      foreach c {call delete} {
	 if {$cmd1 == $c  || $cmd12 == $c} {
            set sendVar 1
            dataSheetClear
	 }
      }
     
#  check for a RESET command
     if {$cmd1 == "reset"} {
        if {$cmd2 == "data" || $cmd2 == "variables"} {set sendVar 1}
        if {$cmd2 == "graphics" || $cmd2 == "control" || $cmd2 == "limits"} {set sendGrf ALL}
     }     
      
#  check for a HELP command
      set helpCmd 0
      if {$cmd1 == "help" || $cmd12 == "help"} {
         set helpCmd 1
         if {!$xdp(DEBUG)} {set echoInTextout 0}
      }
      
#  check for a LIST or NLIST command
      set listCmd 0
      foreach c {l list nlist} {
	 if {$cmd1 == $c || $cmd12 == $c} {
            set listCmd 1
            if {!$xdp(DEBUG)} {set echoInTextout 0}
	 }
      }
      
#  check for graph settings commands
      if {$cmd1 == "title"} {set sendGrf TITLE}
      if {[string range $cmd1 2 6] == "label"} {set sendGrf LABELS}
      if {$cmd1 == "legend"} {set sendGrf LEGENDS}
      set c4 [string range $cmd1 0 3]
      foreach c {line char spik bar} {if {$c4 == $c} {set sendGrf $c}}
      foreach c {tic fram plot} {if {$c4 == $c} {set sendGrf LIMITS}}
      if {[string range $cmd1 2 4] == "tic"} {set sendGrf LIMITS}
      if {[string range $cmd1 2 7] == "limits"} {set sendGrf LIMITS}
      if {$cmd1 == "call"} {set sendGrf ALL}
     
#  check for commands where the text output comes back 'slowly'
      if {$cmd1 == "fit" || $cmd1 =="prefit"} {set slow 1}
     
#  check for a MULTIPLOT command
      if {$cmd1 == "multiplot" && $cmd2 != "scale"} {
         if {$cmd2 == "off"} {set canVas(multi) 0} else {set canVas(multi) 1}
      }
     
#  check for diagrammatic graphics commands
      foreach c $canVas(gcmds) {if {$cmd1 == $c} {set canVas(multi) 2}}
     
#  check for a DEVICE command
      if {$cmd1 == "device" && $cmd2 == "1" && $cmd3 != "font" } {
         if {$xdp(GRAPHICS) == "soft" && $cmd3 == "x11"} {
            set cmd ""
         } else {
            set xdp(DEVICE) [string range $cmd3 0 3]
            if {$xdp(DEVICE) == "gene" && $cmd4 != ""} {set xdp(DEVICE) [string range $cmd4 0 3]}
            if {$xdp(DEVICE) == "code" && $cmd5 != ""} {set xdp(DEVICE) [string range $cmd5 0 3]}
         }
      }
      
#  check for LATTICE CELLS command
      if {$cmd1 == "lattice" && $cmd2 == "cells"} {
         set xdp(LATTY) $cmd3
         set xdp(LATTX) $cmd4
         set cmd ""
      }      
      
#  check for REPLOT or . command
      if {$cmd1 == "replot" || ($cmd1 == "." && $cmd2 == "")} {raise .graph}      

#  ----------------------------------------------------------------------------
      
#  echoInTextout=1 is the default
      if {![info exists echoInTextout]} {set echoInTextout 1}

#  command and spawn id not null
      if {$cmd != "" && (($xdp(COMM) == "expect" && $spawnDP != "") || \
         $xdp(COMM) != "expect")} {

#  save for recall
	 if {$cmd != $rclCmd($nRcl) && $cmd1 != "gui" || \
	    ($cmd1 == "gui" && $xdp(DEBUG))} {
	    incr nRcl; set rclCmd($nRcl) $cmd; set iRcl [expr {$nRcl+1}]
	 }

#  filter out commands that should not be echoed in the command history
         set echocmd 1
         foreach i {l list s save r repeat gui} {if {$cmd1 == $i} {set echocmd 0}}

#  echo command
         if {$echocmd} {
            if {$cmd != "/"} {
	       pageWrite .cmdhist $cmd

#  echo saved commands for a /
	    } else {
	       if {[info exists nsavCmd]} {
		  for {set i 1} {$i <= $nsavCmd} {incr i} {
		     pageWrite .cmdhist $savCmd($i)
		  }
	       }
	    }
	 }

#  ----------------------------------------------------------------------------

#  turn off sending output to shell window
         if {$xdp(COMM) == "expect"} {
	    exp_log_user 0
	    if {$cmd1 == "exit"} {exp_log_user 1}
	 }

#  send command to dataplot and wait for possibly a lot of output
         set buffer ""
	 set spawn_id $spawnDP
         if {$xdp(DEBUG)} {puts "  Sending: $cmd"}

#  send the command for expect, set match_max (buffer size) depending on 'slow'
         if {$xdp(COMM) == "expect"} {
	    set notdone 1
            set nfull 0
            if {$slow == 0} {exp_match_max 2000} else {exp_match_max 80}
            if {$cmd1 == "list" || $cmd1 == "help"} {exp_match_max 80000}
	    exp_send "$cmd\r"

#  puts the command for a pipe
	 } elseif {$xdp(COMM) == "pipe"} {
	    puts $xdp(pipe) $cmd

#  puts the command for a file
	 } elseif {$xdp(COMM) == "file"} {
	    set fname fort.10
	    catch {file delete $fname}
	    set wid [open $fname w]
	    puts $wid $cmd
	    flush $wid
	    close $wid
	 }
         if {$xdp(DEBUG)} {puts "  Sent"}

#  set a few variables
	 set dsUpdate 0
	 set gsUpdate 0
	 set dsStatus 0
	 set dpError 0
	 set grPlot 0
         set varStr "VARIABLES--"
         set stvStr "STATUS VARIABLES"
         if {$listCmd} {set stvStr NULL}

#  ----------------------------------------------------------------------------

#  expect patterns (order is important, put > prompt last)
#  go through while loop until prompt is found
         if {$xdp(COMM) == "expect"} {
	    while {$notdone} {
	       expect {
		  "GUI WRITE ALL VARIABLES" {set dsUpdate 1; set varStr NULL} 
		  $varStr {set dsUpdate 1; set varStr NULL} 
		  $stvStr {set dsStatus 1} 
		  "GUI SAVE PLOT CONTROL" {set gsUpdate 1} 
		  "OPDE" {set grPlot 1} 
		  "MORE...?" {exp_send "\n"}
		  -exact "***** NOTE" {set dpError 0}
		  -exact "***** CURRENT PIXMAP" {set dpError 0}
		  -exact "***** ERROR" {set dpError 1; set echoInTextout 1}
		  -exact "***** WARNING" {set dpError 1; set echoInTextout 1}
		  -exact "THIS IS AN EXIT" {set notdone 0}
		  "\r\n>" {set notdone 0}
		  "\r>" {set notdone 0}
		  "\n>" {set notdone 0}

		  exp_full_buffer {    ;#  full buffer depends on size of exp_match_max
		     incr nfull
		     if {$slow == 0} {
			append buffer $expect_out(buffer)

		     } else {      ;#  handle output from 'slow' commands
		        set buffer $expect_out(buffer)
	                regsub -all "\r" $buffer "" text
	                if {[info exists rem]} {append rem $text; set text $rem}
	                set p [string last \n $text]
                        set out [split [string range $text 0 [expr {$p-1}]] \n]
                        set rem [string range $text [expr {$p+1}] end]
		        if {$nfull == 1} {
                           pageWin .textout $out
		        } else {
		           foreach line $out {pageWrite .textout $line}
                        }
		     }
	             set expect_out(buffer) ""
		  }
	       }

#  accumulate text output in buffer
               if {[info exists expect_out(buffer)]} {append buffer $expect_out(buffer)}
	    }
	    if {$xdp(DEBUG)} {puts "  Expect done: $nfull full buffer(s)"}

#  ----------------------------------------------------------------------------

#  read command pipeline channel if not using expect
	 } elseif {$xdp(COMM) != "expect"} {
	    if {$cmd1 == "exit"} {exit}
	    if {$xdp(PLATFORM) == "unix" || [info tclversion] >= 8.1} {
	       fileevent $xdp(pipe) readable [set buffer [readPipe]]
	       fileevent $xdp(pipe) readable
	    } else {
	       set buffer [readPipe]
	    }
	    set buffer [linsert $buffer 0 { } $cmd]

#  look for strings if not using expect
	    if {[regexp {GUI WRITE ALL VARIABLES} $buffer]} {
	       set dsUpdate 1
	    } elseif {[regexp {VARIABLES--} $buffer]} {
	       set dsUpdate 1
	    } elseif {[regexp {STATUS VARIABLES} $buffer]} {
	       set dsStatus 1
	    } elseif {[regexp {GUI SAVE PLOT CONTROL} $buffer]} {
	       set gsUpdate 1
	    } elseif {[regexp {OPDE} $buffer]} {
	       set grPlot 1
	    } elseif {[regexp {\*\*\*\*\* NOTE} $buffer]} {
	       set dpError 0
	    } elseif {[regexp {\*\*\*\*\* CURRENT PIXMAP} $buffer]} {
	       set dpError 0
	    } elseif {[regexp {\*\*\*\*\* ERROR} $buffer]} {
	       set dpError 1
	       set echoInTextout 1
	    } elseif {[regexp {\*\*\*\*\* WARNING} $buffer]} {
	       set dpError 1
	       set echoInTextout 1
	    }
	    if {$xdp(DEBUG)} {puts "  Pipe done"}
	 }

#  ----------------------------------------------------------------------------

#  echo what was accumulated text output window
         set xdp(REGSUB) 1
         if {$dsUpdate || $dsStatus || $gsUpdate || \
             $grPlot || $helpCmd || $listCmd} {set xdp(REGSUB) 0}
         set text [buffer2Text $buffer]

#  output in normal way
         if {$slow == 0 || $xdp(COMM) != "expect"} {
            if {$echoInTextout && $text != ""} {
               if {$grPlot == 0 || $xdp(DEBUG)} {
                   pageWin .textout $text
               } else {
                   set end [lsearch $text *OPDE]
                   set tmp [lrange $text 0 [expr {$end - 1}]]
                   lappend tmp {>}
                   pageWin .textout $tmp
               }
            }

#  output remainder of output from a 'slow' command
         } else {
            set pw 0
            foreach line $text {
               if {$pw == 0 && [string first $rem $line] != -1} {set pw 1}
               if {$pw} {pageWrite .textout $line}
            }
         }
	 set echoInTextout 1
         
#  plot a graph in the canvas
         if {$grPlot} {graphPlot .graph $text}

#  send values to datasheet window
	 if {$dsUpdate} {dataSheetUpdate $text}

#  check variable status
	 if {$dsStatus} {dataSheetStatus $text}

#  send values to graph settings window
	 if {$gsUpdate} {graphSetUpdate $text}

#  error dialog
	 if {$dpError} {
            set errout ""
            set errMsg ""
            if {$buffer != ""} {
               if {$xdp(COMM) == "expect"} {
                  set errout [buffer2Text $expect_out(buffer)]
               } else {
                  set errout $buffer
               }
            }
	    for {set i 0} {$i <= [llength $errout]} {incr i} {
	       set errlin($i) [string trim [lindex $errout $i]]
	    }
	    set errMsg $errlin(0)
	    foreach n {1 2 3} {
	       if {[info exists errlin($n)]} {append errMsg "\n$errlin($n)"}
	    }
	    set errMsg "Command: $cmd\n\n$errMsg ...\n\nSee Text Output window for more details."
	    tk_messageBox -type ok -message $errMsg -icon error -title Error
	    set listCmd 0
	    set helpCmd 0
	 }
	 
#  send gui commands
         if {![info exists DS(noecho)]} {set DS(noecho) 0}
         if {$sendVar && $DS(noecho) == 0} {sendVariables}

         if {![info exists GS(noecho)]} {set GS(noecho) 0}
         if {$sendGrf != "NULL" && $GS(noecho) == 0} {sendGraphics $sendGrf}
	 
#  help window
         if {$helpCmd} {
            if {[llength $text] > 3} {
               pageWin .help $text
            } else {
               set errMsg "No help text found for: $cmd"
	       tk_messageBox -type ok -message $errMsg -icon error -title Warning
            }
         }
 	 
#  list window
         if {$listCmd && [llength $text] > 3} {pageWin .list $text}
      }
   }

   cursorConfig arrow
   if {$xdp(DEBUG)} {puts " LEAVE sendDP"}
}   

################################################################################

proc sendVariables {} {
   global xdp echoInTextout
   
   if {$xdp(DSNOUPDATE) == 0} {
      if {!$xdp(DEBUG)} {set echoInTextout 0}
      sendDP "GUI WRITE ALL VARIABLES"
      if {!$xdp(DEBUG)} {set echoInTextout 0}
      sendDP "GUI STATUS VARIABLES"
   }
}

################################################################################

proc sendGraphics {type} {
   global xdp echoInTextout GS

   if {!$xdp(DEBUG)} {set echoInTextout 0}
   sendDP "GUI SAVE PLOT CONTROL $type 1 $GS(lim4)"
}

################################################################################

proc spawnDataplot {} {
   global spawnDP xdp env tcl_platform
   
   update idletasks

#  start dataplot
   if {[findCmd "$xdp(DP_EXEC)"]} {
      cursorConfig watch
      set spawnDP ""
      set dpok 1
      set str(pipe) "pipe"
      set str(file) "pipe/file"
      set str(expect) spawning
      catch {unset env(DATAPLOT_GUI_IO)}

#  first try to spawn dataplot with expect
      if {$xdp(COMM) == "expect"} {
         set tempjunk1 $str($xdp(COMM))
         set tempjunk2 "[file native $xdp(DP_EXEC)] ... "
         pageWin .textout [list " " "$tempjunk1 $tempjunk2"]
	 if {[catch {exp_spawn "$xdp(DP_EXEC)"} err]} {
            pageWrite .textout "Error starting Dataplot"
            if {[info exists env(DATAPLOT_GUI_IO)]} {
               set xdp(COMM) [string tolower $env(DATAPLOT_GUI_IO)]
            } else {
	       if {$xdp(PLATFORM) == "unix" || $xdp(OS) == "Windows NT"} {
		  set xdp(COMM) pipe
	       } else {
		  set xdp(COMM) file
	       }
	    }
	    if {$err != "invalid command name \"exp_spawn\""} {
               set msg "$err\nDataplot could not be spawned\nSwitching to a $str($xdp(COMM))"
               puts $msg
               tk_messageBox -type ok -message $msg -icon error -title Error
            }
	 } else {
	    set spawnDP $spawn_id
	    expect -re ">"
	 }
      }

#  if dataplot cannot be spawned with expect then,
#  open dataplot with command pipeline
      if {$xdp(COMM) == "pipe" || $xdp(COMM) == "file"} {
         set tempjunk1 $str($xdp(COMM))
         set tempjunk2 "[file native $xdp(DP_EXEC)]..."
         pageWin .textout [list " " "$tempjunk1 $tempjunk2"]
         if {$xdp(COMM) == "pipe"} {
            set opt r+
            set env(DATAPLOT_GUI_IO) PIPE
         } elseif {$xdp(COMM) == "file"} {
            set opt r
            set env(DATAPLOT_GUI_IO) FILE
         }
	 if {[catch {open |"$xdp(DP_EXEC)" $opt} err]} {
            pageWrite .textout "Error starting Dataplot"
            set msg "Dataplot not opened with a $str($xdp(COMM))."
            if {[info exists err]} {set msg "$err\n$msg"}
            puts $msg
            tk_messageBox -type ok -message $msg -icon error -title Error
            set dpok 0
         } else {
            set xdp(pipe) $err
            fconfigure $xdp(pipe) -buffering line
         }
      }

#  dataplot started ok
      if {$dpok} {
	 set text ""
         if {$xdp(COMM) == "expect"} {
	    if {[info exists expect_out(buffer)]} {set text [buffer2Text $expect_out(buffer)]}
	 } else {
	 
#  fileevent works OK under unix and for windows with Tcl 8.1 or greater	 
	    if {$xdp(PLATFORM) == "unix" || [info tclversion] >= 8.1} {
	       fileevent $xdp(pipe) readable [set text [readPipe]]
	       fileevent $xdp(pipe) readable
	    } else {
	       set text [readPipe]
	    }
	 }   
	 pageWin .textout $text

#  make dataplot draw to graphics output or pixmap canvas
	 foreach w {.graph .pixmap} {
	    set wid [winfo id $w.f2.canvas]
	    if {$xdp(DEBUG)} {puts " $w $wid"}
	    set wid [string range $wid 2 end]
	    if {$w == ".graph"} {
	       set lid [string length $wid]
	       set part1 [string range $wid 0 [expr {$lid-5}]]
	       set part2 [string range $wid [expr {$lid-4}] end]
	    }
	 }

#  print information about computer, monitor, and variables
#  Windows default path now is "C:\Program Files\ ...", logic
#  needs to be modified
#
#  2019/04: Rename xdp to xdataplot.wish.
#
         set mtime 0
         set tmpj1 [file join "$xdp(DP_TCL)" xdataplot.wish]
         set tmpj2 [file join "$xdp(DP_TCL)" xdpConfig]
         set tmpj3 [file join "$xdp(DP_TCL)" xdpData.tcl]
         set tmpj4 [file join "$xdp(DP_TCL)" xdpDP.tcl]
         set tmpj5 [file join "$xdp(DP_TCL)" xdpForm.tcl]
         set tmpj6 [file join "$xdp(DP_TCL)" xdpGraph.tcl]
         set tmpj7 [file join "$xdp(DP_TCL)" xdpGrset.tcl]
         set tmpj8 [file join "$xdp(DP_TCL)" xdpPage.tcl]
         set tmpj9 [file join "$xdp(DP_TCL)" xdpPull.tcl]
         set tmpj10 [file join "$xdp(DP_TCL)" xdpTop.tcl]
         set flist [list "$tmpj1" "$tmpj2" "$tmpj3" "$tmpj4" "$tmpj5" "$tmpj6" "$tmpj7" "$tmpj8" "$tmpj9" "$tmpj10"]

#        set tempjunk4 " "
#        append flist [split "[glob $tempjunk3 ]"  $tempjunk4 ]

         foreach file $flist {
            set fmtime [file mtime "$file"]
            if {$fmtime > $mtime} {set mtime $fmtime; set lastUpdate [clock format $fmtime]} 
         }
	 set text {}
	 lappend text " "; lappend text "Welcome to XDP";
	 lappend text " "; lappend text "Last Update: $lastUpdate";
	 lappend text " "; lappend text "System Parameters"
	 foreach i [lsort [array names xdp]] {
	    lappend text [format "%-13s %s" $i $xdp($i)]
	 }
	 pageWin .textout $text

#  send device and set commands
	 if {$xdp(GRAPHICS) == "x11"} {
            set devicecmd "DEVICE 1 X11 $part1 $part2"
	 } elseif {$xdp(GRAPHICS) == "soft"} {
            set devicecmd "DEVICE 1 GENERAL CODED PACKED"
	 }
	 sendDP $devicecmd
	 sendDP "SET HELP LINES 100000"
	 sendDP "SET GUI ON"
	 sendVariables
	 sendGraphics ALL
      }
      cursorConfig arrow

   } else {
      set tempjunk "ERROR: Cannot find  [file native $xdp(DP_EXEC)]"
      pageWin .textout [list " " "$tempjunk"]
   }
}

################################################################################

proc readPipe {} {
   global xdp
   
   if {$xdp(DEBUG)} {puts "ENTER readPipe"}
   set text ""
   while {1} {
      if {$xdp(DEBUG)} {puts "before gets: $xdp(pipe)"}
      gets $xdp(pipe) line
      if {$xdp(DEBUG)} {puts "line: $line"}
      lappend text $line
      if {[string first ">" [string range $line 0 2]] != -1} {return $text}
   }
}

################################################################################

proc buffer2Text {buffer} {
   global xdp

   if {$xdp(DEBUG)} {puts "ENTER buffer2Text"}
   if {![info exists xdp(REGSUB)]} {set xdp(REGSUB) 1}

#  convert expect_out(buffer) to a list (text)
   set text ""
   if {$buffer != ""} {

#  fix some output text
      if {$xdp(DEBUG) == 0 && $xdp(REGSUB) == 1} {
	 regsub -all {THE FORTRAN COMMON PARAMETER} $buffer \
	    "Parameter" buffer
	 regsub -all {THE FORTRAN COMMON CHARACTER VARIABLE} $buffer \
	    "Character variable" buffer
	 regsub -all {THE COMPUTED VALUE OF THE CONSTANT} $buffer \
	    "Constant" buffer
	 regsub -all {HA(S|VE) (JUST BEEN|BEEN) (SET TO|TURNED|EQUIVALENCED|SET)} $buffer \
	    "=" buffer

	 regsub -all {THE NUMBER OF VALUES GENERATED FOR THE VARIABLE} $buffer \
	    "Values generated for" buffer
	 regsub -all {THE CURRENT COLUMN FOR THE VARIABLE} $buffer \
	    "Column for" buffer
	 regsub -all {THE CURRENT LENGTH OF  THE VARIABLE} $buffer \
	    "Length of " buffer

	 regsub -all "CURRENT PIXMAP SUCCESSFULLY SAVED TO FILE" $buffer \
	    "Pixmap saved\n\n Pixmaps can be displayed by clicking on\nFile -> Pixmaps in the Graph window" buffer
	 regsub -all "ERROR IN DPSAPL--NO CURRENT PIXMAP TO SAVE." $buffer \
	    "No Graph to save as a Pixmap" buffer

	 regsub -all "THE FIRST 8 CHARACTERS OF THE FIRST WORD" $buffer \
	    "Error in command" buffer
	 regsub -all "OF THE COMMAND ARE" $buffer "" buffer
	 regsub -all "PLEASE REENTER COMMAND LINE" $buffer \
	    "Please try again" buffer
	 regsub -all "NO MATCH FOUND FOR COMMAND" $buffer \
	    "Command does not exist" buffer
      }
      
      if {$xdp(COMM) == "expect"} {
	 regsub -all "\r" $buffer "" text
	 set text [split $text \n]
      } else {
         set text $buffer
      }

   }
   return $text
}
