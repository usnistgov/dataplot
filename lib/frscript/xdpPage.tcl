proc pageWin {w {text ""}} {

#  displays a text in a window to list data from HELP or LIST commands

   global numPage curPage nwinPage page pageOf cmdHist cmdLin xdp

#  initialize
   if {$xdp(DEBUG)} {puts "ENTER pageWin $w"}
   if {![info exists numPage($w)]} {set numPage($w) 0}
   if {![info exists curPage($w)]} {set curPage($w) 0}
   if {![info exists nwinPage]} {set nwinPage 0}
   if {![info exists cmdHist]} {set cmdHist ""}
   incr nwinPage

#  create the window if it doesn't already exist
   if {![winfo exists $w]} {
      catch {destroy $w}
      toplevel $w
      if {$w == ".cmdhist"} {set title "Command History"}
      if {$w == ".textout"} {set title "Text Output"}
      if {$w == ".help"} {set title "Help"}
      if {$w == ".list"} {set title "List"}
      wm title $w $title
      wm iconname $w $title
      wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]

#  position top window (textout) next to graphics window
      if {$w == $xdp(WINTOP)} {
	 set xx [expr {[winfo x .] + $xdp(OFFSET,x)}]
	 set yy [expr {[winfo y .] + [winfo height .] + $xdp(OFFSET,y)}]
	 set xdp(WID,$xdp(WINTOP)) [expr {[winfo width .] - [winfo width .graph] + \
	    2*$xdp(OFFSET,x) - $xdp(OFFSET,x1)}]
	 set xdp(HGT,$xdp(WINTOP)) $xdp(HGT,.graph)
	 wm geometry $w $xdp(WID,$xdp(WINTOP))\x$xdp(HGT,$xdp(WINTOP))+$xx+$yy

#  position bottom window (cmdhist) below top window and above command line window
      } elseif {$w == $xdp(WINBOT)} {
	 set xx [expr {[winfo x .] + $xdp(OFFSET,x)}]
	 set yy [expr {[winfo y .graph] + [winfo height .graph] + $xdp(OFFSET,y)}]
	 set xdp(WID,$xdp(WINBOT)) $xdp(WID,$xdp(WINTOP))
	 set yoff [expr {2*$xdp(OFFSET,y)}]
	 if {$xdp(OS) != "IRIX" && $xdp(PLATFORM) != "windows"} {
	    set yoff [expr {2*$xdp(DHGT,.graph)+6}]
	 }
	 set xdp(HGT,$xdp(WINBOT)) [expr {[winfo y $cmdLin(win)] \
	    - [winfo y $xdp(WINTOP)] - [winfo height $xdp(WINTOP)] - $yoff}]
	 if {$xdp(HGT,$xdp(WINBOT)) < 0} {set xdp(HGT,$xdp(WINBOT)) 300}
	 wm geometry $w $xdp(WID,$xdp(WINBOT))\x$xdp(HGT,$xdp(WINBOT))+$xx+$yy

#  position other windows (help, list)
      } else {
	 #set xx [expr {[winfo x .] + ($nwinPage-1)*50}]
	 #set yy [expr {[winfo y .] + [winfo height .] + ($nwinPage-1)*35}]
	 set xx [expr {[winfo x .] + 50}]
	 set yy [expr {[winfo y .] + [winfo height .] + 35}]
	 wm geometry $w +$xx+$yy
	 wm withdraw $w
      }

      set f1 [frame $w.f1 -bd 1 -relief raised]
      set f2 [frame $w.f2 -bd 1 -relief raised]

#  back and forward buttons
      set blist ""
      if {$w != ".cmdhist"} {
	 button $f1.back -text "<<" -state disabled -command "pageBack $w"
	 button $f1.forward -text ">>" -state disabled -command "pageForward $w"
	 pack $f1.back $f1.forward -side left
	 lappend blist $f1.back $f1.forward
      }

#  file menu
      menubutton $f1.file -text "File" -menu $f1.file.menu
      pack $f1.file -side left
      set m [menu $f1.file.menu -tearoff 0]

      $m add command -label "Save As ..." -acc "Ctrl-S" -command "pageSave $w"
      $m add command -label "Append To ..." -command "pageAppend $w"
      $m add command -label "Edit ..." -command "pageEdit"
      if {![findCmd $xdp(DP_EDIT)]} {$m entryconfigure 2 -state disabled}
      $m add command -label "Edit Command ..." -command {
	 set xdp(DP_EDIT) [cmdSetup EDIT]
      }
      
      $m add command -label "Print" -acc "Ctrl-P" -command "pagePrint $w"
      if {![findCmd $xdp(DP_PRINT)]} {$m entryconfigure 4 -state disabled}
      $m add command -label "Print Command ..." -command {
	 set xdp(DP_PRINT) [cmdSetup PRINT]
      }

      $m add command -label "Browser Command ..." -command {
	 set xdp(DP_BROWSER) [cmdSetup BROWSER]
      }
      $m add separator

      $m add command -label "Remenu" -acc "Button-3" -command "pullDownShow"
      $m add checkbutton -label "Debug" -variable xdp(DEBUG) -command {
	 if {$xdp(PLATFORM) == "windows" && $xdp(DEBUG)} {console show}
      }

#  commands menu
      menubutton $f1.cmd -text "Commands" -menu $f1.cmd.menu
      pack $f1.cmd -side left
      set m [menu $f1.cmd.menu]
      if {$w == ".cmdhist"} {
	 $m add command -label "Exec selected" -acc "Button-2" -command {pageCmdSelect}
	 $m add command -label "SAVE selected" -command {pageCmdSave}
	 $m add separator
      }
      pageMenuCmd $m CALL
      pageMenuCmd $m LIST
      $m add separator
      set mi 0
      set mi [pageMenuCmd1 $m $mi {FEEDBACK} {ON OFF}]
      set mi [pageMenuCmd1 $m $mi {RESET} {GRAPHICS DATA LIMITS VARIABLES PARAMETERS FUNCTIONS MATRICES}]
      set mi [pageMenuCmd1 $m $mi {STATUS "WRITE ALL"} {VARIABLES PARAMETERS}]
      if {$w != ".cmdhist"} {
	 menubutton $f1.help -text "Help" -menu $f1.help.menu
	 pack $f1.help -side left
	 pageMenuHelp $f1.help.menu
      }
      
#  kill button
      if {$w == ".list" || $w == ".help"} {
	 button $f1.kill -text "X" -command "wm withdraw $w"
	 lappend blist $f1.kill
	 pack $f1.kill -side right
	 focus $f1.kill      
      }

#  of label
      if {$w != ".cmdhist"} {
	 set pageOf($w) "$curPage($w) of $numPage($w)"
	 label $f1.label -textvariable pageOf($w) -relief groove -bg $xdp(COLOR,2) \
            -padx 4 -pady 2  
	 pack $f1.label -side right
      }

#  configure buttons      
      foreach b $blist {$b configure -padx $xdp(PADX) -pady $xdp(PADY)}

#  listbox with scroll bars
      if {$w == ".help" || $w == ".list"} {set h 30} else {set h 100}
      listbox $f2.list -xscroll [list $f2.xscroll set] -selectmode multiple \
	 -yscroll [list $f2.yscroll set] -width 80 -height $h
      scrollbar $f2.yscroll -command [list $f2.list yview]
      scrollbar $f2.xscroll -command [list $f2.list xview] -orient horiz
      bind $f2.list <Button-3> "pullDownShow"

      grid $f2.list $f2.yscroll -sticky news
      grid $f2.xscroll -row 1 -column 0 -sticky ew
      grid rowconfigure $f2 0 -weight 1
      grid columnconfigure $f2 0 -weight 1

      pack $f1 $f2 -side top -fill x
	
#  bindings
      bind $w <Control-s> "pageSave $w"
      bind $w <Control-p> "pagePrint $w"
   
#  bind double click on a line in the command history window
#  to executing that command
      if {$w == ".cmdhist"} {
	 bind .cmdhist.f2.list <Double-Button-1> {
	    set index [.cmdhist.f2.list curselection]
	    if {$index != ""} {foreach i $index {sendDP [lindex $cmdHist $i]}}
	    .cmdhist.f2.list selection clear 0 end
	 }
	 bind .cmdhist.f2.list <Button-2> {pageCmdSelect}
      
#  disable bindings for all but .cmdhist
      } else {
	 bind $f2.list <Any-ButtonPress> {break}
	 bind $f2.list <Any-ButtonRelease> {break}
	 bind $f2.list <B1-Motion> {break}
	 bind $f2.list <B2-Motion> {break}
      }

      update idletasks
      winOnScreen $w $xx $yy 0
   }

#  window already exists, make visible and put text in window
   if {$text != ""} {
      wm deiconify $w
      if {$w == ".help" || $w == ".list"} {focus $w.f1.kill}
      incr numPage($w)
      set page($numPage($w),$w) $text  
      $w.f2.list delete 0 end
      foreach line $page($numPage($w),$w) {$w.f2.list insert end $line}
      $w.f1.forward configure -state disabled
      if {$numPage($w) > 1} {
	 $w.f1.back configure -state normal
	 if {$w == ".textout"} {focus $w.f1.back}
      }
      set curPage($w) $numPage($w)
      set title [lindex $page($numPage($w),$w) 1]
      wm title $w $title
      set pageOf($w) "$curPage($w) of $numPage($w)"
      if {$w == ".help" || $w == ".list"} {raise $w}
   }
}

###############################################################################

proc pageWrite {w text} {
   global cmdHist page curPage
   
   if {[winfo exists $w]} {
      wm deiconify $w
      $w.f2.list insert end $text
      $w.f2.list yview end
      if {$w == ".cmdhist"} {lappend cmdHist $text}
      lappend page($curPage($w),$w) $text
   }
}

###############################################################################

proc pageBack {w} {
   global curPage page numPage pageOf

   focus $w.f1.back
   incr curPage($w) -1
   $w.f2.list delete 0 end
   foreach line $page($curPage($w),$w) {$w.f2.list insert end $line}
   if {$curPage($w) == 1} {
      $w.f1.back configure -state disabled
      focus $w.f1.forward
   }
   if {$curPage($w) < $numPage($w)} {
      $w.f1.forward configure -state normal
   }
   set title [lindex $page($curPage($w),$w) 1]
   wm title $w $title
   set pageOf($w) "$curPage($w) of $numPage($w)"      
}

proc pageForward {w} {
   global curPage page numPage pageOf

   focus $w.f1.forward
   incr curPage($w) 1
   $w.f2.list delete 0 end
   foreach line $page($curPage($w),$w) {$w.f2.list insert end $line}
   if {$curPage($w) == $numPage($w)} {
      $w.f1.forward configure -state disabled
      focus $w.f1.back
   }
   if {$curPage($w) > 1} {
      $w.f1.back configure -state normal
   }
   set title [lindex $page($curPage($w),$w) 1]
   wm title $w $title
   set pageOf($w) "$curPage($w) of $numPage($w)"      
}

###############################################################################

proc pageSave {w} {
   global curPage page
   
   set fsave [tk_getSaveFile -title "Save File"]
   if {$fsave != ""} {
      set fid [open $fsave w]
      foreach line $page($curPage($w),$w) {puts $fid $line}
      close $fid
   }
}

proc pageAppend {w} {
   global curPage page
   
   set fappend [tk_getOpenFile -title "Append File"]
   if {$fappend != ""} {
      set fid [open $fappend a]
      foreach line $page($curPage($w),$w) {puts $fid $line}
      close $fid
   }
}

proc pagePrint {w} {
   global curPage page nPrint xdp

   if {![info exists nPrint]} {set nPrint 0}
   incr nPrint
   set fname dp$nPrint[pid]
   if {$xdp(PLATFORM) == "windows"} {set fname [string range $fname 0 7]}
   file mkdir /tmp
   set fprint [file join /tmp $fname.dat]
   set fid [open $fprint w]
   foreach line $page($curPage($w),$w) {puts $fid $line}
   close $fid
   set opt ""
   if {$xdp(PLATFORM) == "windows"} {set opt /p}
   if {[catch {exec $xdp(DP_PRINT) $opt $fprint} err]} {
      tk_messageBox -type ok -message $err -icon error -title Error
   }
}

proc pageEdit {} {
   global xdp
   
   set fedit [tk_getOpenFile -title "Edit File"]
   if {$fedit != ""} {
      if {[catch {exec [file native $xdp(DP_EDIT)] $fedit &} err]} {
         tk_messageBox -type ok -message $err -icon error -title Error
      }
   }
}

################################################################################

proc pageMenuCmd {m cmd} {
   if {![info exists dirs($cmd)] && $cmd == "LIST"} {
      set dirs($cmd) {. data dex help macros programs text}
   }
   if {![info exists dirs($cmd)] && $cmd == "CALL"} {
      set dirs($cmd) {. macros programs}
   }

   set men $m.[string tolower $cmd]
   $m add cascade -label "$cmd ..." -menu $men
   set m2 [menu $men -tearoff 0]
   if {$cmd == "LIST"} {
     foreach l {CONCLUSIONS DEFINITIONS SAVE DIRECTORY DICTIONARY} {
	$m2 add command -label $l -command [list sendDP "$cmd $l"]
     }
     $m2 add separator
   }
   foreach d $dirs($cmd) {
      $m2 add command -label $d -command "pageMenuFileSelect $d $cmd $m"
   }
}

proc pageMenuCmd1 {m mi list1 list2} {
   foreach c0 $list1 {
      incr mi
      $m add cascade -label $c0 -menu $m.$mi
      set mc($mi) [menu $m.$mi -tearoff 0]
      foreach c1 [lsort $list2] {
	 $mc($mi) add command -label $c1 -command [list sendDP "$c0 $c1"]
      }
   }
   return $mi
}

proc pageMenuFileSelect {dir cmd m} {
   global xdp

   if {$dir != "."} {
      set file [tk_getOpenFile -title "Open File" \
	 -initialdir [file join "$xdp(DP_LIB)" $dir]]
   } else {
      set file [tk_getOpenFile -title "Open File"]
   }
   if {$file != ""} {
      if {$cmd == "CALL"} {wm withdraw .help; wm withdraw .list}
      sendDP "$cmd $file"
   }
}

proc pageCmdSelect {} {
   set index [.cmdhist.f2.list curselection]
   if {$index != ""} {
      foreach i $index {
	 set cmd [join [.cmdhist.f2.list get $i $i] ""]
	 sendDP $cmd
      }
   }
   .cmdhist.f2.list selection clear 0 end
}

proc pageCmdSave {} {
   set index [.cmdhist.f2.list curselection]
   if {$index != ""} {
      set ncmds [llength [.cmdhist.f2.list get 0 end]]
      set n ""
      foreach i $index {
	 append n "[expr {$ncmds-$i}] "
	 if {$n != ""} {sendDP "SAVE $n"}
      }
   }
   .cmdhist.f2.list selection clear 0 end
}

################################################################################

proc pageMenuHelp {m {filter ""}} {
   global topics xdp
   
#  list of main help topics
   set main "1.00 Main"
   if {![info exists topics($main)]} {
      set topics($main) {"OVERVIEW" "GRAPHICS" "DIAGRAMMATIC GRAPHICS" "ANALYSIS" \
	 "PLOT CONTROL" "SUPPORT" "OUTPUT DEVICE" "KEYWORDS" "FUNCTIONS" \
	 "MATH FUNCTIONS" "TRIG FUNCTIONS" "PROB FUNCTIONS" "LET SUBCOMMANDS" \
	 "STATISTICS" "MATH OPERATIONS" "MATRIX OPERATIONS" "RANDOM NUMBERS" \
	 "TEXT SUBCOMMANDS" "CAPITALIZATION" "SUBSCRIPTS" "GREEK SYMBOLS" \
	 "MATH SYMBOLS" "MISC SYMBOLS" "CHARACTER TYPES" "LINE TYPES" \
	 "COLOR TYPES"}

#  read refman.tex to get other help topics
      set fhelp [file join "$xdp(DP_LIB)" help refman.tex]
      if {[file exists $fhelp]} {
      set fid [open $fhelp r]
      while {[gets $fid line] >= 0} {
	 set p [string first "refman" $line]

#  filter out some of the entries
	 if {$p != -1 && [string first "VOLUME" $line] == -1 && \
	    [string first "INTRODUCTION" $line] == -1 && \
	    [string first "htm" $line] == -1} {

#  group topics by chapter
	    set topic [string trim [string range $line 0 [expr {$p-1}]]]
	    set chapter [string trim [string range $line $p end]]
	    set refman  [lindex [split $chapter /] 0]
	    set chapter [lindex [split $chapter /] 1]
	    if {$refman == "refman1"} {
	       if {$chapter == "ch1"} {set chapter "1.01 Introduction"}
	       if {$chapter == "ch2"} {set chapter "1.02 Graphics Commands"}
	       if {$chapter == "ch3"} {set chapter "1.03 Analysis Commands"}
	       if {$chapter == "ch4"} {set chapter "1.04 Plot Control Commands"}
	       if {$chapter == "ch5"} {set chapter "1.05 Support Commands"}
	       if {$chapter == "ch6"} {set chapter "1.06 Diagrammatic Graphics Commands"}
	       if {$chapter == "ch7"} {set chapter "1.07 Output Devices"}
	       if {$chapter == "ch8"} {set chapter "1.08 Keywords"}
	       if {$chapter == "ch9"} {set chapter "1.09 Input/Output Commands"}
	       if {$chapter == "ch10"} {set chapter "1.10 SET Subcommands"}
	       if {$chapter == "ch11"} {set chapter "1.11 Color"}
	       if {$chapter == "ch12"} {set chapter "1.12 Graphics Attributes"}
	       if {$chapter == "ch13"} {set chapter "1.13 Text Attributes"}
	       if {$chapter == "ch14"} {set chapter "1.14 Reference Files"}
	    } elseif {$refman == "refman2"} {
	       if {$chapter == "ch1"} {set chapter "2.01 Introduction"}
	       if {$chapter == "ch2"} {set chapter "2.02 Statistics LET Subcommands"}
	       if {$chapter == "ch3"} {set chapter "2.03 Mathematics LET Subcommands"}
	       if {$chapter == "ch4"} {set chapter "2.04 Matrix LET Subcommands"}
	       if {$chapter == "ch5"} {set chapter "2.05 Generating Random Numbers"}
	       if {$chapter == "ch6"} {set chapter "2.06 Mathematical Library Functions"}
	       if {$chapter == "ch7"} {set chapter "2.07 Trigonometric Library Functions"}
	       if {$chapter == "ch8"} {set chapter "2.08 Probability Library Functions"}
	       if {[string first "aux" $chapter] != -1} {set chapter "3.00 Auxillary"}
	    }
	    lappend topics($chapter) $topic
	 }
      }
      }
   }

#  cascade the chapters
   set mh [menu $m]
   set ns 0
   set gmax 30
   foreach c [lsort [array names topics]] {
      set c1 [string range $c 5 end]
      set fmatch 0
      foreach f $filter {if {$f == $c1} {set fmatch 1}}
      if {$filter == "" || $fmatch} {
	 incr ns
	 $mh add cascade -label $c1 -menu $mh.$ns
	 set m2 [menu $mh.$ns]
	 set lc [llength $topics($c)]
	 set nc 0
	 if {$lc <= $gmax} {
	    foreach t $topics($c) {
	       $m2 add command -label $t -command [list sendDP "HELP $t"]
	    }
	 } else {
	    set nt -1
	    foreach t $topics($c) {
	       incr nt
	       if {[expr {$nt%$gmax}] == 0} {
		  incr nc
		  $m2 add cascade -label "Group $nc" -menu $m2.$ns$nc
		  set m3 [menu $m2.$ns$nc]
	       }
	       $m3 add command -label $t -command [list sendDP "HELP $t"]
	    }
	 }
      }
   }
}
