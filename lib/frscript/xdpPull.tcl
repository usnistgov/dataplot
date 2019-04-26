proc pullDownMenu {title lines items actions rmsiz mentyp} {

   global nPrint xdp fileIndex
   global PD pdLbnam pdActions pdLines pdItems pdPush
   global rooment ri
   
   if {$xdp(DEBUG_MENU)} {puts "\nENTER pullDownMenu"}
   if {[winfo exists .form]} {destroy .form}
   
   if {$title == "" && [lindex $lines 0] != "" && [lindex $lines 1] == ""} {
      set title [lindex $lines 0]
      set lines [lrange $lines 1 end]
      set items [lrange $items 1 end]
      set actions [lrange $actions 1 end]
   }
   set pdActions($PD(curr)) $actions
   set pdLines($PD(curr)) $lines
   set pdItems($PD(curr)) $items
   
#  figure out if the window is a menu or just text
   set PD(lb_is_menu) 0
   set nact 0
   for {set i 0} {$i < [llength $actions]} {incr i} {
      if {[lindex $actions $i] != ""} {
	 incr nact
	 set PD(lb_is_menu) 1
      }
   }

#  set the font based on lines of commands vs. text
   set lbfont $xdp(FONT,2)
   set ratio [expr {double($nact)/double([llength $actions])}]
   if {$ratio > 0.26} {set lbfont $xdp(FONT,1)}
   
   set w .pd$PD(curr)
   if {[winfo exists $w]} {destroy $w}
   set pdLbnam($PD(curr)) $w.f1.listbox

   catch {destroy $w}
   toplevel $w

#------------------------------------------------------------------------------
#  pdMenu window title and location
   wm title    $w $title
   wm iconname $w $title
   wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]

   if {$PD(curr) == 1} {
      set xx [expr {[winfo x .] + $xdp(OFFSET,x)}]
      set yy [expr {[winfo y .] + [winfo height .] + $xdp(OFFSET,y)}]
   } else {
      set xywin .pd[expr {$PD(curr) - 1}]
      set xx [expr {[winfo x $xywin] + 50}]
      set yy [expr {[winfo y $xywin] + 35}]
   }
   wm geometry $w +$xx+$yy

   if {!$xdp(DEBUG_MENU) || $mentyp == "RM"} {
      set sublist $items
   } else {
      set sublist $lines
   }

#  figure out size of a room
   if {$mentyp == "RM"} {
      set rmcol [expr {[lindex $rmsiz 2] + [lindex $rmsiz 3]}]
      set rmrow [expr {[lindex $rmsiz 0] + [lindex $rmsiz 1]}]
   }
      
#  create buttons and label at top of window
   frame $w.f0 -bd 1 -relief raised

#------------------------------------------------------------------------------
#  kill button
   button $w.f0.lbutton -text "X" -padx $xdp(PADX) -pady $xdp(PADY) -command {
      set i [string trimleft $pdPush ".pd"]
      set i [string trimright $i ".f0.lbutton"]
      pullDownWithdraw [expr {$i-1}]
   }
   bind $w.f0.lbutton <Button-1> {+ set pdPush %W}
   pack $w.f0.lbutton -side left

#  pulldown menu
   menubutton $w.f0.rbutton -text "File" -menu $w.f0.rbutton.menu
   bind $w.f0.rbutton <Button-1> {+ set pdPush %W}

   set m [menu $w.f0.rbutton.menu -tearoff 0 -postcommand {
      set spick [string trimleft $pdPush ".pd"]
      set spick [string trimright $spick ".f0.rbutton"]
   }]
   $m add command -label "Save As ..." -command {
      set fsave [tk_getSaveFile -title "Save File"]
      if {$fsave != ""} {
	 set fid [open $fsave w]
	 foreach line $pdItems($spick) {puts $fid $line}
	 close $fid
      }
   }
   $m add command -label "Append To ..." -command {
      set fappend [tk_getSaveFile -title "Save File"]
      if {$fappend != ""} {
	 set fid [open $fsave a]
	 foreach line $pdItems($spick) {puts $fid $line}
	 close $fid
      }
   }
   $m add command -label Print -command {
      if {![info exists nPrint]} {set nPrint 0}
      incr nPrint
      set fprint [file join [file native /tmp] dp_$nPrint[pid]]
      set fid [open $fprint w]
      foreach line $pdItems($spick) {puts $fid $line}
      close $fid
      exec [file native $xdp(DP_PRINT)] $fprint
   } -state $xdp(PRINT_STATE)
   $m add command -label "Print Command ..." -command {cmdSetup PRINT}
   $m add separator
   $m add command -label "Remenu" -acc "Button-3" -command {pullDownShow}
   $m add checkbutton -label "Debug menu" -variable xdp(DEBUG_MENU) -command {
      if {$xdp(PLATFORM) == "windows" && $xdp(DEBUG_MENU)} {console show}
   }
   $m add checkbutton -label "Debug form" -variable xdp(DEBUG_FORM) -command {
      if {$xdp(PLATFORM) == "windows" && $xdp(DEBUG_FORM)} {console show}
   }


   pack $w.f0.rbutton -side right

#------------------------------------------------------------------------------
#  CREATE LISTBOX
   if {$mentyp == "LB"} {
      frame $w.f1 -bd 1 -relief raised -bg $xdp(COLOR,1)

#  put text in
      if {$lbfont == $xdp(FONT,1)} {
	 foreach i $sublist {
	    if {[string first ".DAT" $i] != -1} {set lbfont $xdp(FONT,2); break}
	 }
      }
      listbox $pdLbnam($PD(curr)) -width 0 -height 0 -font $lbfont \
	 -selectmode browse -relief flat -highlightthickness 0 -bg $xdp(COLOR,1)
      set refont 0
      foreach i $sublist {
	 $pdLbnam($PD(curr)) insert end $i
	 if {[string first "....." $i] != -1} {set refont 1}
      }
      if {$refont} {
	 $pdLbnam($PD(curr)) configure -font $xdp(FONT,2)
	 set lbfont $xdp(FONT,2)
      }

#  put symbol at end of line
      listbox $pdLbnam($PD(curr))_arrow -width 0 -height 0 -font $lbfont \
	 -selectmode browse -relief flat -highlightthickness 0 -bg $xdp(COLOR,1)
      for {set i 0} {$i < [llength $sublist]} {incr i} {
	 set act [lindex $actions $i]
	 set file ""
	 if {$act == ""} {
	    $pdLbnam($PD(curr))_arrow insert end "   "
	 } elseif {[regexp {xemf} $act]} {
	    regsub {xemf:} $act "" file
	    set file [string trim $file]
	 } elseif {[regexp {xcf} $act]} {
	    regsub {xcf:} $act "" file
	    set file [string trim $file]
	 } elseif {[regexp {xcl} $act] || [regexp {x2cl} $act] || \
	    [regexp {xccl} $act]} {
	    $pdLbnam($PD(curr))_arrow insert end " C>"
	 } else {
	    set file $act
	 }

	 if {$file != ""} {
	    set file [pullDownFileName $file]
	    if {$xdp(PLATFORM) == "unix"} {
	       regsub "$xdp(DP_FEND)" $file {} ifile
	    } else {
	       set ifile [string range $file [string length "$xdp(DP_FEND)"] end]
	    }
	    if {($xdp(INDEX_FILE) && [info exists fileIndex($ifile)]) || [file exists $file]} {
               set ext [file extension $file]
	       if {$ext == ".men"} {
	          $pdLbnam($PD(curr))_arrow insert end " F>"
	       } elseif {$ext == ".DP"} {
	          $pdLbnam($PD(curr))_arrow insert end " M>"
	       } else {
	          $pdLbnam($PD(curr))_arrow insert end "  >"
	       }
	    } else {
	       $pdLbnam($PD(curr))_arrow insert end "  -"
	    }
	 }
      }

      $pdLbnam($PD(curr)) configure -yscroll [list $pdLbnam($PD(curr))_scroll set]
      $pdLbnam($PD(curr))_arrow configure -yscroll [list $pdLbnam($PD(curr))_scroll set]
      scrollbar $pdLbnam($PD(curr))_scroll \
	 -command [list scrollBoth [list $pdLbnam($PD(curr)) $pdLbnam($PD(curr))_arrow]]
      grid $pdLbnam($PD(curr)) -row 0 -column 0 -sticky news
      grid $pdLbnam($PD(curr))_arrow -row 0 -column 1 -sticky news
      grid rowconfigure $w.f1 0 -weight 1
      grid columnconfigure $w.f1 0 -weight 1
      pack $w.f0 $w.f1 -side top -fill x

      set resize [lindex [winCrop $w $xx $yy] 0]
      if {$resize} {
	 grid $pdLbnam($PD(curr))_scroll -row 0 -column 2 -sticky news
	 wm resizable $w 0 1
      }

      bind $pdLbnam($PD(curr))_arrow <Any-ButtonPress> {break}
      bind $pdLbnam($PD(curr))_arrow <Any-ButtonRelease> {break}
      bind $pdLbnam($PD(curr))_arrow <B1-Motion> {break}
      bind $pdLbnam($PD(curr))_arrow <B2-Motion> {break}

#  select the first item in the menu list that has an action or
#  that matches the rmsiz string
      set rmsiz [string trim $rmsiz]
      set notsel 1
      set j 0
      if {$rmsiz != ""} {
	 foreach i $sublist {
	    if {[string first $rmsiz $i] != -1} { 
	       $pdLbnam($PD(curr)) selection set $j $j
	       set notsel 0
	       break
	    }
	    incr j
	 }
      }
      while {$notsel} {
	 foreach i $sublist {
	    set act [lindex $actions $j]
	    if {$i != "" && $act != ""} { 
	       $pdLbnam($PD(curr)) selection set $j $j
	       set notsel 0
	       break
	    }
	    incr j
	 }
	 set notsel 0
      }
      focus $pdLbnam($PD(curr))

#------------------------------------------------------------------------------
#  CREATE ROOM
   } elseif {$mentyp == "RM"} {
      set ni 0
      set ri 0
      set ii 0
      set nr 0
      set nc 0
      frame $w.f1 -bd 0 -relief raised
      foreach li $sublist {
	 if {$li != "" || [lindex $actions $ii] != ""} {
	    incr ni
	    incr ri
	    if {[lindex $actions $ii] != ""} {
	       set rooment($ri) $w.f1.but$ni
	       button $rooment($ri) -text $li \
		  -highlightthickness 0 -bg $xdp(COLOR,1) -command {
		  set pd_picked 1
		  for {set j $PD(total)} {$j >= [expr {$pd_picked+1}]} \
		     {incr j -1} {
		     catch {destroy .pd$j}
		  }
		  set PD(total) $pd_picked
		  set PD(curr) $pd_picked
		  foreach k [array names rooment] {
		     if {$rooment($k) == $pdPush} {set bpick $k}
		  }
		  set line [lindex $pdLines($PD(curr)) $bpick]
		  set item [lindex $pdItems($PD(curr)) $bpick]
		  set action [lindex $pdActions($PD(curr)) $bpick]
		  if {$item != ""} {pullDownAction $line $item $action}
	       }
	       bind $rooment($ri) <Button-1> {+ set pdPush %W}
	    } else {
	       set rooment($ri) $w.f1.tex$ni
	       label $rooment($ri) -text $li \
		  -highlightthickness 0 -relief groove
	    }
	    grid $rooment($ri) -column $nr -row $nc -ipadx 15 -ipady 25 -sticky news
	    incr nr
	    if {$nr == $rmrow} {incr nc; set nr 0}
	 }
	 incr ii
      }
      pack $w.f0 $w.f1 -side top -expand 1 -fill both
   }

#------------------------------------------------------------------------------
   
   bind $w <Button-3> pullDownShow

# Bind single click and return to pdMenu items in listbox

   if {$mentyp == "LB"} {
      if {$PD(lb_is_menu)} {
	 foreach bitem {ButtonRelease-1 Button-2} {
	    bind $pdLbnam($PD(curr)) <$bitem> {

#  Figure out which listbox was clicked in, put PD(curr) in pd_picked
	       for {set i 1} {$i <= $PD(curr)} {incr i} {
		  if {[winfo exists $pdLbnam($i)]} { 
		     if {[$pdLbnam($i) curselection] != ""} {set pd_picked $i}
		  }
	       }
	       set PD(menuindex) [$pdLbnam($pd_picked) curselection]

	       if {$PD(menuindex) >= 0} {
		  set line [lindex $pdLines($pd_picked) $PD(menuindex)]
		  set item [lindex $pdItems($pd_picked) $PD(menuindex)]
		  set action [lindex $pdActions($pd_picked) $PD(menuindex)]
		  if {$item != ""} {
		     for {set j $PD(total)} {$j >= [expr {$pd_picked+1}]} \
			{incr j -1} {
			catch {destroy .pd$j}
		     }
		     set PD(total) $pd_picked
		     set PD(curr) $pd_picked
		     pullDownAction $line $item $action
		  }
	       }
	    }
	    bind $pdLbnam($PD(curr)) <B2-Motion> {break}
	 }

#------------------------------------------------------------------------------
#  turn off all button bindings if the listbox is just text, not a menu
      } elseif {!$PD(lb_is_menu)} {
	 bind $pdLbnam($PD(curr)) <Any-ButtonPress> {break}
	 bind $pdLbnam($PD(curr)) <Any-ButtonRelease> {break}
	 bind $pdLbnam($PD(curr)) <B1-Motion> {break}
	 bind $pdLbnam($PD(curr)) <B2-Motion> {break}
	 $pdLbnam($PD(curr)) configure -bg $xdp(COLOR,2)
	 $pdLbnam($PD(curr))_arrow configure -bg $xdp(COLOR,2)
      }
   }
}

################################################################################

proc pullDownAction {line item action} {

   global PD xdp formLines formFile pdMenu
   
   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownAction\n LINE $line"}
   set word1 [lindex $action 0]
   set char1 [string range $action 0 0]
   if {$char1 == "~"} {set file $action} else {regsub $word1 $action "" file}
   set file [string trim $file]
   set fext [string tolower [file extension $file]]
   if {$fext == ".top" && [string range $word1 0 0] == "x"} {
      if {$xdp(DEBUG_MENU)} {
         errorMsg "WARNING: extra action $word1 for .top on menu file"
      }
      set word1 ""
   }
   if {$xdp(DEBUG_MENU)} {puts " WORD1 $word1\n FILE $file"}

#  read xem file and generate window
   if {$word1 == "xemf:" || $fext == ".men"} {
      if {$word1 != "xemf:" && $xdp(DEBUG_MENU)} {
         errorMsg "WARNING: missing action \"xemf:\" on menu file"
      }
      set formFile [pullDownFileName $file]
      set formLines [pullDownReadfile $formFile]
      if {$formLines != ""} {formDisplay}

#  execute Dataplot command(s)
   } elseif {$word1 == "xcl:" || $word1 == "x2cl:"} {
      pullDownWithdraw
      set c [string first ":" $action]
      set cmd [string trim [string range $action [expr {$c+1}] end]]
      sendDP $cmd

#  execute Dataplot command in a macro file (.DP)
   } elseif {$word1 == "xcf:" || $fext == ".dp"} {
      if {$word1 != "xcf:" && $xdp(DEBUG_MENU)} {
         errorMsg "WARNING: missing action \"xcf:\" on menu file"
      }
      pullDownWithdraw
      set fname [pullDownFileName $file]
      set cmds [pullDownReadfile $fname]
      set i 0
      foreach line $cmds {incr i; if {$i > 3 && $line != ""} {sendDP $line}}

#  commands to be interpreted and executed in Tcl
   } elseif {$word1 == "xccl:"} {
      pullDownWithdraw
      pullDownCmd [lrange $action 1 end]

#  display the next menu
   } elseif {$char1 == "~" || $fext == ".top"} {
      set fname [pullDownFileName $action]
      set lines [pullDownReadfile $fname]
      if {$lines != ""} {
	 cursorConfig watch
	 incr PD(curr)
	 set rmsiz [lindex $lines 1]
	 set title [lindex $lines 2]
	 set lines [lrange $lines 3 end]
	 set pdMenu(lines) $lines
	 pullDownSplitLines $PD(curr)
	 set items $pdMenu(items)
	 set actions $pdMenu(actions)
	 incr PD(total)
	 set PD(curr) $PD(total)
	 if {$fext == ".top"} {
	    pullDownMenu $title $lines $items $actions $rmsiz LB
	 } elseif {$fext == ".roo"} {
	    pullDownMenu $title $lines $items $actions $rmsiz RM
	 }
	 cursorConfig arrow
      }

   } elseif {$word1 == ""} {

#  not recognized
   } else {
      errorMsg "ERROR: Unrecognized directive $line"
   }
}

################################################################################

proc pullDownCmd {cmd} {

   global xdp

   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownCmd"}
   cursorConfig watch
   set cmd0 [lindex $cmd 0]
   set cmd1 [lindex $cmd 1]
      
   if {$cmd0 == "web"} {
      set url $cmd1
      if {"$xdp(DP_BROWSER)" != ""} {exec "$xdp(DP_BROWSER)" $url &}
   
   } elseif {$cmd0 == "listfile"} {
      if {[file exists $cmd1]} {
	  set text ""
	  set fid [open $cmd1 r]
	  while {[gets $fid line] >= 0} {lappend text $line}
	  pageWin .list $text
      } else {   
	 errorMsg "ERROR: File not found $cmd1"
      }
	    
   } elseif {$cmd0 == "printfile"} {
      set fname [string range $cmd1 10 end]
      if {[file exists [file join $xdp(DP_LIB) $fname]]} {
	 eval exec $xdp(DP_PRINT) [file join $xdp(DP_LIB) $fname]
	 errorMsg "Printing file: $fname"
      } else {
	 errorMsg "ERROR: File not found $fname"
      }
      
   } else {
      errorMsg "ERROR: xccl command not found $cmd"
   }
   cursorConfig arrow
}

################################################################################

proc pullDownWithdraw {{max 0}} {
   global PD xdp

   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownWithdraw"}
   for {set i $PD(total)} {$i > $max} {incr i -1} {catch {wm withdraw .pd$i}}   
   catch {wm withdraw .form}
   catch {destroy .varbrowse}
   for {set i 1} {$i <= $max} {incr i} {catch {raise .pd$i}}   
   update idletasks
}

################################################################################

proc pullDownShow { } {
   global PD xdp

   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownShow"}
   for {set i 1} {$i <= $PD(total)} {incr i 1} {catch {wm deiconify .pd$i; raise .pd$i}}
   catch {wm deiconify .form; raise .form}
   catch {wm deiconify .varbrowse; raise .varbrowse}
   update idletasks
}

################################################################################

proc pullDownSplitLines {current} {
   global topMenu pdMenu xdp

   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownSplitLines"}
   if {$current == 0} {
      set lines $topMenu(lines)
   } else {
      set lines $pdMenu(lines)
   }

   foreach item $lines {
      set pos [string first "<" $item]
      if {[regexp {<=} $item]} {set pos -1}
      if {$pos != -1} {
	 set str1 [string range $item 0 [expr {$pos-1}]]
	 set str2 [string range $item $pos end]
      } else {
	 set str1 $item
	 set str2 {}
      }
      set str1 [string trimright $str1]
      set str2 [string trimleft $str2 "<"]
      set str2 [string trimright $str2 ">"]
      lappend out1 $str1
      lappend out2 $str2
   }

   if {$current == 0} {
      set topMenu(items) $out1
      set topMenu(actions) $out2
   } else {
      set pdMenu(items) $out1
      set pdMenu(actions) $out2
   }
}

################################################################################

proc pullDownReadfile {readfile} {
   global xdp fileCache fileIndex
   
   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownReadfile"}
   set rlist {}
   if {$xdp(PLATFORM) == "unix"} {
      regsub "$xdp(DP_FEND)" $readfile "" r
   } else {
      set r [string range $readfile [string length "$xdp(DP_FEND)"] end]
   }

#  read from fileCache if it exists
   if {[info exists fileCache]} {
      foreach index [array names fileCache] {
	 if {$index == [file native $readfile]} {
	    if {$xdp(DEBUG)} {
	       set msg "fileCache: $r"
	       puts $msg
	       pageWrite .textout $msg
	    }
	    set rlist $fileCache($index)
	    return $rlist
	 }
      }
   }
   
#  look at fileIndex to figure out which file to read
   set indexerr 0
   if {$xdp(INDEX_FILE)} {
      if {[info exists fileIndex($r)]} {
	 set ifile [file join "$xdp(DP_FEND)" $fileIndex($r)]
	 if {[file exists $ifile]} {
	    if {$xdp(DEBUG)} {
	       set msg "Master read: [file native $fileIndex($r)]"
	       puts $msg
	       pageWrite .textout $msg
	    }
	    set f [open $ifile r]
	    set delim 0
	    foreach line [split [read $f] \n] {
	       if {[string first "$$$$$$$$$$$$$$$$" $line] != -1} {
		  set delim 1
	       } elseif {$delim == 1} {
		  set delim 0
		  if {$line != ""} {
		     set ind [file native [file join "$xdp(DP_FEND)" $line]]
		     set fileCache($ind) {}
		  }
	       } else {
		  lappend fileCache($ind) $line
	       }
	    }
	    close $f
	    set fn [file native $readfile]
	    if {[info exists fileCache($fn)]} {
	       return $fileCache($fn)
	    } else {
	       set errMsg "ERROR: File missing from master: [file native $r]"
	       errorMsg $errMsg
	       tk_messageBox -type ok -message $errMsg -icon error -title Error
	    }
	 } else {
	    set errMsg "ERROR: Master not found: [file native $fileIndex($r)]"
	    errorMsg $errMsg
	    tk_messageBox -type ok -message $errMsg -icon error -title Error
	 }
      } else {
	 set indexerr 1
      }
   }   

#  open file and read
   if {[file exists $readfile]} {
      if {$xdp(DEBUG)} {
	 set msg "Read: [file native $r]"
	 puts $msg
	 pageWrite .textout $msg
      }
      set f [open "$readfile" r]
      while {[gets $f line] >= 0} {lappend rlist $line}
      close $f

#  cache every file that is read
      set fileCache($readfile) $rlist

#  file not found
   } else {
      if {$indexerr} {
	 set errMsg "ERROR: File not in index.mas: [file native $r]"
	 errorMsg $errMsg
	 tk_messageBox -type ok -message $errMsg -icon error -title Error
      } else {
	 set errMsg "ERROR: File not found: [file native $r]"
	 errorMsg $errMsg
	 tk_messageBox -type ok -message $errMsg -icon error -title Error
      }
   }

   return $rlist
}

################################################################################

proc pullDownFileName {fname} {
   global xdp

   if {$xdp(DEBUG_MENU)} {puts "ENTER pullDownFileName"}
   regsub -all {\\} $fname / fname
   regsub -all {//} $fname / fname
   if {[string range $fname 0 0] == "~"} {
      set fname [string trimleft $fname "~"]
      set fname [file join "$xdp(DP_FEND)" $fname]
   }
   regsub {[file native "$xdp(DP_FEND)"]} $fname "" r

#  check file name if greater than 12 characters (8 + .top)
   if {[string length [file tail $fname]] > 12} {
      if {$xdp(DEBUG)} {errorMsg "WARNING: Length of file name in menu > 8 characters $r"} 
      set psl [string last "/" $fname]
      set new [string range $fname 0 $psl]
      append new [string range $fname [expr {$psl+1}] [expr {$psl+8}]]
      append new [file extension $fname]
      set fname $new
   }
   
#  check if file had wrong extension
   if {![file exists $fname]} {
      foreach i {.top .men} {
	if {[file extension $fname] == $i} {
	    set new [file rootname $fname]
	    if {$i == ".top"} {
	       append new ".men"
	    } else {
	       append new ".top"
	    }
	    if {[file exists $new]} {
	       if {$xdp(DEBUG)} {errorMsg "WARNING: Found file with wrong extension: $new"}
	       set fname $new
	       regsub "$xdp(DP_FEND)" $fname "" r
	    }
	 }
      }
   }
   return $fname
}
