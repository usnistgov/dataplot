proc topMenuBar {} {

   global xdp topMenu topBnam topBact topBpush
   global pdMenu PD

   if {$xdp(DEBUG)} {puts "ENTER TopMenuBar"}

#  define window title and location
   wm title    . "Dataplot"
   wm iconname . "Dataplot"
   wm geometry . +126+0
   wm iconbitmap . @[file join "$xdp(DP_TCL)" xdpIcon.xbm]
   frame .f1 

#  open main menu file & read in the top menu items
   set lines [pullDownReadfile [file join "$xdp(DP_FEND)" top.top]]

#  split the list into 2 lists: items & actions
   set PD(curr) 0
   set PD(total) 0
   set rmsiz [lindex $lines 1]
   set title [lindex $lines 2]
   set lines [lrange $lines 3 end]
   set topMenu(lines) $lines
   pullDownSplitLines $PD(curr)
   set items $topMenu(items)
   set actions $topMenu(actions)

#  determine number of items in the menu list 
   set lm [llength $topMenu(items)]
   set lm [expr {$lm/2 +$lm%2 - 1}]

#  create the top menu buttons from the menu list
   set nb -1
   set nc 0
   set nr 0
   set bpadx 0

#  loop over all the buttons
   foreach j $topMenu(items) {
      incr nb
      set topBnam($j) .f1.b$j
      set topBact($j) [lindex $topMenu(actions) $nb]

#  create the button and what happens when the button is clicked
      button $topBnam($j) -text $j -highlightthickness 0 -bg $xdp(COLOR,1) -width 8 \
	 -command {

	 set bpick [string trimleft $topBpush ".f1.b"]
	 set action [lindex $topMenu(actions) [lsearch $topMenu(items) $bpick]]

#  execute the action called for by the button
	 if {[regexp {xccl} $action]} {
	    pullDownWithdraw
	    pullDownCmd [lrange $action 1 end]

	 } elseif {$action == "exec exit"} {
	    pullDownWithdraw
	    sendDP "EXIT"
	    exit

	 } elseif {$action == "exec cis"} {
	    pullDownWithdraw
	    spawnCis

#  startup pull down menu
	 } else {
	    set pdfile [pullDownFileName $action]
	    set lines [pullDownReadfile $pdfile]

	    if {$lines != ""} {
	       cursorConfig watch
	       set PD(curr) 1
	       set rmsiz [lindex $lines 1]
	       set title [lindex $lines 2]
	       set lines [lrange $lines 3 end]
	       set pdMenu(lines) $lines
	       pullDownSplitLines $PD(curr)
	       set items $pdMenu(items)
	       set actions $pdMenu(actions)

#  destroy all current pdMenus before putting up a new one
	       for {set j $PD(total)} {$j >= 1} {incr j -1} {catch {destroy .pd$j}}
	       catch {destroy .varbrowse}
	       set PD(total) 1
	       set PD(curr) $PD(total)

#  pull down menu (not a room)
	       set ext [file extension $pdfile]
	       if {$ext == ".top"} {
		  pullDownMenu $title $lines $items $actions $rmsiz LB

#  room
	       } elseif {$ext == ".roo"} {
		  pullDownMenu $title $lines $items $actions $rmsiz RM
	       }
	       cursorConfig arrow
	    }
	 }
      }
      set bfont [$topBnam($j) cget -font]
      set bwid [font measure $bfont $j]
      if {$bwid > $bpadx} {set bpadx $bwid}
      
#  bind to determine which button was pushed
      bind $topBnam($j) <Button-1> {+ set topBpush %W}
      
#  grid the buttons      
      grid $topBnam($j) -row $nr -column $nc -sticky news
      incr nc
      if {$nc > $lm} {incr nr; set nc 0}
   }
   if {$bpadx < 65} {set bpadx [expr {int(72-$bpadx)/2}]} else {set bpadx 0}

#  reconfigurebuttons
   set nb -1
   foreach j $topMenu(items) {
      incr nb
      set action $topBact($j)
      if {$bpadx != 0} {$topBnam($j) configure -padx $bpadx}
      if {[string first "~" $topBact($j)] == -1} {
	 $topBnam($j) configure -bg $xdp(COLOR,3)
      } else {
	 $topBnam($j) configure -state disabled
      }

#  check to disable web and cis buttons         
      if {[string first "xccl: web" $action] != -1 && ![findCmd "$xdp(DP_BROWSER)"]} {
	 $topBnam($j) configure -state disabled
      }
      if {[string first "exec cis" $action] != -1} {
	 if {$xdp(OS) == "SunOS"} {
	    if {![findCmd cis]} {$topBnam($j) configure -state disabled}
	 } else {
	    $topBnam($j) configure -state disabled
	 }
      }
   }
   pack .f1 -side left -fill both -expand 1
   
#  compute new window geometry
   update idletasks
   set xdp(WID,.) [winfo width .]
   set xdp(HGT,.) [winfo height .]
   if {$xdp(WID,.) < [expr {$xdp(WID,screen)*0.7}]} {
      set xdp(WID,.) [expr {int($xdp(WID,screen)*0.7)}]
      wm geometry . $xdp(WID,.)\x$xdp(HGT,.)
   }
   if {$xdp(TOPPOS) == "center"} {
      set xx [expr {($xdp(WID,screen) - $xdp(WID,.))/2}]
   } elseif {$xdp(TOPPOS) == "right"} {
      set xx [expr {($xdp(WID,screen) - $xdp(WID,.)) - 10}]
   } elseif {$xdp(TOPPOS) == "left"} {
      set xx 10
   }
   if {$xdp(PLATFORM) == "unix"} {
      wm geometry . +$xx+[expr {$xdp(OFFSET,y) - 5}]
   } else {
      wm geometry . +$xx+0
   }
   wm resizable . 0 0
   update idletasks
}

################################################################################

#  command line entry window

proc commandLine {} {
   global xdp nRcl iRcl rclCmd cmdInput cmdLin formFocus tcl_platform

   if {$xdp(DEBUG)} {puts "ENTER commandLine"}
   set cmdLin(win) .input
   set w $cmdLin(win)
   catch {destroy $w}
   toplevel $w

   if {![info exists nRcl]} {set nRcl 0}
   if {![info exists rclCmd(0)]} {set rclCmd(0) ""}

#  set position of window relative to text output window
   wm title    $w "Command Line"
   wm iconname $w "Command"
   if {$xdp(OS) == "SunOS" && $tcl_platform(osVersion) == "5.5.1"} {update idletasks}
   set wid [winfo width .]
   set hgt 40
   set xx [expr {[winfo x .] + $xdp(OFFSET,x)}]
   set yy [expr {$xdp(HGT,screen)}]
   wm geometry $w $wid\x$hgt+$xx+$yy
   wm iconbitmap $w @[file join "$xdp(DP_TCL)" xdpIcon.xbm]

#  entry
   set f1 [frame $w.f1 -bg $xdp(COLOR,1)]
   set cmdLin(entry) [entry $f1.entry -textvariable cmdInput -state disabled]

#  buttons
   set blist ""
   set cmdLin(ok) [button $f1.ok -text "OK" -command {commandSend} -state disabled]
   button $f1.clear -text "Clear" -command {$cmdLin(entry) delete 0 end}
   button $f1.bvari -text "Variables" \
      -command {set formFocus $cmdLin(entry); formChoose $cmdLin(entry) vari}
   lappend blist $f1.ok $f1.clear $f1.bvari
   foreach b $blist {$b configure -padx $xdp(PADX) -pady $xdp(PADY) -highlightthickness 0}
   pack $f1.entry -side left -expand 1 -fill x -padx 8
   pack $f1.ok $f1.clear $f1.bvari -side left
   pack $f1 -expand 1 -fill both
   
   focus $cmdLin(entry)
   $cmdLin(entry) xview 0

#  set focus and bind Return in the entry to sending cmdInput to Dataplot
   bind $cmdLin(entry) <Return> {commandSend}

#  set bind to recall commands
   bind $cmdLin(entry) <Up> {
      if {![info exists iRcl]} {set iRcl $nRcl}
      if {$iRcl > 1} {incr iRcl -1}
      $cmdLin(entry) delete 0 end
      $cmdLin(entry) insert 0 $rclCmd($iRcl)
   }
   bind $cmdLin(entry) <Down> {
      if {![info exists iRcl]} {set iRcl $nRcl}
      if {$iRcl < $nRcl} {incr iRcl}
      if {$iRcl <= $nRcl} {
	 $cmdLin(entry) delete 0 end
	 $cmdLin(entry) insert 0 $rclCmd($iRcl)
      }
   }
   update idletasks
   winOnScreen $w $xx $yy
   update idletasks
}

################################################################################

#  send a command to Dataplot from the command line window

proc commandSend {} {
   global cmdInput cmdLin xdp

   pullDownWithdraw
   set cmd $cmdInput
   $cmdLin(entry) delete 0 end
   if {$xdp(DEBUG) == 0} {
      sendDP $cmd
   } else {
      puts " [expr {double([lindex [time {sendDP $cmd}] 0])/1000000.}] seconds"
   }
   set cmd [string tolower $cmd]
   if {$cmd == "exit" || $cmd == "quit"} {exit}
}
 
################################################################################

#  an entry window with OK, Cancel, Clear buttons

proc entryWin {str {default ""}} {
   global xdp ewInput

   set w .ewin
   catch {destroy $w}
   toplevel $w

   wm title $w $str
   wm iconname $w $str
   set xx [expr {[winfo x .textout] + $xdp(OFFSET,x) + 40}]
   set yy [expr {[winfo y .textout] + $xdp(OFFSET,y) + 40}]
   wm geometry $w +$xx+$yy
   wm iconbitmap $w @[file join "$xdp(DP_TCL)" xdpIcon.xbm]

   set f1 [frame $w.f1]
   label $f1.msg -text $str
   set ewInput ""
   entry $f1.entry -textvariable ewInput -width 55
   grid $f1.msg $f1.entry -padx 2 -pady 2
   grid $f1

   set f2 [frame $w.f2]
   button $f2.ok -text "OK" -command "destroy $w"
   button $f2.clear -text "Clear" -command "set ewInput \"\"; $f1.entry delete 0 end; update idletasks"
   button $f2.reset -text "Reset" -command "set ewInput [list $default]"
   button $f2.browse -text "Browse..." -command {set ewInput [tk_getOpenFile -title "Browse Command"]}
   button $f2.cancel -text "Cancel" -command "set ewInput [list $default]; destroy $w"

   grid $f2.ok $f2.clear $f2.reset $f2.browse $f2.cancel -padx 2 -pady 2
   grid $f2 -sticky e

   bind $f1.entry <Return> "destroy $w"
   focus $f1.entry
   $f1.entry delete 0 end
   $f1.entry insert 0 $default
   update idletasks
   wm resizable $w 0 0
   tkwait window $w
}

################################################################################

proc winParam { } {
   global xdp env

#  depending on OS, set window offsets
   if {$xdp(OS) == "IRIX"} {
      set xdp(OFFSET,y)  42
      set xdp(OFFSET,x)   0
      set xdp(OFFSET,x1) 18
   } elseif {$xdp(PLATFORM) == "unix"} {
      set xdp(OFFSET,y)   7
      set xdp(OFFSET,x)   [expr {-$xdp(OFFSET,y)}]
      set xdp(OFFSET,x1)  0
   } elseif {$xdp(PLATFORM) == "windows"} {
      set xdp(OFFSET,y)  30
      set xdp(OFFSET,x)   0
      set xdp(OFFSET,x1)  8
   }

   set xdp(WINTOP) ".textout"
   set xdp(WINBOT) ".cmdhist"
   
   if {[winfo depth .] >= 8} {
      set xdp(COLOR,1) bisque
      set xdp(COLOR,2) white
      set xdp(COLOR,3) grey91
   } else {
      set xdp(COLOR,1) grey91
      set xdp(COLOR,2) white
      set xdp(COLOR,3) grey91
   }
   if {$xdp(PLATFORM) == "unix"} {
      option add *background $xdp(COLOR,3)
   }

#  internal button padding, if used
   if {$xdp(PLATFORM) == "unix"} {
      set xdp(PADX) 5
      set xdp(PADY) 1
   } elseif {$xdp(PLATFORM) == "windows"} {
      set xdp(PADX) 0m
      set xdp(PADY) 0m
   }

#  set default fonts
   if {$xdp(PLATFORM) == "unix"} {
      set xdp(FONT,1) {Helvetica -12 bold}
      if {$xdp(OS) == "IRIX"} {
	 set xdp(FONT,2) {screen +10 bold roman}
	 set xdp(FONT,3) {screen +13 bold roman}
      } else {
	 set xdp(FONT,2) {fixed +10 bold roman}
	 set xdp(FONT,3) {fixed +11 bold roman}
      }
   } elseif {$xdp(PLATFORM) == "windows"} {
      set xdp(FONT,1) {{MS Sans Serif} 8 normal}
      set xdp(FONT,2) {Fixedsys 10 normal}
      set xdp(FONT,3) {Fixedsys 10 normal}
   }
   
#  set default executables
   if {$xdp(PLATFORM) == "unix"} {
      set xdp(DP_EXEC) dataplot
      set xdp(DP_LIB)  /usr/local/apps/dataplot/lib
      set xdp(DP_PRINT)   lp
      set xdp(DP_EDIT)    nedit
      set xdp(DP_BROWSER) netscape
   } elseif {$xdp(PLATFORM) == "windows"} {
      set xdp(DP_EXEC) "c:/Program Files/NIST/DATAPLOT/dplahey.exe"
      set xdp(DP_LIB)  "c:/Program Files/NIST/Dataplot"
      set xdp(DP_EDIT)    "c:/Program Files/Windows NT/Accessories/Wordpad.exe"
      set xdp(DP_PRINT)   $xdp(DP_EDIT)
      set xdp(DP_BROWSER) "c:/Program Files/Internet Explorer/Iexplore.exe"
   }

#  check if executables and/or libraries and/or fonts are specified in an
#  .xdpConfig or xdpConfig file and use those instead
#  
#  check the following directories for the file
#  1-current dir, 2-user's home dir, 3-dir with xdp Tcl code
   set font(variable) 1
   set font(fixed_small) 2
   set font(fixed_large) 3
   set found 0
   foreach dir {. $env(HOME) "$env(XDP_CODE)"} {
      foreach file {.xdpConfig xdpConfig} {
         eval set df $dir/$file
         if {$found == 0 && [file exists $df]} {
            set found 1
            set f [open $df r]
            while {[gets $f line] >= 0} {
               set line [string trim $line]
               set c1 [string range $line 0 0]
               if {$line != "" && $c1 != "#"} {
                  set line1 [lindex $line 0]
                  set line2 [lindex $line 1]
                  set line3 [lindex $line 2]
                  if {$line1 == $xdp(PLATFORM) || $line1 == $xdp(OS)} {
                     eval set xdp(FONT,$font($line2)) {$line3}
                  } elseif {$line2 == $xdp(PLATFORM) || $line2 == $xdp(OS)} {
                     if {$line1 == "dataplot"} {
                	eval set xdp(DP_EXEC) {$line3}
                     } elseif {$line1 == "library"} {
                	eval set xdp(DP_LIB) {$line3}
                     } elseif {$line1 == "print" || $line1 == "edit" || \
                        $line1 == "browser"} {
                        eval set xdp(DP_[string toupper $line1]) {$line3}
                     }
                  }
               }
            }
         }
      }
   }
   set xdp(PRINT_STATE) normal
   
#  actual font used
   for {set i 1} {$i <= 3} {incr i} {
      if {[info exists xdp(FONT,$i)]} {
	 set fa [font actual $xdp(FONT,$i)]
	 set xdp(FONT,$i,a)  "{[lindex $fa 1]} [lindex $fa 3] \
	    [lindex $fa 5] [lindex $fa 7]"
      }
   }

   option add *font             $xdp(FONT,1)
   option add *Listbox.font     $xdp(FONT,2)
   option add *Entry.font       $xdp(FONT,3)
   option add *Entry.background white
   option add *Radiobutton.padY 0m
   option add *Radiobutton.padX 0m
}

################################################################################

#  Look for commands in directories of PATH environment variable
#  or if it exists if an absolute path is given

proc findCmd {cmd} {
   global env xdp
   if {$xdp(PLATFORM) == "unix"} {set schar ":"}
   if {$xdp(PLATFORM) == "windows"} {set schar ";"}

   if {[file dirname $cmd] == "."} {
      if {[info exists env(PATH)]} {
	 foreach dir [split $env(PATH) $schar] {
	    set pathname [file join $dir $cmd]
	    if {[file exists $pathname]} {
	       if {[file isfile $pathname] && [file executable $pathname]} {return 1}
	    }
	 }
	 if {$cmd != ""} {
	    set msg "ERROR: $cmd  not found in PATH environment variable"
            puts $msg
            catch {pageWrite .textout $msg}
	 }
      }
   } else {
      if {[file exists $cmd]} {return 1}
   }
   return 0
}

################################################################################

proc cursorConfig {type} {
   global PD xdp cmdLin

#  change background color of command line entry
   if {$type == "watch"} {
      catch {$cmdLin(entry) configure -bg $xdp(COLOR,1)}
      catch {$cmdLin(entry) configure -state disabled}
   }
   if {$type == "arrow"} {
      catch {$cmdLin(entry) configure -bg $xdp(COLOR,2)}
      catch {$cmdLin(entry) configure -state normal}
   }
   
#  create list of windows
   set win {.cmdhist .graph .textout .pixmap .data .help .gset1 .gset2 .list}
   if {$xdp(OS) != "SunOS"} {lappend win .}
   for {set i 1} {$i <= $PD(total)} {incr i 1} {
      if {[winfo exists .pd$i]} {lappend win .pd$i}
   }

#  change cursor configuration
   foreach i $win {
      if {[winfo exists $i]} {catch {$i config -cursor "$type red white"}}
   }
   update idletasks
}

################################################################################

proc winOnScreen {w xx yy {noresize 1}} {
   global xdp

#  reposition window so it's all on the screen

   update idletasks
   if {[expr {[winfo width $w] + [winfo x $w]}] > $xdp(WID,screen)} {
      set xx [expr {$xdp(WID,screen) - [winfo width $w] - 10}] 
      wm geometry $w +$xx+$yy
      if {$xdp(DEBUG)} {puts " $w repositioned: $xx $yy"}
   }
   if {[expr {[winfo height $w] + [winfo y $w]}] > $xdp(HGT,screen)} {
      set yy [expr {$xdp(HGT,screen) - [winfo height $w] - 30}] 
      wm geometry $w +$xx+$yy
      if {$xdp(DEBUG)} {puts " $w repositioned: $xx $yy"}
   }
   
   if {$noresize} {wm resizable $w 0 0}
}

################################################################################

proc winCrop {w xx yy {noresize 1}} {
   global xdp

#  crop the window size so it's all on the screen

   update idletasks
   set resized 0
   set d [expr {[winfo y $w] + [winfo height $w] - $xdp(HGT,screen)}]
   if {$d > 0} {
      set xx [winfo width $w]
      set yy [expr {[winfo height $w] - $d - 20}]
      wm geometry $w $xx\x$yy
      if {$xdp(DEBUG)} {puts " $w cropped: $d $xx $yy"}
      set resized 1
   }
   
   if {$noresize} {wm resizable $w 0 0}
   return [list $resized $xx $yy]
}

################################################################################

proc errorMsg {text} {

#  print message in text output window and standard output

   if {[winfo exists .textout]} {
      pageWrite .textout $text
      .textout.f2.list yview end
   }
   puts \a$text
}

################################################################################

proc cmdSetup {type} {
   global ewInput xdp topMenu topBact topBnam PD

   entryWin "$type command:" $xdp(DP_$type)
   set cmd [string trim $ewInput]

   if {[findCmd $cmd]} {set state normal} else {set state disabled}

   if {$type == "EDIT"} {
      foreach w {.textout .cmdhist .help .list} {
         $w.f1.file.menu entryconfigure 2 -state $state
      }
   }
   if {$type == "PRINT"} {
      foreach w {.textout .cmdhist .help .list} {
         $w.f1.file.menu entryconfigure 4 -state $state
      }
      for {set i 1} {$i <= $PD(total)} {incr i 1} {
         .pd$i.f0.rbutton.menu entryconfigure 2 -state $state 
      }
      .pixmap.f1.print configure -state $state 
      set xdp(PRINT_STATE) $state
   }
   if {$type == "BROWSER"} {
      foreach j $topMenu(items) {
	 set action $topBact($j)
	 if {[string first "xccl: web" $action] != -1} {
	    $topBnam($j) configure -state $state
	 }
      }
   }

   return $cmd
}

################################################################################

proc scrollBoth {lists args} {
   foreach l $lists {eval {$l yview} $args}
}
