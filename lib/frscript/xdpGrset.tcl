proc graphSet {w} {
   global GS grSetData grSetSave xdp VB vbNam
   
   if {$xdp(DEBUG)} {puts "ENTER graphSet $w"}
   cursorConfig watch

#  initialize
   if {![winfo exists $w]} {
      set GS(rjust) {THICKNESS SIZE WIDTH ANGLE BASE SPACING \
	 DISPLACEMENT COORDINATES DECIMALS NUMBER}
      set GS(formatent) [lrange $GS(rjust) 0 [expr {[llength $GS(rjust)] - 2}]]

      if {![info exists VB(title,vari)]} {
	 set VB(title,vari) "Variables"
	 set VB(title,line) "Lines"
	 set VB(title,char) "Characters"
	 set VB(title,fill) "On / Off"
	 set VB(title,ooff) "On / Off"
	 set VB(title,inou) "In / Out"
	 set VB(title,case) "Cases"
	 set VB(title,font) "Fonts"
	 set VB(title,just) "Justifications"
	 set VB(title,colo) "Colors"
	 set VB(title,dire) "Directions"
	 set VB(title,patt) "Patterns"
	 set VB(title,thic) "Thickness"
	 set VB(title,unit) "Units"
	 set VB(title,size) "Size"
	 set VB(title,angl) "Angle"
	 set VB(title,widt) "Width"
	 set VB(title,spac) "Spacing"
      }

      if {![info exists VB(var,line)]} {
	 set VB(var,line) {SOLI DOTT DASH DA1 DA2 DA3 DA4 BL}
	 set VB(var,char) {BLAN X - + * # CIRC SQUA DIAM TRIA TRIR STAR \
	    PYRA CUBE ARRO AU AD VECT POIN 1 2 3 4 5 6 7 8 9 0 ALPH BETA GAMM}
	 set VB(var,fill) {ON OFF}
	 set VB(var,ooff) {ON OFF}
	 set VB(var,inou) {IN OUT}
	 set VB(var,case) {UPPE LOWE ASIS}
	 set VB(var,font) {TEKT SIMP DUPL TRIP COMP TRII SIMS COMS}
	 set VB(var,just) {LEFT CENT RIGH LECE CECE RICE LETO CETO RITO LEBO CEBO RIBO}
	 set VB(var,colo) {BLAC RED GREE BLUE CYAN MAGE YELL WHIT} 
	 set VB(var,dire) {HORI VERT}
	 set VB(var,patt) {BLAN SOLI HORI VERT D1 D2 D1D2 VED1 VED1 HOD1 HOD2 VEDD}
	 set VB(var,thic) {0.1 0.2 0.3 0.4 0.5}
	 set VB(var,unit) {ABSO DATA}
	 set VB(var,size) {0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0}
	 set VB(var,angl) {0. 30. 45. 60. 90.}
	 set VB(var,none) {}
	 set VB(var,widt) $VB(var,size)
	 set VB(var,spac) $VB(var,size)
	 set vbNam {vari}
	 foreach n [array names VB] {
            if {[string range $n 0 3] == "var,"} {lappend vbNam [string range $n 4 7]}
         }
      }

#  open graph parameters window
      toplevel $w
      if {$w == ".gset1"} {set title "Graph Settings"}
      if {$w == ".gset2"} {set title "More Graph Settings"}
      wm title $w $title
      wm iconname $w $title
      set xx [expr {[winfo x .graph] + $xdp(OFFSET,x)}]
      set yy [expr {[winfo y .graph] + [winfo height .graph] + $xdp(OFFSET,y)}]
      wm geometry $w [winfo width .data]\x[winfo height .data]+$xx+$yy
      wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]
      if {$w == ".gset1"} {wm withdraw $w}
      set blist ""

#  create a canvas
      frame $w.f1 -bd 1 -relief groove
      set GS(canvas,w) [canvas $w.f1.c -highlightthickness 0 -bg $xdp(COLOR,1) \
	 -height [expr {$xdp(HGT,.textout) - 60}] -width $xdp(WID,.graph) \
	 -yscrollcommand [list $w.f1.yscroll set] \
	 -xscrollcommand [list $w.f1.xscroll set]] 
      scrollbar $w.f1.xscroll -relief sunken -command [list $GS(canvas,w) xview] \
	 -orient horiz -takefocus 0
      scrollbar $w.f1.yscroll -relief sunken -command [list $GS(canvas,w) yview] \
	 -takefocus 0

      grid $GS(canvas,w) $w.f1.yscroll -sticky news
      grid $w.f1.xscroll -row 1 -column 0 -sticky ew
      grid rowconfigure $w.f1 0 -weight 1
      grid columnconfigure $w.f1 0 -weight 1

#  create frame in the canvas
      set GS(frame,w) [frame $GS(canvas,w).f -bg $xdp(COLOR,1)]
      $GS(canvas,w) create window 0 0 -anchor nw -window $GS(frame,w)
      set f $GS(frame,w)

      set GS(bind,w) ""
      set nrow -1
      set nrow [graphSetSpace $nrow $w]

#  entries for first window
      if {$w == ".gset1"} {

#  create entries with only one entry
	 set GS(set1) {TITLE X1LABEL Y1LABEL X2LABEL X3LABEL Y2LABEL}
	 foreach p $GS(set1) {set nrow [graphSetEntry1 $nrow $p 8 $w]}
	 set nrow [graphSetSpace $nrow $w]
	 focus $f.entry1

#  create entries, with 8 per line
	 set GS(first) 1
	 set GS(lim4) 8
	 set GS(set4) {CHARACTER LINE SPIKE BAR}
	 foreach p $GS(set4) {set nrow [graphSetEntry4 $nrow $p $GS(lim4) $w]}
	 set nrow [graphSetSpace $nrow $w]

#  create entries, 4 per line
	 set GS(first) 1
	 set GS(set3) {TIC "TIC POSITION" "TIC LABEL" " " LOG FRAME}
	 set GS(subset3) {X1 Y1 X2 Y2}
	 foreach p $GS(set3) {set nrow [graphSetEntry3 $nrow $p $GS(subset3) $w]}
	 set nrow [graphSetSpace $nrow $w]

#  create entries, with one per line
	 set GS(lim2) 4
	 set GS(set2) {LEGEND " " "LEGEND COORDINATES"}
	 foreach p $GS(set2) {set nrow [graphSetEntry2 $nrow $p $GS(lim2) $w]}
	 set nrow [graphSetSpace $nrow $w]

#  create entries, 2 per line
	 set GS(first) 1
	 set GS(set5) {GRID "GRID PATTERN"}
	 set GS(subset5) {X Y}
	 foreach p $GS(set5) {set nrow [graphSetEntry3 $nrow $p $GS(subset5) $w]}
	 set nrow [graphSetSpace $nrow $w]

	 set s {"FRAME COORDINATES" " " X1LIMITS Y1LIMITS X2LIMITS Y2LIMITS}
	 foreach p $s {set nrow [graphSetEntry1 $nrow $p 5 $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set1) " $s"

#  entries for second window
      } elseif {$w == ".gset2"} {
	 set GS(first) 1
	 set s {"LINE COLOR" "LINE THICKNESS" " " \
	    "CHARACTER COLOR" "CHARACTER FILL" "CHARACTER FONT" "CHARACTER CASE" \
	    "CHARACTER JUSTIFICATION" "CHARACTER DIRECTION" \
	    "CHARACTER SIZE" "CHARACTER WIDTH" "CHARACTER ANGLE" "CHARACTER THICKNESS" " " \
	    "BAR FILL" "BAR FILL COLOR" "BAR BORDER LINE" "BAR BORDER COLOR" \
	    "BAR BORDER THICKNESS" "BAR PATTERN" "BAR PATTERN LINE" \
	    "BAR PATTERN COLOR" "BAR PATTERN THICKNESS" "BAR PATTERN SPACING" \
	    "BAR BASE" "BAR WIDTH" " " \
	    "SPIKE LINE" "SPIKE COLOR" "SPIKE THICKNESS" "SPIKE BASE"}
	 foreach p $s {set nrow [graphSetEntry4 $nrow $p $GS(lim4) $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set4) " $s"

	 set GS(first) 1
	 set GS(set6) {TITLE X1LABEL Y1LABEL X2LABEL X3LABEL Y2LABEL}
	 set GS(subset6) {FONT CASE FILL COLOR SIZE THICKNESS DISPLACEMENT}
	 foreach p $GS(set6) {set nrow [graphSetEntry5 $nrow $p $GS(subset6) $w]}
	 set nrow [graphSetSpace $nrow $w]

	 set GS(first) 1
	 set s {"LEGEND JUSTIFICATION" "LEGEND COLOR" "LEGEND FONT" "LEGEND CASE" \
	    "LEGEND DIRECTION" "LEGEND FILL" "LEGEND SIZE" \
	    "LEGEND WIDTH" "LEGEND THICKNESS" "LEGEND ANGLE"}
	 foreach p $s {set nrow [graphSetEntry4 $nrow $p $GS(lim4) $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set4) " $s"

	 set GS(first) 1
	 set s {"FRAME COLOR"}
	 foreach p $s {set nrow [graphSetEntry3 $nrow $p $GS(subset3) $w]}
	 append GS(set3) " $s"
	 set nrow [graphSetSpace $nrow $w]

	 set s {"FRAME THICKNESS"}
	 foreach p $s {set nrow [graphSetEntry1 $nrow $p 1 $w]}
	 append GS(set1) " $s"
	 set nrow [graphSetSpace $nrow $w]

	 set GS(first) 1
	 set s {"TIC COLOR" "TIC SIZE" "TIC LABEL COLOR" "TIC LABEL CASE" \
            "TIC LABEL FONT"  "TIC LABEL JUSTIFICATION" "TIC LABEL DIRECTION" \
            "TIC LABEL FILL" "TIC LABEL DECIMALS" "TIC LABEL DISPLACEMENT" \
            "TIC LABEL ANGLE" "TIC LABEL SIZE" "MAJOR TIC MARK NUMBER" \
            "MINOR TIC MARK NUMBER"}
	 foreach p $s {set nrow [graphSetEntry3 $nrow $p $GS(subset3) $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set3) " $s"

	 set s {"X1TIC OFFSET" "Y1TIC OFFSET" "X2TIC OFFSET" "Y2TIC OFFSET"}
	 foreach p $s {set nrow [graphSetEntry1 $nrow $p 5 $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set1) " $s"

	 set s {"TIC LABEL THICKNESS" "TIC OFFSET UNITS" " " "BACKGROUND COLOR"}
	 foreach p $s {set nrow [graphSetEntry1 $nrow $p 1 $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set1) " $s"

	 set GS(first) 1
	 set s {"GRID COLOR" "GRID THICKNESS"}
	 foreach p $s {set nrow [graphSetEntry3 $nrow $p $GS(subset5) $w]}
	 set nrow [graphSetSpace $nrow $w]
	 append GS(set5) " $s"
      }

#  buttons
      frame $w.f2 -bd 1 -relief groove
      button $w.f2.apply -text "Apply" -command {graphSetApply curr}
      button $w.f2.replot -text "Replot" -command {sendDP "REPLOT"}
      button $w.f2.refresh -text "Refresh" -command {sendGraphics ALL}
      button $w.f2.state -text "State" -command {graphSetApply save}
      lappend blist $w.f2.apply $w.f2.replot $w.f2.refresh $w.f2.state 
      menubutton $w.f2.help -text "Help" -menu $w.f2.help.menu
      pageMenuHelp $w.f2.help.menu [list "Plot Control Commands"]
      if {$w == ".gset1"} {
	 button $w.f2.more -text "More Settings" -command {graphSet .gset2}
	 lappend blist $w.f2.more
      }
      button $w.f2.kill -text "X" -command "wm withdraw $w; catch {destroy .varbrowse}"
      lappend blist $w.f2.kill
      foreach b $blist {$b configure -padx $xdp(PADX) -pady $xdp(PADY)}

      pack $w.f2.apply $w.f2.replot $w.f2.refresh $w.f2.state -side left
      pack $w.f2.help -side left
      pack $w.f2.kill -side right
      if {$w == ".gset1"} {pack $w.f2.more -side right -padx 2}

      pack $w.f2 -fill x
      pack $w.f1 -expand 1 -fill both

#  bindings
      for {set i 0} {$i < [expr {[llength $GS(bind,w)]-1}]} {incr i} {
	 bind [lindex $GS(bind,w) $i] <Return> "focus [lindex $GS(bind,w) [expr {$i+1}]]"
      }
      set el [lindex $GS(bind,w) [expr {[llength $GS(bind,w)]-1}]]
      set ef [lindex $GS(bind,w) 0]
      bind $el <Return> "focus $ef"

      update idletasks
      $GS(canvas,w) configure -scrollregion [grid bbox $GS(frame,w)]

      if {$w == ".gset2"} {
         focus $w.f2.kill
      } else {
         foreach p {"SET X11 FONT" "SET POSTSCRIPT FONT"} {
            set grSetData($p) ""; set grSetSave(curr,$p) ""; set grSetSave(save,$p) ""
         }
      }

#  show window if it already exists
   } else {
      wm deiconify $w
      raise $w
   }

   cursorConfig arrow
}

###############################################################################

proc graphSetEntry1 {nrow p cs w} {
   global GS grSetData grSetSave xdp

#  entries for TITLE, X1LABEL, FRAME COORDINATES, etc.
   if {$p != " "} {
      incr nrow
      regsub {[XY][12]} $p "" help
      graphSetHelpButton $nrow $p $w $help
      set e [entry $GS(frame,w).entry$nrow -textvariable grSetData($p) -width 5]
      grid $e -row $nrow -column 1 -columnspan $cs -sticky we
      if {$p == "TITLE" || [string range $p 2 6] == "LABEL"} {
         graphSetClearButton $nrow $p [expr {$cs+1}]
      }	
      lappend GS(bind,w) $e
      graphSetFocusBind $p $e
   } else {
      set nrow [graphSetSpace $nrow $w]
   }
   return $nrow
}

###############################################################################

proc graphSetEntry2 {nrow p max w} {
   global GS grSetData grSetSave xdp

#  entries for LEGEND only
   if {$p != " "} {
      for {set i 1} {$i <= $max} {incr i} {
	 incr nrow
	 if {$i == 1} {graphSetHelpButton $nrow $p $w $p}
	 set l [label $GS(frame,w).lable$nrow -text $i -relief groove -width 5]
	 grid $l -row $nrow -column 1 -sticky news -ipadx 1m
	 set e [entry $GS(frame,w).entry$nrow -textvariable grSetData($p,$i)]
	 set cs 7
	 grid $e -row $nrow -column 2 -columnspan $cs -sticky we
	 if {$p == "LEGEND"} [list graphSetClearButton $nrow $p,$i [expr {$cs+2}]]
	 lappend GS(bind,w) $e
	 graphSetFocusBind $p $e
      }
   } else {
      set nrow [graphSetSpace $nrow $w]
   }
   return $nrow
}

###############################################################################

proc graphSetEntry3 {nrow p subset w} {
   global GS grSetData grSetSave xdp formFocus VB
   
#  entries for TIC, GRID, etc. vs. X,Y or X1,Y1,X2,Y2
   if {$GS(first)} {set nrow [graphSetEntryHeader $nrow $subset $w]}
   if {$p != " "} {
      set just left
      foreach s $GS(rjust) {if {[string first $s $p] != -1} {set just right}}
      incr nrow
      graphSetHelpButton $nrow $p $w
      for {set i 1} {$i <= [llength $subset]} {incr i} {
	 set j [lindex $subset [expr {$i-1}]]
	 if {[lrange $p 1 3] != "TIC MARK NUMBER"} {
	    set jp $j$p
	 } else {
	    set jp "[lindex $p 0] $j[lrange $p 1 3]"
	 }
	 set e [entry $GS(frame,w).e[expr {$i*1000+$nrow}] \
	    -textvariable grSetData($jp) -width 5 -justify $just]
	 grid $e -row $nrow -column $i -sticky we
	 lappend GS(bind,w) $e
         graphSetFocusBind $p $e
      }
      graphSetAllButton $p $nrow $i $subset
   } else {
      set nrow [graphSetSpace $nrow $w]
   }
   return $nrow
}

###############################################################################

proc graphSetEntry4 {nrow p max w} {
   global GS grSetData grSetSave xdp VB

#  entries for CHARACTER, LINE, BAR, SPIKE
   if {$GS(first)} {
      set subset ""
      for {set s 1} {$s <= $max} {incr s} {append subset "$s "}
      set nrow [graphSetEntryHeader $nrow $subset $w]
   }
   if {$p != " "} {
      set just left
      foreach s $GS(rjust) {if {[string first $s $p] != -1} {set just right}}
      incr nrow
      graphSetHelpButton $nrow $p $w
      for {set i 1} {$i <= $max} {incr i} {
         set e [entry $GS(frame,w).e[expr {$i*1000+$nrow}] -textvariable grSetData($p,$i) \
            -width 5 -justify $just]
         grid $e -row $nrow -column $i -sticky we
         lappend GS(bind,w) $e
         graphSetFocusBind $p $e
      }
      graphSetAllButton $p $nrow $i $max
   } else {
      set nrow [graphSetSpace $nrow $w]
   }
   return $nrow
}

###############################################################################

proc graphSetEntry5 {nrow p subset w} {
   global GS grSetData grSetSave xdp

#  entries for TITLE FONT, X1LABEL COLOR, etc. in a table
   if {$GS(first)} {
      set nc 0
      incr nrow
      foreach s $subset {
	 graphSetHelpButton $nrow [string range $s 0 3] $w "TITLE $s" [incr nc]
      }
      set GS(first) 0
   }
   if {$p != " "} {
      incr nrow
      set l [label $GS(frame,w).l$nrow$p -text $p -bg $xdp(COLOR,1) -justify right]
      grid $l -row $nrow -column 0 -sticky e
      set nc 0
      foreach j $subset {
         incr nc
         set pj "$p $j"
	 set just left
	 foreach s $GS(rjust) {if {[string first $s $pj] != -1} {set just right}}
	 set e [entry $GS(frame,w).e[expr {$nc*1000+$nrow}] \
	    -textvariable grSetData($pj) -width 5 -justify $just]
	 grid $e -row $nrow -column $nc
	 lappend GS(bind,w) $e
         graphSetFocusBind $pj $e
      }
   } else {
      set nrow [graphSetSpace $nrow $w]
   }
   return $nrow
}

###############################################################################

proc graphSetAllButton {p nrow j max} {
   global xdp GS VB
   
#  an ALL button that sets all entries to a value
   set ok 0
   foreach str {CHARACTER LINE SPIKE BAR "SPIKE LINE" "BAR BORDER LINE" \
      "CHARACTER FILL" "BAR FILL" "LEGEND FILL" "BAR PATTERN LINE" \
      "BAR PATTERN" "TIC LABEL FILL"} {
      if {$p == $str} {set ok 1}
   }
   foreach str {TIC "TIC LABEL" LOG FRAME GRID "TIC POSITION" "GRID PATTERN"} {
      if {$p == $str} {set ok 1}
   }
   foreach str {COLOR FONT CASE JUSTIFICATION DIRECTION THICKNESS SIZE \
      WIDTH ANGLE SPACING} {
      if {[string first $str $p] != -1} {set ok 1}
   }
   if {$ok} {
      set b [menubutton $GS(frame,w).all$nrow -text All -width 5 \
	 -takefocus 0 -highlightthickness 0 -bg $xdp(COLOR,1) \
	 -padx $xdp(PADX) -pady $xdp(PADY) -relief raised \
	 -menu $GS(frame,w).all$nrow.menu]
      grid $b -row $nrow -column $j -sticky we -padx 1m
      set m [menu $GS(frame,w).all$nrow.menu -tearoff 0]

      set type none
      if {$p == "CHARACTER"} {set type char}
      foreach str {LINE "SPIKE LINE" "BAR BORDER LINE" "BAR PATTERN LINE" \
         "GRID PATTERN"} {if {$p == $str} {set type line}}
      foreach str {SPIKE BAR "CHARACTER FILL" "LEGEND FILL" "BAR FILL" TIC \
         "TIC LABEL" LOG FRAME GRID "TIC LABEL FILL"} {if {$p == $str} {set type ooff}}
      if {$p == "BAR PATTERN"} {set type patt}
      if {$p == "TIC POSITION"} {set type inou}
      foreach str {COLOR FONT CASE JUSTIFICATION DIRECTION THICKNESS ANGLE \
         SIZE WIDTH SPACING} {
         set l4 [string tolower [string range $str 0 3]]
         if {[string first $str $p] != -1} {set type $l4}
      }
      if {[llength $max] == 1} {
	 foreach v $VB(var,$type) {
            $m add command -label $v -command [list graphSetAll $max $p $v]
	 }
      } else {
	 foreach v $VB(var,$type) {
            $m add command -label $v -command [list graphSetAll1 $max $p $v]
	 }
      }
   }
}

###############################################################################

proc graphSetAll {max p v} {
   global grSetData
   for {set j 1} {$j <= $max} {incr j} {
      set grSetData($p,$j) $v
   }
}

###############################################################################

proc graphSetAll1 {subset p v} {
   global grSetData
   for {set i 1} {$i <= [llength $subset]} {incr i} {
      set j [lindex $subset [expr {$i-1}]]
      if {[lrange $p 1 3] != "TIC MARK NUMBER"} {
	 set jp $j$p
      } else {
	 set jp "[lindex $p 0] $j[lrange $p 1 3]"
      }
      set grSetData($jp) $v
   }
}

###############################################################################

proc graphSetClearButton {nrow index cs} {
   global GS xdp

#  a CLEAR button that clears the value of an entry
   set b [button $GS(frame,w).clear$nrow -text Clear \
      -takefocus 0 -highlightthickness 0 -bg $xdp(COLOR,1) \
      -command [list set grSetData($index) ""] -padx $xdp(PADX) -pady $xdp(PADY)]
   grid $b -row $nrow -column $cs -sticky we -padx 1m
}

###############################################################################

proc graphSetEntryHeader {nrow subset w} {
   global GS xdp

   incr nrow
   set nc 0
   foreach s $subset {
      incr nc
      set s [string trim $s]
      set l [label $GS(frame,w).head$nrow$s -text $s -width 5 -bg $xdp(COLOR,1)]
      grid $l -row $nrow -column $nc
   }
   set GS(first) 0
   return $nrow
}

###############################################################################

proc graphSetSpace {nrow w} {
   global GS xdp
   
#  blank line (empty label)
   incr nrow
   set l [label $GS(frame,w).l$nrow -text " " -bg $xdp(COLOR,1)]
   grid $l -row $nrow -column 0 -sticky e -pady 1m
   return $nrow
}

###############################################################################

proc graphSetHelpButton {nrow p w {h ""} {nc 0}} {
   global GS xdp

#  a HELP button
   if {$h == ""} {set h $p}
   set b [button $GS(frame,w).help[expr {100*$nc+$nrow}] -text $p \
      -takefocus 0 -highlightthickness 0 -bg $xdp(COLOR,1) \
      -command [list sendDP "HELP $h"] -padx $xdp(PADX) -pady $xdp(PADY)]
   if {$nc == 0} {set stick e} else {set stick we}
   grid $b -row $nrow -column $nc -sticky $stick -padx 1m
}

###############################################################################

proc graphSetFocusBind {p e} {
   global GS formFocus browseFocus

   set nobind 1
   if {![info exists GS(focus,fill)]} {
      set GS(focus,fill) {SPIKE BAR TIC FRAME GRID LOG "TIC LABEL"}
      set GS(focus,line) {LINE "SPIKE LINE" "GRID PATTERN" "BAR BORDER LINE" \
         "BAR PATTERN LINE"}
      set GS(focus,char) {CHARACTER}
      set GS(focus,inou) {"TIC POSITION"}
      set GS(focus,patt) {"BAR PATTERN"}
   }
   
#  look for the following strings from GS(focus,...)
   foreach t {fill line char inou patt} {
      foreach b $GS(focus,$t) {
	 if {$p == $b} {
            set GS($e,btyp) $t
            bind $e <FocusIn> {
               set formFocus [focus]
               catch {formChoose $formFocus $GS($formFocus,btyp)}
            }
            set nobind 0
	 }
      }
   }

#  look for the following strings
   foreach b {COLOR CASE FONT JUSTIFICATION DIRECTION THICKNESS FILL UNIT \
      ANGLE WIDTH SIZE SPACING} {
      if {$nobind} {
	 set f [string first $b $p]
	 if {$f != -1} {
            set GS($e,btyp) [string tolower [string range $b 0 3]]
            bind $e <FocusIn> {
               set formFocus [focus]
               catch {formChoose $formFocus $GS($formFocus,btyp)}
            }
            set nobind 0
	 }
      }
   }
   
#  no binding created   
   if {$nobind} {
      bind $e <FocusIn> {catch {destroy .varbrowse}; set browseFocus a}
   }
}

###############################################################################

proc graphSetUpdate {gsdata} {
   global GS grSetData grSetSave xdp

   if {$xdp(DEBUG)} {puts "ENTER graphSetUpdate"}
   cursorConfig watch
   
#  read through dsdata (output from GUI SAVE PLOT CONTROL command)
   foreach line $gsdata {
      set line [string trim $line]
      
#  look for an equal
      set eq [string first "=" $line]
      if {$eq != -1} {set eq [lsearch [string range $line 0 $eq] "="]}

#  only process lines with an = sign in them
      if {$eq != -1} {
	 set ll [llength $line]
	 set l0 [lindex $line 0]
	 set l1 [lindex $line 1]
         set eqm1 [expr {$eq-1}]
         set eqm2 [expr {$eq-2}]
         set eqm3 [expr {$eq-3}]
         set eqp1 [expr {$eq+1}]
         set eqp2 [expr {$eq+2}]

#  read TITLE and LABEL entries
         if {$l0 == "TITLE" || $l0 == "FRAME" || $l0 == "TIC" || \
            $l0 == "BACKGROUND" || [string first LABEL $l0] != -1} {
            set p [lrange $line 0 $eqm1]
            if {$ll > $eqp1} {
               set formatent [graphSetFormatEnt $p]
               set v [lrange $line $eqp1 end]
               if {$formatent} {set v [graphSetFormatVal $v]}
	       set grSetData($p) $v
            } else {
               set grSetData($p) ""
            }
            set grSetSave(curr,$p) $grSetData($p)

#  read CHARACTER, LINE, BAR, SPIKE, and LEGEND entries
         } elseif {$l0 == "CHARACTER" || $l0 == "LINE" || \
                   $l0 == "BAR" || $l0 == "SPIKE" || $l0 == "LEGEND"} {
            set p [lrange $line 0 $eqm3]
            set formatent [graphSetFormatEnt $p]
            set i1 [expr {[lindex $line $eqm2] - 1}]
            graphSetUpdateSet $line $p $eqp1 $ll $i1 $formatent

#  read TIC, FRAME, and LOG entries
         } elseif {$l0 == "X1,"} {
            set n 4
            if {[lindex $line $n] == "TIC" || [lindex $line $n] == "FRAME" || \
                [lindex $line $n] == "LOG"} {
               set p [lrange $line $n $eqm1]
               set formatent [graphSetFormatEnt $p]
               graphSetUpdateSet1 $line $p $eqp1 $formatent $n
            } else {
               if {$xdp(DEBUG)} {puts " $line"}
            }

#  read GRID entries
         } elseif {$l0 == "X,"} {
            set n 2
            if {[lindex $line $n] == "GRID"} {
               set p [lrange $line $n $eqm1]
               set formatent [graphSetFormatEnt $p]
               graphSetUpdateSet1 $line $p $eqp1 $formatent $n
            } else {
               if {$xdp(DEBUG)} {puts " $line"}
            }

#  read LIMITS and TIC OFFSET entries
         } elseif {$l1 == "LIMITS" || $l1 == "TIC"} {
            set p [lrange $line 0 1]
            regsub " " $p "" p
            if {$eq > 2} {append p " [string trim [lrange $line 2 $eqm1]]"}
            set grSetData($p) [lrange $line $eqp1 end]
            set grSetSave(curr,$p) $grSetData($p)

#  read NUMBER OF LEGENDS
         } elseif {$l0 == "NUMBER" && $l1 == "OF"} {
            set nlegend [lindex $line 6]
            for {set i 10} {$i > $nlegend} {incr i -1} {
               set grSetData(LEGEND,$i) ""
               set grSetSave(curr,LEGEND,$i) ""
            }

         } else {
            if {$xdp(DEBUG)} {puts " $line"}
         }
      } else {
         if {$xdp(DEBUG)} {puts " $line"}
      }
   }

   update idletasks
   cursorConfig arrow
}

###############################################################################

proc graphSetUpdateSet {line p eqp1 ll i1 formatent} {
   global grSetData grSetSave

#  set CHARACTER, LINE, BAR, SPIKE entries
   if {$p != "LEGEND" && $p != "LEGEND COORDINATES"} {
      for {set i $eqp1} {$i < $ll} {incr i} {
	 incr i1
	 set v [lindex $line $i]
	 if {$formatent} {set v [graphSetFormatVal $v]}
	 set grSetData($p,$i1) $v
	 set grSetSave(curr,$p,$i1) $grSetData($p,$i1)
      }

#  set LEGEND and LEGEND COORDINATES entries
   } else {
      incr i1
      set v [lrange $line $eqp1 end]
      if {$formatent} {set v [graphSetFormatVal $v]}
      set grSetData($p,$i1) $v
      set grSetSave(curr,$p,$i1) $grSetData($p,$i1)
   }
}

###############################################################################

proc graphSetUpdateSet1 {line p eqp1 formatent n} {
   global GS grSetData grSetSave

   for {set i 0} {$i < $n} {incr i} {
      regsub "," [lindex $line $i] "" i1
      set v [lindex $line [expr {$eqp1+$i}]]
      if {$formatent} {set v [graphSetFormatVal $v]}
      if {[lrange $line 4 5] != "TIC NUMBER"} {
         set j $i1$p
      } else {
         set j "[lindex $line 6] $i1"
         append j "TIC MARK NUMBER"
      }
      set grSetData($j) $v
      set grSetSave(curr,$j) $grSetData($j)
   }
}

###############################################################################

proc graphSetFormatVal {v} {
   global GS
   
   set vf ""
   for {set i 0} {$i < [llength $v]} {incr i} {
      append vf "[format "%15.7e" [lindex $v $i]] "
   }
   return $vf
}

###############################################################################

proc graphSetFormatEnt {p} {
   global GS

#  depending on the type of setting, set if entry is to be formatted
   set formatent 0
   foreach s $GS(formatent) {if {[string first $s $p] != -1} {set formatent 1}}
   return $formatent
}

###############################################################################

proc graphSetApply {type} {
   global GS grSetData grSetSave
   
   set GS(noecho) 1
   set sendGrf 0
   set GS(state) ""

#  loop through all indices of grSetData
   foreach c [array names grSetData] {

#  look for a comma
      set comma [string first "," $c]

#  no comma found
      if {$comma == -1} {
         if {$grSetData($c) != $grSetSave($type,$c)} {
            set cmd "$c $grSetData($c)"
            set sendGrf [graphSetApplySet $cmd $c $type]
         }

#  comma found
      } else {
         if {$grSetData($c) != $grSetSave($type,$c)} {
            set cm1 [expr {$comma-1}]
            set cp1 [expr {$comma+1}]
            set c1 [string range $c 0 $cm1]
            set c2 [string range $c $cp1 end]
         
#  CHARACTER, LINE, BAR, SPIKE
            set GS(notsent,$c1) 1
            if {[lindex $c1 0] != "LEGEND"} {
        	  set cmd $c1
        	  set jmax $GS(lim4)
        	  set j $jmax
        	  while {$j >= 1} {
        	     if {$grSetData($c1,$j) != $grSetSave($type,$c1,$j)} {
        		for {set k 1} {$k <= $j} {incr k} {
        		   append cmd " $grSetData($c1,$k)"
        		}
        		set j 0
        		if {$type == "curr"} {
        		   sendDP $cmd
        		   set sendGrf 1
        		   for {set j 1} {$j <= $jmax} {incr j 1} {
        		      set grSetSave(curr,$c1,$j) $grSetData($c1,$j)
        		   }
        		} else {
        		   lappend GS(state) $cmd
        		}
        	     }
        	     incr j -1
        	  }
         
#  LEGEND
            } else {
               regsub "LEGEND" $c1 "LEGEND $c2" c1
               set cmd "$c1 $grSetData($c)"
               set sendGrf [graphSetApplySet $cmd $c $type]
            }       
         }         
      }
   }
   
   if {$sendGrf && $type == "curr"} {sendGraphics ALL}
   set GS(noecho) 0

#  display state to save
   if {$GS(state) != "" && $type == "save"} {
      set GS(state) "{. } {. graph settings state} $GS(state)"
      pageWin .list $GS(state)
   }
}

###############################################################################

proc graphSetApplySet {cmd c type} {
   global GS grSetData grSetSave
   
   set sendGrf 0
   if {$type == "curr"} {
      set grSetSave(curr,$c) $grSetData($c)
      sendDP $cmd
      set sendGrf 1
   } else {
      lappend GS(state) $cmd
   }
   return $sendGrf            
}

###############################################################################

proc graphSetSaveState {} {
   global grSetSave
      
   foreach i [array names grSetSave] {
      regsub "curr" $i "save" j
      if {![info exists grSetSave($j)]} {set grSetSave($j) $grSetSave($i)}
   }
}
