proc dataSheet {{w .data}} {

#  variable definitions (not complete)
#  canvas related
#     dsNcol - number of columns of entries
#     dsNrow(c) - number of rows of entries per column
#     dsNind - number of index numbers along left and right sides
#
#  array related
#     dsNnam - number of variable names
#     dsNam(c) - variable names for each column
#     dsNval(c) - number of values per column
#     dsArr(r,c) - values
#     dsSav(r,c) - saved values

   global DS dsNrow dsNcol dsNval dsNam dsArr dsSav dsNind dsNnam dsLab
   global xdp
   
   if {$xdp(DEBUG)} {puts "ENTER dataSheet"}
   if {[winfo exists $w]} {destroy $w}

#  initialize
   cursorConfig watch
   set nrow 100
   set dsNind $nrow
   set dsNcol 5
   for {set j 1} {$j <= $dsNcol} {incr j} {
      set dsNam($j) ""
      set dsLab($j) "              "
      set dsNval($j) 0
      set dsNrow($j) $nrow
      for {set i 1} {$i <= $dsNrow($j)} {incr i} {
         set dsArr($i,$j) ""
         set dsSav($i,$j) ""
      }
   }
   set prtlst ""
   set DS(carr) 14
   set DS(cnam) 10

#  open worksheet window
   toplevel $w
   wm title $w "Spreadsheet"
   wm iconname $w "Spreadsheet"
   set xx [expr {[winfo x .graph] + $xdp(OFFSET,x)}]
   set yy [expr {[winfo y .graph] + [winfo height .graph] + $xdp(OFFSET,y)}]
   wm geometry $w $xdp(WID,.graph)\x$xdp(HGT,$xdp(WINBOT))+$xx+$yy
   wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]
   
#  create a canvas
   frame $w.f1 -bd 1 -relief groove
   set DS(canvas) [canvas $w.f1.c -highlightthickness 0 \
      -yscrollcommand [list $w.f1.yscroll set] \
      -xscrollcommand [list $w.f1.xscroll set]] 
   scrollbar $w.f1.xscroll -relief sunken -command [list $DS(canvas) xview] -orient horiz -takefocus 0
   scrollbar $w.f1.yscroll -relief sunken -command [list $DS(canvas) yview] -takefocus 0

   grid $DS(canvas) $w.f1.yscroll -sticky news
   grid $w.f1.xscroll -row 1 -column 0 -sticky ew
   grid rowconfigure $w.f1 0 -weight 1
   grid columnconfigure $w.f1 0 -weight 1
    
#  create frame in the canvas
   set f [frame $DS(canvas).f -bg $xdp(COLOR,1)]
   $DS(canvas) create window 0 0 -anchor nw -window $f
   set DS(frame) $f

#  put each variable in dsArr in its own entry so it can be edited
   for {set j 1} {$j <= $dsNcol} {incr j} {
      set col1 [expr {$j*2-1}]
      set col2 [expr {$j*2}]

#  put variable name at top
      dataSheetColHead $f $j $col1 $col2

#  variable entry
      for {set i 1} {$i <= $nrow} {incr i} {dataSheetVarEntry $f $i $j $col1}
   }

#  variable index on left
   for {set i 1} {$i <= $dsNind} {incr i} {dataSheetIndNum $f $i}

#  decimals button
   frame $w.f2 -bd 1 -relief groove
   set DS(ndec) ""
   menubutton $w.f2.decimals -text "Set Decimals" -menu $w.f2.decimals.menu 
   set m [menu $w.f2.decimals.menu -tearoff 0]
   set decval {-1 0 1 2 3 4 5 6 7 8 9 10}
   foreach item $decval { 
      $m add radiobutton -label $item -variable DS(ndec) \
         -value $item -command {
	 set cmd "SET WRITE DECIMALS $DS(ndec)"
	 sendDP $cmd
	 sendVariables
      }
   }

#  apply button
   set blist ""
   button $w.f2.apply -text "Apply" -command {
      set DS(noecho) 1
      set sendVar 0
      for {set j 1} {$j <= $dsNcol} {incr j} {
         if {$dsNam($j) != ""} {
            set pe [string first "=" $dsNam($j)]
            if {$pe == -1} {
               for {set i 1} {$i <= $dsNrow($j)} {incr i} {
        	  if {$dsArr($i,$j) != $dsSav($i,$j)} {
                     set dsSav($i,$j) $dsArr($i,$j)
                     if {$dsArr($i,$j) != ""} {
        		set cmd "LET $dsNam($j) = DATA $dsArr($i,$j) FOR I = $i 1 $i"
        		if {$i > $dsNval($j)} {set dsNval($j) $i}
        	     } else {
        		set cmd "DELETE $dsNam($j)($i)"
        	     }
        	     sendDP $cmd
        	     set sendVar 1
        	  }
	       }

#  an expression with an = sign, make it a LET command
	    } else {
	       if {$pe >= 5} {
	          foreach l {let LET} {set dsNam($j) [string trimleft $dsNam($j) $l]}
                  set dsNam($j) [string trim $dsNam($j)]
	          set pe [string first "=" $dsNam($j)]
	       }
	       set cmd "LET $dsNam($j)"
	       set dsNam($j) [string trim [string range $dsNam($j) 0 [expr {$pe-1}]]]
	       sendDP $cmd
	       set sendVar 1
	    }
	 }
      }
      if {$sendVar} {sendVariables}
      set DS(noecho) 0
   }

   button $w.f2.replot -text "Replot" -command {sendDP "REPLOT"}
   button $w.f2.refresh -text "Refresh" -command {dataSheetClear; sendVariables}
   if {![info exists xdp(DSNOUPDATE)]} {set xdp(DSNOUPDATE) 0}
   checkbutton $w.f2.noupdate -text "No Update" -variable xdp(DSNOUPDATE)
   lappend blist $w.f2.apply $w.f2.replot $w.f2.refresh $w.f2.noupdate
   foreach b $blist {$b configure -padx $xdp(PADX) -pady $xdp(PADY)}

   pack $w.f2.decimals -side left
   pack $w.f2.apply $w.f2.replot $w.f2.refresh -side left
   pack $w.f2.noupdate -side right
   pack $w.f2 -fill x
   pack $w.f1 -expand 1 -fill both

   dataSheetBind
   update idletasks
   $DS(canvas) configure -scrollregion [grid bbox $DS(frame)]
   cursorConfig arrow
   focus $f.vn1
}

################################################################################

proc dataSheetColHead {f j col1 col2} {
   global DS dsNam xdp
   
   set w [entry $f.vn$j -textvariable dsNam($j) -width $DS(cnam) \
      -highlightbackground $xdp(COLOR,1)]
   grid $w -row 1 -column $col2 -sticky e
   if {![info exists DS(font)]} {set DS(font) [$DS(canvas).f.vn1 cget -font]}

   set w [label $f.vnt$j -text $j\: -font $DS(font) -bg $xdp(COLOR,1)]
   grid $w -row 1 -column $col1 -sticky e

   set w [entry $f.ve$j -textvariable dsLab($j) -relief groove -bg $xdp(COLOR,1) \
      -justify left -width $DS(carr) -font $DS(font) \
      -highlightthickness 0 -takefocus 0 -state disabled]
   grid $w -row 0 -column $col1 -columnspan 2
}

proc dataSheetVarEntry {f i j col1} {
   global DS dsArr xdp
   
   set w [entry $f.r$i\c$j -textvariable dsArr($i,$j) -width $DS(carr) \
      -justify right -highlightbackground $xdp(COLOR,1)]
   grid $w -row [expr {$i+1}] -column $col1 -columnspan 2
}

proc dataSheetIndNum {f i} {
   global DS xdp
   
   set w [label $f.vil$i -text $i -font $DS(font) -bg $xdp(COLOR,1)]
   grid $w -row [expr {$i+1}] -column 0 -sticky e
}

################################################################################

proc dataSheetUpdate {dsData} {

#  variable definitions
#  canvas related
#     dsNcol - number of columns of entries
#     dsNrow(c) - number of rows of entries per column
#     dsNind - number of index numbers along left and right sides
#
#  array related
#     dsNnam - number of variable names
#     dsNam(c) - variable names for each column
#     dsNval(c) - number of values per column
#     dsArr(r,c) - values
#     dsSav(r,c) - saved values

   global DS dsNrow dsNcol dsNval dsNam dsArr dsSav dsNind dsNnam dsExp dsLab
   global xdp

   if {$xdp(DEBUG)} {puts "ENTER dataSheetUpdate"}
   cursorConfig watch

#  initialize
   set nrow 0
   set f $DS(frame)
   set dsNnam 0
   for {set j 1} {$j <= $dsNcol} {incr j} {
      if {$dsNam($j) != ""} {incr dsNnam}
   }
   set namind ""
   set datval 0
   set varnam 0
   set prtlst ""
   set varstr "VARIABLES--"   

#  read through dsData
   foreach line $dsData {

#  store variable names in dsNam
      if {$datval == 0} {
         if {[string first $varstr $line] != -1} {
            set prtlst "$prtlst [string trimleft [string trim $line] $varstr]"
            set varnam 1
         } elseif {$varnam} {
            if {[string trim $line] != ""} {
               set prtlst "$prtlst [string trimleft [string trim $line] $varstr]"
        
#  found blank line to delimit variables names from the data
#  store list of variable names in dsNam and check for existing names
            } else {
               set datval 1
               set varnam 0
               set nprt [llength $prtlst]
               for {set j 0} {$j < $nprt} {incr j} {
        	  set j1 [expr {$j+1}]
        	  set nam [lindex $prtlst $j]
        	  set fndnam 0
        	  for {set k 1} {$k <= $dsNnam} {incr k} {
        	     if {$nam == [string toupper $dsNam($k)]} {set fndnam $k}
        	  }
        	  if {$fndnam == 0} {
        	     incr dsNnam
        	     set ind $dsNnam
        	  } else {
        	     set ind $fndnam
        	  }
        	  lappend namind $ind
        	  set dsNam($ind) $nam
        	  set nvalnew($ind) 0
               }
               set cline 1
               set ja 0
            }
         }
         
#  store variables in dsArr, handle values split across mulitple lines
      } elseif {$datval} {
         if {[string range $line 0 0] == ">" || $line == ""} {
            set datval 0
         } else {
            if {$cline == 1} {incr nrow}
            set jb [expr {$ja+[llength $line]}]
            set k 0
            for {set j $ja} {$j < $jb} {incr j} {
               set j1 [lindex $namind $j]
               if {$j1 != ""} {
        	  set dsArr($nrow,$j1) [lindex $line $k]
        	  set dsSav($nrow,$j1) $dsArr($nrow,$j1)
        	  incr nvalnew($j1)
        	  incr k
               }
            }
            incr cline
            if {$jb != $nprt} {
               set ja $jb
            } else {
               set ja 0
               set cline 1
            }
	 }
      }
   }
   
#  check if the canvas needs to be updated, loop through all columns
   if {[info exists nprt]} {
      for {set j1 1} {$j1 <= $nprt} {incr j1} {
	 set j [lindex $namind [expr {$j1-1}]]
	 set col1 [expr {$j*2-1}]
	 set col2 [expr {$j*2}]

#  check to create a new row
	 if {![info exists dsNrow($j)]} {
            incr dsNcol
            set dsNrow($j) 0
            dataSheetColHead $f $j $col1 $col2
	 }

	 if {$nvalnew($j) > $dsNrow($j)} {

#  add values to each row
            for {set i [expr {$dsNrow($j)+1}]} {$i <= $nvalnew($j)} {incr i} {
               dataSheetVarEntry $f $i $j $col1
            }

#  add new index values on left if necessary
            for {set i [expr {$dsNind+1}]} {$i <= $nvalnew($j)} {incr i} {
               dataSheetIndNum $f $i
            }

            if {$nvalnew($j) > $dsNind} {set dsNind $nvalnew($j)}
            set dsNrow($j) $nvalnew($j)
            set dsNval($j) $nvalnew($j)
	 }
      }
   } else {
      dataSheetClear
   }
   
#  set labels for each column based on LET expressions in dsExp
   for {set j 1} {$j <= $dsNcol} {incr j} {
      set dsLab($j) ""
      foreach i [array names dsExp] {
         if {$i == $dsNam($j)} {
            set dsLab($j) $dsExp($i)
            $f.ve$j configure -justify left
         }
      }
   }
      
   dataSheetBind
   update idletasks
   $DS(canvas) configure -scrollregion [grid bbox $DS(frame)]
   cursorConfig arrow
}

################################################################################

proc dataSheetBind {} {
   global DS dsNrow dsNcol dsNval dsNam dsArr dsSav dsNind dsNnam

   set f $DS(frame)

   for {set j 1} {$j <= $dsNcol} {incr j} {
      set entnam $f.vn$j
      bind $entnam <Return> "focus $f.r1c$j"

      for {set i 1} {$i <= $dsNrow($j)} {incr i} {
         set entnam $f.r$i\c$j
         if {$i < $dsNrow($j)} {
            bind $entnam <Return> "focus $f.r[expr {$i+1}]c$j"
         } else {
            if {$j < $dsNcol} {
               bind $entnam <Return> "focus $f.vn[expr {$j+1}]"
            } else {
               bind $entnam <Return> "focus $f.vn1"           
            }
         }
      }
   }
}

################################################################################

proc dataSheetClear {} {
   global dsNrow dsNcol dsNval dsNam dsArr dsSav dsNind dsNnam dsLab

   set dsNnam 0
   for {set j 1} {$j <= $dsNcol} {incr j} {
      set dsNam($j) ""
      set dsLab($j) ""
      set dsNval($j) 0
      for {set i 1} {$i <= $dsNrow($j)} {incr i} {
         set dsArr($i,$j) ""
         set dsSav($i,$j) ""
      }
   }
}

################################################################################

proc dataSheetStatus {dsData} {
   global dsNrow dsNcol dsNval dsNam dsArr dsSav dsNind dsNnam
   global varBrowse

   set nline 0
   set varBrowse ""
   foreach line $dsData {
      if {[lindex $line 0] == "VARIABLE"} {
         set j [lindex $line 1]
         set new_val [lindex $line 3]
         lappend varBrowse [lindex $line 6]
         for {set i [expr {$new_val + 1}]} {$i <= $dsNrow($j)} {incr i} {
            set dsArr($i,$j) ""
            set dsSav($i,$j) ""
         }
         set dsNval($j) $new_val
      }
   }
}
