#  window for graphics output of dataplot and window for pixmaps

proc graphPixmap {{w .graph}} {
   global xdp pixMap nPrint env canVas

   if {$xdp(DEBUG)} {puts "ENTER graphPixmap $w"}
   if {![info exists pixMap(new)]} {set pixMap(new) 1}
   if {![info exists pixMap(label)]} {set pixMap(label) ""}
   if {![info exists xdp(DEBUG_GRAPH)]} {set xdp(DEBUG_GRAPH) 0}
   set xdp(LATTY) 4
   set xdp(LATTX) 5

   catch {destroy $w}
   toplevel $w

#  window title and location
   if {$w == ".graph"} {set title "Graph"} else {set title "Pixmaps"}
   wm title    $w $title
   wm iconname $w $title
   set xt [expr {int([winfo screenwidth .]*0.4)}]
   set yt [expr {[winfo y .] + [winfo height .] + $xdp(OFFSET,y)}]
   wm geometry $w +$xt\+$yt
   wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]

#  file menu
   set blist ""
   frame $w.f1 -bd 1 -relief groove
   if {$w == ".graph"} {
      menubutton $w.f1.file -text File -menu $w.f1.file.m
      set m [menu $w.f1.file.m]
      set opt "(ERASE)"
      if {$xdp(GRAPHICS) == "soft"} {set opt ""}
      $m add command -label "Clear $opt" -command {
         if {$xdp(GRAPHICS) == "x11"} {sendDP "ERASE"}
         if {$xdp(GRAPHICS) == "soft"} {$canVas(name) delete screen; set canVas(grdata) ""}
      }

#  print
#      $m add command -label "Print (PP)" -acc "Ctrl-P" -command {sendDP "PP"}

#  save commands
      $m add command -label "Save postscript (dppl2f.dat) as ..." -command {
	 set fsave [tk_getSaveFile -title "Save File"]
	 if {$fsave != ""} {
            sendDP "DEVICE 3 CLOSE"
            file copy -force dppl2f.dat $fsave
	 }
      }
      if {$xdp(GRAPHICS) == "soft"} {
	 $m add command -label "Save postscript (canvas) as ..." -command {
	    set fsave [tk_getSaveFile -title "Save File"]
	    if {$fsave != ""} {
               .graph.f2.canvas postscript -file $fsave -rotate true \
        	  -pagewidth 11.0i -pageheight 8.5i
            }
	 }
      }

#  pixmaps and software graphics
      if {$xdp(GRAPHICS) == "x11"} {
	 $m add separator
	 $m add command -label "Save pixmap (SAVE GRAPH)" -acc "Ctrl-S" -command {
	    while {[file exists pixmap.$pixMap(new)]} {incr pixMap(new)}
	    sendDP "SAVE GRAPH pixmap.$pixMap(new)"
	 }
	 $m add command -label "View pixmap" -command {
	    wm deiconify .pixmap
	    focus .pixmap.f1.kill
	 }
	 $m add separator
	 $m add command -label "Software graphics" -command {
            set xdp(GRAPHICS) soft
            unset xdp(DEVICE)
            destroy .graph
            graphPixmap .graph
            sendDP "DEVICE 1 GENERAL CODED PACKED"
	 }
      }
      
#  debug graph
      if {$xdp(GRAPHICS) == "soft"} {
	 $m add separator
	 $m add checkbutton -label "Debug graph" -variable xdp(DEBUG_GRAPH) -command {
	    if {$xdp(PLATFORM) == "windows" && $xdp(DEBUG_GRAPH)} {console show}
	 }
      }

#  edit menu
      menubutton $w.f1.edit -text Edit -menu $w.f1.edit.m
      set m [menu $w.f1.edit.m -tearoff 0]

#  settings and fonts
      $m add command -label "Settings" -acc "Ctrl-G" -command {wm deiconify .gset1}
      graphFont $m      

#  draw menu for diagrammatic graphics
      menubutton $w.f1.draw -text Draw -menu $w.f1.draw.m
      set m [menu $w.f1.draw.m]
      set canVas(pos,x1) NULL
      set canVas(pos,x2) NULL
      set canVas(gcmd) NONE
      set gnpts 0
      set gcmds {NONE " " MOVE POINT CH " " DRAW ARROW BOX CIRCLE HEXAGON SEMI-CIRCLE \
         ELLIPSE OVAL CUBE LATTICE AMPLIFIER CAPACITOR GROUND INDUCTOR RESISTOR \
         AND NAND OR NOR " " TRIANGLE DIAMOND PYRAMID}
      set canVas(gcmds) {text drawdata}
      foreach gcmd $gcmds {
         if {$gcmd != " "} {
            if {$gcmd != "NONE" && $gcmd != "CH"} {
               lappend canVas(gcmds) [string tolower $gcmd]
            }
            set canVas($gcmd,pts) $gnpts
            $m add radiobutton -label $gcmd -variable canVas(gcmd) -value $gcmd -command {
	       bind $canVas(name) <Shift-Button-1> {
	          if {$canVas(gcmd) != "MOVE" && $canVas(gcmd) != "NONE" && \
	             $canVas(gcmd) != "CH"} {
	             set canVas(multi) 2
	          }
		  set tmp [graphPosition %x %y]
		  set xc [lindex $tmp 0]
		  set yc [lindex $tmp 1]
		  if {$canVas(gcmd) != "NONE"} {
		     graphGraphics $canVas(gcmd) $xc $yc %x %y $canVas($canVas(gcmd),pts)
		  }
	       }
            }
         } else {
            $m add separator
            incr gnpts
         }
      }

      pack $w.f1.file $w.f1.edit $w.f1.draw -side left

#  size
      if {$xdp(GRAPHICS) == "soft"} {
	 menubutton $w.f1.size -text Size -menu $w.f1.size.m
	 set m [menu $w.f1.size.m -tearoff 0]
	 $m add command -label "Zoom Up" -acc "Ctrl-U" -command {graphZoom 1.25}
	 $m add command -label "Zoom Down" -acc "Ctrl-D" -command {graphZoom 0.8}
	 $m add separator
	 $m add command -label "Square" -command {
	    set nheight $canVas(height)
	    set nwidth $nheight
	    graphResize $nheight $nwidth
	 }
	 $m add command -label "Default" -command {
	    set nheight $canVas(cheight,$xdp(GRAPHRES))
	    graphResize $nheight
	 }
	 pack $w.f1.size -side left
      }

#  help
      menubutton $w.f1.help -text "Help" -menu $w.f1.help.menu
      pack $w.f1.help -side left
      pageMenuHelp $w.f1.help.menu [list "Graphics Commands" \
         "Plot Control Commands" "Diagrammatic Graphics Commands"]

#  replot and refresh
      button $w.f1.replot -text "Replot" -command {sendDP "REPLOT"}
      button $w.f1.refresh -text "Refresh" -command {
         if {$xdp(GRAPHICS) == "x11"} {sendDP "."} else {graphPlot .graph $canVas(grdata) 0}
      }
      button $w.f1.print -text "Print" -command {sendDP "PP"}
      lappend blist $w.f1.replot $w.f1.refresh $w.f1.print
      pack $w.f1.replot $w.f1.refresh $w.f1.print -side left
      
#  xy label
      label $w.f1.pos -textvariable canVas(poslab) -relief groove -bg $xdp(COLOR,2) \
         -padx 4 -pady 2
      pack $w.f1.pos -side right
     
#  back and foward buttons, label, kill
   } elseif {$w == ".pixmap"} {
      button $w.f1.back -text "<<" -command "pixmapBack"
      button $w.f1.forward -text ">>" -command "pixmapForward"
      button $w.f1.print -text "Print" -command {
	 if {![info exists nPrint]} {set nPrint 0}
	 incr nPrint
	 set fprint [file join [file native /tmp] dp_$nPrint[pid]]
	 .pixmap.f2.canvas postscript -file $fprint -rotate true
	 exec $xdp(DP_PRINT) $fprint
      } -state $xdp(PRINT_STATE)
      label $w.f1.label -textvariable pixMap(label)
      button $w.f1.kill -text "X" -command "wm withdraw $w" -padx $xdp(PADX) -pady $xdp(PADY)
      lappend blist $w.f1.back $w.f1.forward $w.f1.print $w.f1.kill

      pack $w.f1.back $w.f1.forward $w.f1.print -side left
      pack $w.f1.label -side left -padx 4m
      pack $w.f1.kill -side right
      focus .pixmap.f1.kill
   }
   pack $w.f1 -side top -fill x
   foreach b $blist {$b configure -padx $xdp(PADX) -pady $xdp(PADY)}

#  canvas (default X11 device size is 550x425, 425/550 = 0.772727 = 8.5/11)
   frame $w.f2
   set canVas(cheight,hi) 435
   set canVas(cheight,lo) 315
   set canVas(height) $canVas(cheight,$xdp(GRAPHRES))
   set canVas(width) [expr {int($canVas(height) / 0.772727)}]
   set c [canvas $w.f2.canvas -bg $xdp(COLOR,2) \
      -width $canVas(width) -height $canVas(height)]
   pack $c -expand 1 -fill both
   pack $w.f2 -side top -expand 1 -fill both
   if {$w == ".graph"} {
      set canVas(name) $c
      set canVas(grdata) ""
   }
   frame $c.f
   $c create window 0 0 -anchor nw -window $c.f
   
#  button binding 
   if {$w == ".graph"} {
      bind .graph <Control-p> {sendDP "PP"}
      bind $canVas(name) <Motion> {graphPosition %x %y}
   
#  save pixmap
      if {$xdp(GRAPHICS) == "x11"} {
	 bind .graph <Control-s> {
	    while {[file exists pixmap.$pixMap(new)]} {incr pixMap(new)}
	    sendDP "SAVE GRAPH pixmap.$pixMap(new)"
	 }

#  zoom window
      } elseif {$xdp(GRAPHICS) == "soft"} {
         bind .graph <Control-u> {graphZoom 1.25}
         bind .graph <Control-d> {graphZoom 0.8}
         set canVas(cursor) 0

#  rubberbanding for diagrammatic graphics
	 bind .graph <Shift-Motion> {
	    if {$canVas(cursor)} {
	       $canVas(name) delete rubberband
	       set item line
	       if {$canVas(gcmd) == "BOX" || $canVas(gcmd) == "LATTICE"} {set item rectangle}
	       $canVas(name) create $item $canVas(cursor,x) $canVas(cursor,y) %x %y \
	          -tags rubberband
	    }
   	 }
      }

#  quit pixmap
   } elseif {$w == ".pixmap"} {
      bind .pixmap <Control-q> {wm withdraw .pixmap}
   }

#  resize the window
   update idletasks
   set xdp(WID,$w) [winfo width $w]
   set xdp(HGT,$w) [winfo height $w]
   set xx [expr {[winfo x .] + [winfo width .] - $xdp(WID,$w) + $xdp(OFFSET,x)}]
   set yy [expr {[winfo y .] + [winfo height .] + $xdp(OFFSET,y)}]
   wm geometry $w $xdp(WID,$w)\x$xdp(HGT,$w)+$xx+$yy
   if {$w == ".pixmap"} {wm withdraw $w}
   update idletasks

#  bind to resize canvas in graph window
   if {$w == ".graph"} {
      set xdp(DWID,.graph) [expr {$xdp(WID,.graph) - $canVas(width)}]
      set xdp(DHGT,.graph) [expr {$xdp(HGT,.graph) - $canVas(height)}]
      if {$xdp(GRAPHICS) == "soft"} {
	 bind .graph <Configure> {
	    if {"%W" == ".graph"} {
	       if {$xdp(DEBUG_GRAPH)} {puts " Canvas configure event %W %w %h"}
	       set nwidth [expr {%w - $xdp(DWID,.graph)}]
	       set nheight [expr {%h - $xdp(DHGT,.graph)}]
	       graphResize $nheight $nwidth 0
	    }
	 }
      } else {
	 wm minsize $w $xdp(WID,$w) $xdp(HGT,$w)
	 wm maxsize $w $xdp(WID,$w) $xdp(HGT,$w)
      }
   }
} 

###############################################################################

proc graphResize {nheight {nwidth -1} {regeom 1}} {
   global xdp canVas

   if {$nwidth == -1} {set nwidth [expr {int($nheight / 0.772727)}]}
   if {($nwidth != $canVas(width) || $nheight != $canVas(height)) && \
      $nwidth > 0 && $nheight > 0} {
      if {$xdp(DEBUG_GRAPH)} {puts " New canvas size: $canVas(width)\x$canVas(height)"}
      set canVas(width) $nwidth
      set canVas(height) $nheight
      if {$xdp(DEBUG_GRAPH)} {puts " Old canvas size: $canVas(width)\x$canVas(height)"}
      .graph.f2.canvas configure -width $canVas(width) -height $canVas(height)
      if {$regeom} {
	 set xx [expr {$canVas(width) + $xdp(DWID,.graph)}]
	 set yy [expr {$canVas(height) + $xdp(DHGT,.graph)}]
	 wm geometry .graph $xx\x$yy
	 raise .graph
      }
      $canVas(name) delete screen
      graphPlot .graph $canVas(grdata) 0
   }
}

###############################################################################

proc graphZoom {factor} {
   global canVas

   set nheight [expr {int($canVas(height) * $factor)}]
   set nwidth [expr {int($canVas(width) * $factor)}]
   graphResize $nheight $nwidth
}

###############################################################################

proc graphPosition {x y} {
   global canVas

   set xc [expr {double(int(1000*double($x)/double($canVas(width))))/10.}] 
   set yc [expr {double(int(1000*(1.-(double($y)/double($canVas(height))))))/10.}]
   set canVas(poslab) "$xc  $yc"
   return [list $xc $yc]
}

###############################################################################

#  send commands for diagrammatic graphics

proc graphGraphics {cmd x y mx my {nval 2}} {
   global canVas xdp

   set isend 0
   set canVas(cursor) 0

#  store 1st coordinates, send command for 1 set of coords
   if {$canVas(pos,x1) == "NULL"} {
      set canVas(pos,x1) $x; set canVas(pos,y1) $y
      if {$nval == 1} {
         sendDP "$cmd $canVas(pos,x1) $canVas(pos,y1)"
         set isend 1
      } else {
         set canVas(cursor) 1
      }

#  store 2nd coordinates
   } elseif {$nval == 3 && $canVas(pos,x2) == "NULL"} {  
      set canVas(pos,x2) $x; set canVas(pos,y2) $y
      set canVas(cursor) 1

#  send command for 2 sets of coords
   } elseif {$nval == 2} {
      catch {$canVas(name) delete cursor; $canVas(name) delete rubberband}
      if {$cmd != "LATTICE"} {
         sendDP "$cmd $canVas(pos,x1) $canVas(pos,y1) $x $y"
      } else {
         set dx [expr {abs($canVas(pos,x1)-$x)/double($xdp(LATTX))}]
         set dy [expr {abs($canVas(pos,y1)-$y)/double($xdp(LATTY))}]
         sendDP "$cmd $canVas(pos,x1) $canVas(pos,y1) $dx $dy $x $y"
      }
      set isend 1

#  send command for 3 sets of coords
   } elseif {$nval == 3} {
      catch {$canVas(name) delete cursor; $canVas(name) delete rubberband}
      sendDP "$cmd $canVas(pos,x1) $canVas(pos,y1) $canVas(pos,x2) $canVas(pos,y2) \
         $x $y"
      set isend 1
   }

   if {$canVas(cursor) && $xdp(GRAPHICS) == "soft"} {
      catch {$canVas(name) delete cursor}
      $canVas(name) create line $mx $my [expr {$mx+5}] $my -tags cursor
      $canVas(name) create line $mx $my [expr {$mx-5}] $my -tags cursor
      $canVas(name) create line $mx $my $mx [expr {$my+5}] -tags cursor
      $canVas(name) create line $mx $my $mx [expr {$my-5}] -tags cursor
      $canVas(name) lower cursor
      set canVas(cursor,x) $mx
      set canVas(cursor,y) $my
   }
   
   if {$isend} {
      set canVas(pos,x1) NULL
      set canVas(pos,x2) NULL
   }
}

###############################################################################

proc pixmapBack {} {
   global pixMap
   if {![info exists pixMap(num)]} {set pixMap(num) 2}

   incr pixMap(num) -1
   set pixMap(file) pixmap.$pixMap(num)
   if {[file exists $pixMap(file)]} {
      wm deiconify .pixmap
      focus .pixmap.f1.kill
      .pixmap.f2.canvas create bitmap 0 0 -bitmap @$pixMap(file) -anchor nw \
	 -background white -foreground black
      set pixMap(label) $pixMap(file)
   } else {
      incr pixMap(num) 1
   }
}

###############################################################################

proc pixmapForward {} {
   global pixMap
   if {![info exists pixMap(num)]} {set pixMap(num) 0}

   incr pixMap(num) 1
   set pixMap(file) pixmap.$pixMap(num)
   if {[file exists $pixMap(file)]} {
      wm deiconify .pixmap
      focus .pixmap.f1.kill
      .pixmap.f2.canvas create bitmap 0 0 -bitmap @$pixMap(file) -anchor nw \
	 -background white -foreground black
      set pixMap(label) $pixMap(file)
   } else {
      incr pixMap(num) -1
   }
}

###############################################################################

proc graphFont {m} {
   global xdp grSetData

#   $m add cascade -label Fonts -menu $m.f
#   set m [menu $m.f -tearoff 0]

   $m add separator
   $m add cascade -label "Graph Font" -menu $m.x
   set mx [menu $m.x]

   set xf ""
   foreach fam {helvetica times courier} {
      foreach size {bold medium} {
         foreach point {10 12 14 18} {
            set font "$fam $size $point"; lappend xf $font
         }
      }
   }

   set cmd "SET X11 FONT"
   foreach i $xf {
      $mx add radiobutton -label $i -variable grSetData($cmd) -value $i -command {
	 set cmd "SET X11 FONT"
	 if {[llength $grSetData($cmd)] > 1} {
	    set grSetData($cmd) "-*-[lindex $grSetData($cmd) 0]-[lindex $grSetData($cmd) 1]-r-normal--[lindex $grSetData($cmd) 2]-*-*-*-*-*-*-*"
	 }
	 sendDP "$cmd $grSetData($cmd)"
      }
   }
 
   $m add cascade -label "Print Font" -menu $m.p
   set mp [menu $m.p]
   set psf {"HELVETICA BOLD" "HELVETICA" "HELVETICA NARROW" "HELVETICA NARROW BOLD" \
      "TIMES ROMAN" "TIMES BOLD" "COURIER" "COURIER BOLD"}
   set cmd "SET POSTSCRIPT FONT"
   foreach i $psf {
      $mp add radiobutton -label $i -variable grSetData($cmd) -value $i -command {
	 set cmd "SET POSTSCRIPT FONT"
	 sendDP "$cmd $grSetData($cmd)"
      }
   }
}

###############################################################################

#  plot the graph is software graphics mode

proc graphPlot {w grdata {append 1}} {
   global xdp grSetData canVas
   
   if {$xdp(DEBUG)} {puts "ENTER graphPlot"}
   cursorConfig watch

   set c $canVas(name)
   set s "SET X11 FONT"
   set gfont $grSetData($s)
   if {$gfont == ""} {
      if {$xdp(PLATFORM) == "unix"} {
	 set gfont -*-helvetica-bold-r-normal--12-*-*-*-*-*-*-*
      } else {
	 set gfont -*-arial-bold-r-normal--14-*-*-*-*-*-*-*
      }
   }
   set dy [lindex [font metrics $gfont] 1]

   if {$xdp(DEVICE) == "code"} {
      set canVas(scale) 100.
   } elseif {$xdp(DEVICE) == "pack"} {
      set canVas(scale) 1000.
   }
   set canVas(canvas,x) [expr {$canVas(width)/$canVas(scale)}]
   set canVas(canvas,y) [expr {$canVas(height)/$canVas(scale)}]
  
   set colors(BLAC) black; set colors(WHIT) white
   set colors(RED)  red;   set colors(GREE) green;   set colors(BLUE) blue
   set colors(CYAN) cyan;  set colors(MAGE) magenta; set colors(YELL) yellow
   
   set anchors(CETO) n;  set anchors(CEBO) s
   set anchors(CECE) c;  set anchors(CENT) c
   set anchors(RITO) ne; set anchors(RIBO) ne
   set anchors(RICE) e;  set anchors(RIGH) e
   set anchors(LETO) nw; set anchors(LEBO) sw
   set anchors(LECE) w;  set anchors(LEFT) w
  
   set nfill 0
   set fcoords {}
   set coords {}
   set ndraw 0
   set nline 0
   set ntext 0
   set nerase 0
   set dash ""
   set lastdash ""
   
   raise .graph

   foreach line $grdata {
      set l1 [lindex $line 0]

#  draw
      if {$l1 == "DRTO" || $l1 == "D"} {
	 incr ndraw
	 lappend coords [graphPlotScaleCoord [lindex $line 1] x]
	 lappend coords [graphPlotScaleCoord [lindex $line 2] y]

#  move
      } elseif {$l1 == "MOTO" || $l1 == "M"} {
	 if {[llength $coords] > 3} {
	    if {$dash != ""} {
	      eval $c create line [join $coords " "] -tags screen \
		 -fill $colors($cindex) -width $thickness -dash $dash
	    } else {
	      eval $c create line [join $coords " "] -tags screen \
		 -fill $colors($cindex) -width $thickness
	    }
	    incr nline
	 }
	 set coords {}
	 set x [graphPlotScaleCoord [lindex $line 1] x]
	 set y [graphPlotScaleCoord [lindex $line 2] y]
	 lappend coords $x
	 lappend coords $y

#  text, vertical is a special case
      } elseif {$l1 == "WRTE"} {
         incr ntext
	 set text [string range $line 5 end]
	 if {$dir == "HORI"} {
	    $c create text $x $y -text $text -tags screen \
	       -anchor $anchors($textalign) -font $gfont -fill $colors($cindex)
	 } elseif {$dir == "VERT"} {
	    set len [string length $text]
	    set y [expr {$y - ($len/2)*$dy}]
	    for {set i 0} {$i < $len} {incr i} {
	       $c create text $x [expr {$y+$i*$dy}] -text [string range $text $i $i] -tags screen \
	       -anchor $anchors($textalign) -font $gfont -fill $colors($cindex) 
	    }
	 }

#  text justification
      } elseif {$l1 == "SEJU"} {
	 set textalign [lindex $line 1]

#  text direction
      } elseif {$l1 == "SEDI"} {
	 set dir [lindex $line 1]

#  line thickness
      } elseif {$l1 == "SETH"} {
	 set thickness [expr {int([lindex $line 1]/0.1)}]

#  line type
      } elseif {$l1 == "SEPA" && [info tclversion] >= 8.3} {
        if {[lindex $line 1] == "LINE"} {
	  set l2 [lindex $line 2]
	  if {$l2 == "DOTT"} {
	    set dash [list .]
	  } elseif {$l2 == "DASH"} {
	    set dash [list -]
	  } elseif {$l2 == "DA1" || $l2 == "DA2" || $l2 == "DA3" || $l2 == "DA4"} {
	    set dash [list -.]
	  } elseif {$l2 == "SOLI"} {
	    set dash ""
	  } else {
	    set dash ""
	  }
	  if {$dash != $lastdash} {
	    if {[llength $coords] > 3} {
	       if {$lastdash != ""} {
		 eval $c create line [join $coords " "] -tags screen \
		    -fill $colors($cindex) -width $thickness -dash $lastdash
	       } else {
		 eval $c create line [join $coords " "] -tags screen \
		    -fill $colors($cindex) -width $thickness
	       }
	       set coords {}
	       incr nline
	    }
	  }
	  set lastdash $dash
	}

#  colors
      } elseif {$l1 == "SECO"} {
	 set type [lindex $line 1]
	 if {$type != "BACK"} {
	    set cindex [lindex $line 2]
	 } else {
	    $c configure -bg $colors([lindex $line 2])
	 }

#  clear screen
      } elseif {$l1 == "ERSC"} {
	 incr nerase
	 if {$nerase > 1} {after 1000; update idletasks}
	 $c delete screen
	 if {$xdp(DEBUG_GRAPH)} {puts " ERASE"}	

#  fill on
      } elseif {$l1 == "FIRE"} {
	 incr nfill

#  fill off
      } elseif {$l1 == "ENFI"} {
	 set len [llength $fcoords]
	 eval $c create poly [join [lrange $fcoords 0 [expr {$len-3}]] " "] \
	    -fill $colors($cindex) -tags screen -width $thickness
	 set nfill 0
	 set fcoords {}

#  fill
      } elseif {$nfill > 0} {
	 set x [graphPlotScaleCoord [lindex $line 0] x]
	 set y [graphPlotScaleCoord [lindex $line 1] y]
	 lappend fcoords $x $y
	 incr nfill

      } else {
        if {$xdp(DEBUG_GRAPH)} {puts " $l1"}
      }
      
#  draw
      if {$l1 != "DRTO" && $l1 != "MVTO" && $l1 != "M" && $l1 != "D"} {
	 if {[llength $coords] > 3} {
	    if {$dash != ""} {
	      eval $c create line [join $coords " "] -tags screen \
		 -fill $colors($cindex) -width $thickness -dash $dash
	    } else {
	      eval $c create line [join $coords " "] -tags screen \
		 -fill $colors($cindex) -width $thickness
	    }
	    set coords {}
	    incr nline
	 }
      }     
   }

   update idletasks
   cursorConfig arrow
   if {$xdp(DEBUG_GRAPH)} {puts " $ndraw draws, $nline lines, $ntext text"}

#  save plot data
   if {$canVas(multi) != 2 || $append == 0} {
      set canVas(grdata) $grdata
      if {$canVas(multi) == 1} {set canVas(multi) 2}
      if {$xdp(DEBUG_GRAPH)} {puts " $xdp(cmd1) SET [llength $canVas(grdata)]\n"}
   } else {
      append canVas(grdata) $grdata
      foreach c $canVas(gcmds) {if {$xdp(cmd1) == $c} {set canVas(multi) 0}}
      if {$xdp(DEBUG_GRAPH)} {puts " $xdp(cmd1) APPEND [llength $canVas(grdata)]\n"}
   }
}

###############################################################################

proc graphPlotScaleCoord {coord type} {
   global canVas
   
   if {$type == "y"} {set coord [expr {$canVas(scale) - $coord}]}
   set coord [format "%7.2f" [expr {$coord * $canVas(canvas,$type)}]]
   
   return $coord
}
