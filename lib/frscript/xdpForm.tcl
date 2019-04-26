proc formDisplay {{w .form}} {

   global formLines formFile formFocus browseFocus PD dpc ndpc
   global fradVr fradVar fradText
   global xdp vbNam
   global xm mixm xmFill xmFpos
   global ntent entNam entbr
   
   if {$xdp(DEBUG_FORM)} {puts "ENTER Form"}
   catch {destroy .varbrowse}

#  initialize
   set ffont $xdp(FONT,2)
   set wcmd 1
   set dcmd 0
   set notopen 1
   set notpack 1
   set lfr 0
   set nfr 0
   set mixm 0
   set ntent 0
   set ndpc 0
   set nent(0) 0
   set form_type 0
   set ndash 0
   set sfr ""
   set mce4 0
   set labok 1
   set npadl 0
   for {set i 1} {$i < 100} {incr i} {
      set xm($i) ""
      set fradVar($i) ""
   }
   regsub {[file native "$xdp(DP_FEND)"]} $formFile "" pff
   set fpady 4
   if {$xdp(PLATFORM) == "windows"} {set fpady 0m}
   
#  strings to check for on forms to fill in previous values
   set fillstr {Subset {Vertical Response Variable} {Horizontal Independent} \
      {List Factors}}
   foreach i $fillstr {set xmFill($i) ""; set xmFpos($i) ""}

   cursorConfig watch

#  read items in formLines
   set i 0
   foreach item $formLines {
      incr i
      if {$xdp(DEBUG_FORM)} {puts "   <$item>"}

#  skip 1st 3 items and blank lines
      if {$i > 3 && ($item != " " || $form_type == 2)} {

#  a line with ----- is a delimiter
#  form_type =1 - menu in 2 parts (labels, radiobuttons, entries) (dp commands)
#  form_type =2 - menu in 3 parts (labels) (entries) (dp commands)
         if {[string range $item 0 4] == "-----"} {
            incr ndash
            if {$form_type == 1 || ($form_type == 2 && $ndash == 2)} {
               set wcmd 0
               set dcmd 1
            }
            if {$form_type == 2 && $ndash == 1} {set nfr -1}
         }

#------------------------------------------------------------------------------
#  labels, entry boxes, etc.
         if {$wcmd} {

#  check for . (but rarely a :)
            set pdot [string first . $item]
            set cdot [string first : $item]
            if {$pdot != -1 && $cdot != -1 && $cdot < $pdot} {set pdot $cdot}

            if {$pdot != -1 && $form_type != 2} {
               set nfr [string trim [string range $item 0 [expr {$pdot-1}]]]
               set nfr [string trimleft $nfr "0"]
               set nfr [expr {int($nfr)}]
               set lsfr $sfr
               set sfr [string trimright \
                  [string range $item [expr {$pdot+2}] end]]
               set form_type 1
            } elseif {$pdot == -1 && $form_type == 1} {
               incr nfr
               set lsfr $sfr
               set sfr [string trimright $item]           
            } else {
               incr nfr
               set lsfr $sfr
               set sfr [string trimright $item]
               set form_type 2
               if {$nfr == 1 && $ndash == 1} {errorMsg "WARNING: Old style menu form: $pff"}
            }

#------------------------------------------------------------------------------
#  start a window
            if {$nfr && $notopen} {
               set notopen 0
	       catch {destroy $w}
	       toplevel $w
	       wm title    $w $sfr
	       wm iconname $w $sfr
	       set xywin .pd$PD(curr)
	       set xx [expr {[winfo x $xywin] + 50}]
	       set yy [expr {[winfo y $xywin] + 35}]
	       wm geometry $w +$xx+$yy
               wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]
               
#  create a canvas and a scroll bar in a frame
               frame $w.f1 -bd 1 -bg $xdp(COLOR,1)
               set c [canvas $w.f1.c -highlightthickness 0 \
		  -yscrollcommand [list $w.f1.yscroll set]]
               set s [scrollbar $w.f1.yscroll -relief sunken \
                  -command [list $c yview] -takefocus 0]

	       grid $c -row 0 -column 0 -sticky news
	       grid rowconfigure $w.f1 0 -weight 1
	       grid columnconfigure $w.f1 0 -weight 1

#  create a frame in the canvas
	       set f [frame $c.f -bg $xdp(COLOR,1)]
	       $c create window 0 0 -anchor nw -window $f
	    }

#  open frame
	    if {$nfr != $lfr} {
	       set llfr $lfr
	       set lfr $nfr
	       set nent($nfr) 0
	       if {![winfo exists $f.$nfr]} {
	          if {$nfr > 2} {
	             frame $f.$nfr
	          } else {
	             frame $f.$nfr -bg $xdp(COLOR,1)
	          }
	          set rpos($nfr) 0
	       }

#  pack entry windows from previous frame
	       set efr $llfr
	       if {$efr > 0} {
	          if {$nent($efr) != 0} {
	             pack $f.$efr.ent -side right -padx 2
                     bind $f.$efr.ent <Button-2> {formOK}
	          }
	       }
	    }

#------------------------------------------------------------------------------
#  @CE commands or entry section for form_type=2
            set sfrt [string trim $sfr " "]
            if {[string range $sfrt 0 0] == "@"} {set sfr $sfrt}
            set sfr1 [string range $sfr 0 0]
	    if {[string range $sfr 0 0] == "@" || \
	       ($form_type == 2 && $ndash == 1 && $sfr != "0" && $sfr1 != "-")} {

#  ce and lce store parts of @CE command, mce4=max(lce(4))
	       set ce [split $sfr " "]
	       for {set k 0} {$k < [llength $ce]} {incr k} {
	          if {[lindex $ce $k] == ""} {set ce [lreplace $ce $k $k]}
	       }
	       for {set k 0} {$k < 6} {incr k} {set lce($k) [lindex $ce $k]}
	       if {$lce(4) >= 90 && $lce(3) < 40} {set lce(4) 50}
	       if {$lce(4) > $mce4} {set mce4 $lce(4)}
	       if {$lce(5) == "*" && $lce(4) > $mce4} {set lce(4) $mce4} 

#  cewid is the widget width, ixm is the index of the variable for the widget
	       if {$form_type == 1} {
	          set ixm $lce(1)
	          set cewid [expr {$lce(4) - $lce(3)}]
	       } else {
	          set ixm $nfr
	          set cewid [expr {$lce(1) - $lce(0)}]
	          if {$cewid > 50} {set cewid [expr {$cewid - 40}]}
	       }
	       if {$ixm > $mixm} {set mixm $ixm}

#------------------------------------------------------------------------------
#  entry box
	       if {$lce(2) == 0 || $form_type == 2} {
	          incr nent($nfr)
	          if {$nent($nfr) == 1} {frame $f.$nfr.ent}
	          incr ntent
	          set entNam($ntent) $f.$nfr.ent.$nent($nfr)
                  if {$xdp(DEBUG_FORM)} {puts "    ENTRY $entNam($ntent) -width $cewid -variable xm($ixm)"} 
	          entry $entNam($ntent) -width $cewid -textvariable xm($ixm)

#  associate which type of browser might be needed for an entry
                  set litem [string tolower $item]
                  set p [string first "gui-" $litem]
                  set ind [string range $litem [expr {$p+4}] [expr {$p+7}]]
                  if {$p != -1} {set browse($ind) 1}
                  set entbr($entNam($ntent)) none
                  set nobind 1
                  foreach t $vbNam {
                     if {$browse($t) == 1} {
                        set entbr($entNam($ntent)) $t
                        bind $entNam($ntent) <FocusIn> {
        		    set formFocus [focus]
        		    formChoose $formFocus $entbr($formFocus)
                        }
                        set nobind 0
                     }
                  }
		  if {$nobind} {
		     bind $entNam($ntent) <FocusIn> {
		        catch {destroy .varbrowse}; set browseFocus a
		     }
		  }
                  

#  add a blank label to space the entry boxes
	          set labwid [expr {$lce(3)-$rpos($nfr)-3}]
                  if {$labwid > 0} {
	             incr npadl
	             set padNam $f.$nfr.pad$npadl
                     if {$xdp(DEBUG_FORM)} {puts "    BLANK LABEL $padNam -width $labwid"} 
	             label $padNam -text "" -width $labwid -bd 1 -font $ffont
	             pack $padNam -side left
	          }
	          set rpos($nfr) $lce(4)

	          if {$form_type == 1} {
	             pack $entNam($ntent) -side left
	          } else {
	             pack $entNam($ntent) -side right
	          }
                  bind $entNam($ntent) <Button-2> {formOK}

#  entry box bind for return
	          if {$ntent > 1} {
                     bind $entNam([expr {$ntent-1}]) <Return> {
                	foreach k [array names entNam] {
                	   if {$entNam($k) == "%W"} {focus $entNam([expr {$k+1}])}
                	}
                     }
                  }

#  fill entry box with stored values from previous entry boxes
                  foreach j $fillstr {
                     foreach j1 $j {
                	if {[string match $j1* $lsfr]} {
                	   if {$xmFill($j) != ""} {set xm($ixm) $xmFill($j)}
               	           set xmFpos($j) $ixm
                	}
                     }
                  }

#------------------------------------------------------------------------------
#  file browse buttons after an entry
                  if {$browse(open) == 1 || $browse(save) == 1} {
                     set butNam $f.$nfr.file
#                     if {$xdp(DEBUG)} {puts "    FILE BROWSER $butNam"}
                     button $butNam -text "..." -pady $fpady -command \
                        "formFileBrowse $ixm $browse(open) $browse(save)"
                     pack $butNam -side right
                     set browse(open) 0
                     set browse(save) 0
                  }

#------------------------------------------------------------------------------
#  radiobutton
	       } else {
	          set radtext [join [lrange $ce 5 end] " "]
	          if {$radtext != "help" && $radtext != "Help"} {
	          set fradVr($radtext) $lce(1)
	          set radNam $f.$nfr.rad$lce(2)
                  if {$xdp(DEBUG_FORM)} {puts "    RADIO BUTTON $radNam -width $cewid -variable xm($ixm)"} 
	          radiobutton $radNam -text $radtext -font $ffont \
	             -width $cewid -relief raised -anchor w \
	             -variable xm($ixm) -value $lce(2) -command {

#  figure out which radiobuttons are pressed and store in fradVar
	             set checkfr 1
	             set chfr 0
	             while {$checkfr} {
	                incr chfr
	                if {[winfo exists .form.$chfr]} {
	                   set checkrd 1
	                   set chrd 0
	                   for {set chrd 1} {$chrd < 30} {incr chrd} {
	                      if {[winfo exists .form.$chfr.rad$chrd]} {
	                         if {[.form.$chfr.rad$chrd cget -state] == "active"} {
	                            set fradText [.form.$chfr.rad$chrd cget -text]
	                            set frad_tmp $fradVr($fradText)
	                            set fradVar($frad_tmp) $fradText
	                         }
	                      } else {
	                         set checkrd 0
	                      }
	                   }
	                } else {
	                   set checkfr 0
	                }
	             }
	          }

#  add a blank label to space the radiobuttons
	          set labwid [expr {$lce(3)-$rpos($nfr)-3}]
                  if {$labwid > 0} {
	             incr npadl
	             set padNam $f.$nfr.pad$npadl
                     if {$xdp(DEBUG_FORM)} {puts "    BLANK LABEL $padNam -width $labwid"} 
	             label $padNam -text "" -width $labwid -bd 1 -font $ffont \
	                -padx 0m -pady 0m
	             pack $padNam -side left
	          }
	          set rpos($nfr) $lce(4)

	          pack $radNam -side left
                  bind $radNam <Button-2> {formOK}
	          }
	       }

#------------------------------------------------------------------------------
#  text labels
            } else {
               if {$form_type == 1 || ($form_type == 2 && $ndash == 0)} {
                  set labNam $f.$nfr.lab
                  if {$xdp(DEBUG_FORM)} {puts "    LABEL $labNam \|$sfr\|"} 

                  if {$i >= 6} {

#  check if there are other widgets before the label, if so trim leading blanks
                      if {[winfo children $f.$nfr] != ""} {
                        set sfr [string trim $sfr " "]
                     }

#  create the label widget but if another label widget exists it indicates
#  a problem with the .men menu file
                     if {![catch {label $labNam -text $sfr -font $ffont}]} {
                        pack $labNam -side left
                        
#  check if a browser button is needed depending on the label                        
                        foreach n $vbNam {set browse($n) 0}

                        set browse(save) 0
                        if {[string first "File name:" $sfr] != -1} {set browse(save) 1}

                        set browse(open) 0
                        foreach bstr {"User file:" "Output file:"} {
                           if {$browse(open) == 0} {if {[string first $bstr $sfr] != -1} {set browse(open) 1}}
                        }

                        foreach bstr {"Variable:" "Axis Variable" "Axis Variable X:" \
                           "Axis Variable Y:" "Response Variable:" "Independent X:" \
                           "Response    Y:" "Factors/Variables:" "Factor/Variable:" \
                           "(Vert. Axis):" "(Horizontal Axis):" "Plot Char.):" \
                           "Coordinate Variable"} {
                           if {$browse(vari) == 0} {
                              if {[string first $bstr $sfr] != -1} {set browse(vari) 1}
                           }
                        }
                        
                     } else {
                        regsub "$xdp(DP_FEND)" $formFile "" r
                        errorMsg "WARNING: Problem with label \"[string trim $item]\" on form $r"
                        set labok 0
                     }

#  labels at the top of the window
                  } else {
                     label $labNam -text $sfr -font $ffont -bg $xdp(COLOR,1)
                     pack $labNam -side left

#  help button at the top of the window
                     if {$nfr == 1} {
                        foreach j {1 2} {
                           if {$j == 1} {set t Help} else {set t "Web Help"}
                           set hlpNam $f.$nfr.help
                           set hlpNam$j $f.$nfr.help$j
                           button $hlpNam$j -text $t -pady $fpady -state disabled \
                              -padx $xdp(PADX) -pady $xdp(PADY)
                           pack $hlpNam$j -side right -padx 1m -pady 1m
                        }
                     }
                  }

#  set attributes of label widget
                  bind $labNam <Button-2> {formOK}
	          set rpos($nfr) [expr {[string length $sfr] + $rpos($nfr)}]
               }
            }	          

#------------------------------------------------------------------------------
#  dataplot commands
         } else {

#  first finish window, add cancel and ok buttons, pack everything
            if {$notpack} {
               set notpack 0

#------------------------------------------------------------------------------
#  OK and cancel buttons
               button $f.$nfr.ok -text "OK" -pady $fpady -command {formOK}
               button $f.$nfr.cancel -text "Cancel" -pady $fpady -command {
                  destroy .form
                  catch {destroy .varbrowse}
               }

#  pack everything
               pack $f.$nfr.cancel $f.$nfr.ok -side right \
                  -padx 2m -pady 2m
               bind $f.$nfr.cancel <Button-2> {formOK}
               bind $f.$nfr.ok <Button-2> {formOK}
               for {set j 1} {$j <= $nfr} {incr j} {
                  if {[winfo exists $f.$j]} {
                     grid $f.$j -sticky we
                     bind $f.$j <Button-2> {formOK}
                  }
               }

#  last bind and focus for entry
               if {$ntent > 1} {bind $entNam($ntent) <Return> {focus $entNam(1)}}
               if {$ntent > 0} {focus $entNam(1)}

               pack $w.f1 -expand 1 -fill both
               update idletasks
               set bbox [grid bbox $f]
               $c configure -scrollregion $bbox
               wm geometry $w [lindex $bbox 2]\x[lindex $bbox 3]
	       set tmp [winCrop $w $xx $yy]
	       set resize [lindex $tmp 0]
	       set ww [lindex $tmp 1]
	       set hh [lindex $tmp 2]
	       if {$resize} {
		  grid $s -row 0 -column 1 -sticky news
                  wm geometry $w [expr {$ww + [winfo reqwidth $s] + 5}]\x$hh
                  wm resizable $w 0 1
	       }
            }
            
#  store dataplot commands to send when user hits OK
            if {[string range $item 0 4] != "-----"} {
               if {$nsep == 1} {
        	  incr ndpc
        	  set dpc($ndpc) $item
               }

#  look for a help command and create help and web help buttons
               set hpos [string first "HELP" $item]
               if {$hpos != -1} {
                  set hcmd [string range $item $hpos end]
                  foreach j {1 2} {$hlpNam$j configure -state normal}
                  $hlpNam1 configure -command [list sendDP $hcmd]
                  $hlpNam2 configure -command [list sendDP "WEB $hcmd"]
               }
            } else {
               if {![info exists nsep]} {set nsep 0}
               incr nsep
            }
         }   
      }
   }

   cursorConfig arrow
}

################################################################################

#  when the OK button is clicked on a form, take values from various
#  entries and radiobuttons and substitute them into the macro for the
#  form and send to Dataplot

proc formOK {} {
   global xm mixm xmFpos xmFill ndpc dpc fradVar xdp

   if {$xdp(DEBUG_FORM)} {puts "ENTER formOK"}
   pullDownWithdraw
   destroy .form
   catch {destroy .varbrowse}

#  save some fill values
   for {set j 1} {$j <= $mixm} {incr j} {
      if {$xm($j) != ""} {
         foreach k [array names xmFpos] {if {$xmFpos($k) == $j} {set xmFill($k) $xm($j)}}
      }
   }

#  loop over all dataplot command lines
   for {set j 1} {$j <= $ndpc} {incr j} {

#  commands without variable substitution, send to dataplot as is
      if {![regexp {_} $dpc($j)]} {
         if {[string range $dpc($j) 3 4] != "FE"} {
            set dosend 0
            set notif 1

#  check for @IF
            if {[lindex $dpc($j) 0] != "@IF"} {
               set dosend 1
            } else {
               if {($xm([lindex $dpc($j) 1]) == [lindex $dpc($j) 2]) || \
                  ([lindex $dpc($j) 2] == "0" && $xm([lindex $dpc($j) 1]) != "")} {
                  set dosend 1
                  set dpc($j) [lrange $dpc($j) 3 end]
               }
            }

#  send to dataplot
            if {$dosend} {
               sendDP $dpc($j)
               set nvar 1
            }
         }

#  commands with variable substitution
      } else {
         set dps [split $dpc($j) " "]
         set dpcmd ""
         set nvar 0

#  substitute variables and build command
         for {set k 0} {$k < [llength $dps]} {incr k} {
            set dosub 0
            set notif 1

#  check for @IF
            if {[lindex $dps 0] != "@IF"} {
               set dosub 1
            } else {
               if {($xm([lindex $dps 1]) == [lindex $dps 2]) || \
                  ([lindex $dps 2] == "0" && $xm([lindex $dps 1]) != "")} {
                  set dosub 1
                  set notif 0
               }
            }

#  do the substitution
            if {$dosub} {
               if {[string range [lindex $dps $k] 0 0] != "_"} {
                  if {$notif || $k > 2} {
                     lappend dpcmd [lindex $dps $k]
                  }
               } else {
                  set ind [string range [lindex $dps $k] 1 end]
                  set xmVar $xm($ind)

#  check if radiobutton label should be substituted
                  if {[info exists fradVar($ind)] && $fradVar($ind) != ""} {
                     set xmVar $fradVar($ind)
                  }
                  if {$xmVar != ""} {
                     if {$notif || $k > 2} {
                        lappend dpcmd $xmVar
                        incr nvar
                     }
                  }
               } 
            } 
         }

#  send to dataplot
         if {$nvar > 0} {sendDP [join $dpcmd " "]}
      }
   }
}

################################################################################

proc formFileBrowse {ixm bopen bsave} {
   global xm

#  display a file browser for forms
   if {$bopen} {set fopen [tk_getOpenFile -title "Open File"]}
   if {$bsave} {set fopen [tk_getSaveFile -title "Save File"]}
   if {$fopen != ""} {set xm($ixm) $fopen}
}

################################################################################

#  automatically display a `variable' browser depending which entry box
#  the cursor is in on a form

proc formChoose {entry {type "vari"}} {
   global xdp varBrowse formFocus browseFocus VB cmdLin

#  the variable VB is defined in graphSet in xdpGrset.tcl

   if {$xdp(DEBUG_FORM)} {puts "ENTER formChoose\n $type browser $entry"}
   if {$type == "none"} {catch {destroy .varbrowse}; return}

   if {![info exists browseFocus]} {set browseFocus "a"}
   if {![info exists formFocus]} {set formFocus "b"}
   if {![info exists varBrowse]} {set varBrowse ""}
   
#  display a browser for variables
   if {($formFocus != $browseFocus && $entry != $cmdLin(entry)) || $entry == $cmdLin(entry)} {
      if {($type == "vari" && $varBrowse != "") || $type != "vari"} {
         set browseFocus $formFocus
	 set w .varbrowse
	 catch {destroy $w}
	 toplevel $w
	 set title "Browse"
	 wm title    $w $VB(title,$type)
	 wm iconname $w $VB(title,$type)
	 wm iconbitmap $w @[file join $xdp(DP_TCL) xdpIcon.xbm]
	 set e4 [string range $entry 0 4]
	 set e5 [string range $entry 0 5]
	 if {$e4 == ".form"} {set p $e4; set ftyp form}
	 if {$e5 == ".input"} {set p $e5; set ftyp entry}
	 if {$e4 == ".gset"} {set p $e5; set ftyp gset}
	 set xx [expr {[winfo x $p] + [winfo width $p] + $xdp(OFFSET,x1)}]
	 set yy [winfo y $p]
	 wm geometry $w +$xx+$yy

	 frame $w.f1 -bd 1 -relief raised
	 set nb 0
	 set ir 0
	 set ic 0
	 if {$type == "vari"} {set VB(var,vari) [lsort $varBrowse]}
	 if {[info exists VB(var,$type)]} {
	    foreach v $VB(var,$type) {
	       incr nb
	       if {$ftyp == "form"} {
		  button $w.f1.$nb -text $v -highlightthickness 0 \
		     -command [list $entry insert end "$v "] -padx $xdp(PADX) -pady $xdp(PADY)
	       } elseif {$ftyp == "gset"} {
		  button $w.f1.$nb -text $v -highlightthickness 0 \
		     -command "$entry delete 0 end; $entry insert 0 $v" -padx $xdp(PADX) -pady $xdp(PADY)
	       } elseif {$ftyp == "entry"} {
		  button $w.f1.$nb -text $v -highlightthickness 0 \
		     -command [list $entry insert end " $v "] -padx $xdp(PADX) -pady $xdp(PADY)
	       }
	       grid $w.f1.$nb -row $ir -column $ic -sticky news
	       incr ic
	       if {$ic == 3} {incr ir; set ic 0}
	    }
	 }

	 frame $w.f2 -bd 1 -relief raised -bg $xdp(COLOR,1)
	 if {$ftyp == "form"} {
	    button $w.f2.clear -text Clear -command "$entry delete 0 end" -padx $xdp(PADX) -pady $xdp(PADY)
	 } elseif {$ftyp == "gset"} {
	    button $w.f2.clear -text Clear -state disabled -padx $xdp(PADX) -pady $xdp(PADY)
	 } elseif {$ftyp == "entry"} {
	    button $w.f2.clear -text Clear -command "$entry delete 0 end" -padx $xdp(PADX) -pady $xdp(PADY)
	 }
	 button $w.f2.close -text Close -command "destroy $w" -padx $xdp(PADX) -pady $xdp(PADY)
	 grid $w.f2.clear $w.f2.close -padx 3 -pady 4
	 pack $w.f1 $w.f2 -expand 1 -fill both
	 winOnScreen $w $xx $yy
      }
   }
}
