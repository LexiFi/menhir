#!/bin/bash

#set -x

#---------------------------------------------------------------------
# General Settings

# In a future release, $quiet could be passed as an option
#
quiet=no

#---------------------------------------------------------------------
# We need to find `echo' first.
#
if test -z "$PATH"
then
  test -f ./gecho && echo_path=./gecho
else
  IFS=':'
  for dir in $PATH; do
    test -z "$dir" && dir=.
    if test -f $dir/gecho; then echo_path=$dir/gecho; break; fi
  done
fi

if test -z "$echo_path"
then
  if test -z "$PATH"
  then
    test -f ./echo && echo_path=./echo
  else
    IFS=':'
    for dir in $PATH; do
      test -z "$dir" && dir=.
      if test -f $dir/echo; then echo_path=$dir/echo; break; fi
    done
  fi
  test -z "$echo_path" && exit 1
fi

#---------------------------------------------------------------------
# Wrappers for several kind of displays
#
function print_newline {
  test "$quiet" != "yes" && $echo_path "$1"
}

function print {
  test "$quiet" != "yes" && $echo_path -n "$1"
}

function fatal_error {
  $echo_path "decrypt.sh: fatal error:"
  $echo_path "$1" 1>&2
  exit 1
}

function debug {
  test "$debug" = "yes" && $echo_path -n "$1"
}

function debug_newline {
  test "$debug" = "yes" && $echo_path "$1"
}

function warning {
  print_newline "decrypt.sh: warning:"
  print_newline "$1"
}

#---------------------------------------------------------------------
# Searching an application in the $PATH
#
function search_path {
  if test -z "$PATH"
  then
    if test -f ./$1; then $echo_path ./$1; break; fi
  else
    IFS=':'
    for dir in $PATH; do
      if test -z "$dir"; then dir=.; fi
      if test -f $dir/$1; then $echo_path $dir/$1; break; fi
    done
  fi
}

function search_command {
  test "$debug" = "yes" && print "checking for $1... "
  current_path=$(search_path g$1)
  if test -n "$current_path"
  then
    eval "$1_path=$current_path"
    test "$debug" = "yes" && print_newline "$current_path"
  else
    current_path=$(search_path $1)
    if test -n "$current_path"
    then
      eval "$1_path=$current_path"
      test "$debug" = "yes" && print_newline "$current_path"
    else
      print_newline " not found."
      fatal_error " => Install the command \"$1\"."
    fi
  fi
}

required_commands='basename cat cp cut echo expr head grep ls nl perl sed sort tail tr uniq wc'

#---------------------------------------------------------------------
# Parsing loop
#
while : ; do
  case "$1" in
    "") break;;
      # Help
      #
    -h | --help | -help) 
      help=yes
      help_opt=$1
      ;;
    -d | --debug)
      debug=yes
      debug_opt=$1
      ;;
      # Invalid option
      #
    -*)
      fatal_error "Invalid option \"$1\"."
      ;;
      # The TeX/LaTeX log basename
      #
     *)
      if test -n "$base_arg"
      then
        fatal_error "Only one TeX/LaTeX log file allowed."
      fi
      base=yes
      base_arg=$1
  esac
  shift
done

if test -n "$required_commands"
then
  if test "$debug" = "yes"
  then
    debug_newline "Checking availability of tools:"
  fi
  IFS=" "
  for cmd in $required_commands; do
    search_command "$cmd"
  done
fi

#---------------------------------------------------------------------
# Help
#
function usage {
  cat <<EOF
Usage: $($basename_path $0) [-h] <basename>

Extracts information from the TeX/LaTeX files and the corresponding
<basename>.log file and/or any BibTeX log file (.blg extension).

Display control:
  -h, --help         display this help and exit

Other options:
  -d, --debug        display configuration of tools
EOF
  exit 1
}

test "$help" = "yes" && usage

# --------------------------------------------------------------------
# Overfulls
#
function show_overfull_box {
  overfull_hbox=$($grep_path 'Overfull \\hbox' ${1}.log 2>/dev/null \
                  | $wc_path --lines | $tr_path --delete ' ')
  case $overfull_hbox in
   0|"") ;;
   1) $echo_path "  [W] 1 horizontal overfull.";;
   *) $echo_path "  [W] $overfull_hbox horizontal overfulls.";;
  esac
  overfull_vbox=$($grep_path 'Overfull \\vbox' ${1}.log 2>/dev/null \
                  | $wc_path --lines | $tr_path --delete ' ')
  case $overfull_vbox in
    0|"") ;;
    1) $echo_path "  [W] 1 vertical overfull.";;
    *) $echo_path "  [W] $overfull_vbox vertical overfulls.";;
  esac
}

function show_overfulls {
  show_overfull_box ${1}
  doc_class=$($sed_path --quiet \
               's|^ *\\documentclass\(\[\(.*\)\]\)\?[{]\(.*\)[}]|\3|p' $*.tex 2>/dev/null)
  if test "$doc_class" = "seminar"
  then
    slide_overfulls=$($sed_path --quiet 's|LaTeX Warning: Slide \(.*\) overfull .*|\1|p' ${1}.log \
                      | $sort_path --numeric-sort \
                      | $uniq_path \
                      | $tr_path '\n' ' ' \
                      | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
    if test "$slide_overfulls" != ""
    then
      if test "$overfull_hbox" != "0" -o "$overfull_vbox" != "0"
      then
        $echo_path "      The following warning may be partly redundant:"
      fi
      case $($echo_path $slide_overfulls | $wc_path --words | $tr_path --delete ' ') in
        1) $echo_path "  [W] Overfull at slide $slide_overfulls.";;
        2) $echo_path "  [W] Overfull at slides" $($echo_path $slide_overfulls | $sed_path 's| | and |g')".";;
        *) $echo_path "  [W] Overfulls at slides" $($echo_path $slide_overfulls | $sed_path 's| |,|g')".";;
      esac
    fi
  fi
}

# --------------------------------------------------------------------
# Underfulls
#
function SET_underfull_vbox {
  underfull_vbox=$($grep_path 'Underfull \\vbox' ${1}.log 2>/dev/null \
                   | $wc_path --lines | $tr_path --delete ' ')
}

function show_underfulls {
  underfull_hbox=$($grep_path 'Underfull \\hbox' ${1}.log 2>/dev/null \
                   | $wc_path --lines | $tr_path --delete ' ')
  case $underfull_hbox in
    0|"") ;;
    1) $echo_path "  [W] 1 horizontal underfull.";;
    *) $echo_path "  [W] $underfull_hbox horizontal underfulls.";;
  esac
  SET_underfull_vbox ${1}
  case $underfull_vbox in
    0|"") ;;
    1) $echo_path "  [W] 1 vertical underfull.";;
    *) $echo_path "  [W] $underfull_vbox vertical underfulls.";;
  esac
}

function show_under_overfulls {
  show_overfulls ${1}
  show_underfulls ${1}
  if test "$underfull_hbox" != "0" -o "$underfull_vbox" != "0" \
        -o "$overfull_hbox" != "0" -o "$overfull_vbox" != "0" \
        -o "$slide_overfulls" != ""
  then
    $echo_path "      => Rerun make for details or check $($basename_path ${1}.log) for transcript."
  fi
}

# --------------------------------------------------------------------
# Get file name
#
function SET_FILE {
  SUFFIXES=$($ls_path \
             | $sed_path -n 's|.*\.\([^\.]*\)$|\1|p' \
             | $tr_path --complement --delete '[:alnum:] \n' \
             | $tr_path ' ' '\n' \
             | $sort_path --dictionary-order \
             | $uniq_path \
             | $tr_path '\n' ' ')
  e=
  for s in $SUFFIXES; do
    case "$s" in
      ac|aux|cache|dvi|dvi0|status|in|log|log1|bib|blg);;
      *~|*\#);;
      *) e="$e;s|\.$s|\.$s |g";;
    esac
  done
  current=$($head_path --lines=$cutting_line_in_log ${1} \
             | $sed_path --expression="$e" \
             | $tr_path --delete '\n' \
             | $perl_path -pe 's|\(|\n\(|g;s|\)|\n\)\n|g' \
             | $tr_path ' ' '\n' \
             | $grep_path "([[:alnum:]\|/\|\.]\|)" \
             | $tr_path --delete '\n')
  next=$($echo_path $current | $perl_path -pe 's|\(|\n\(|g;s|\)|\)\n|g' \
          | $grep_path --invert-match "(.*)" | $tr_path --delete '\n')
  until test "$current" = "$next"; do
    tmp=$current
    current=$next
    next=$($echo_path $current | $perl_path -pe 's|\(|\n\(|g;s|\)|\)\n|g' \
            | $grep_path --invert-match "(.*)" | $tr_path --delete '\n')
  done
  FILE=$($echo_path $current | $tr_path --delete ')' | $sed_path 's=.*(\([^(]\+\)$=\1=g')
  if test -n "$FILE"
  then
    dirname_of_file=$(dirname $FILE)
    if test "$dirname_of_file" = "."
    then
      FILE=$($basename_path $FILE)
    fi
  fi
}

# --------------------------------------------------------------------
# Errors
#
function find_latex_error {
  latex_error=$($sed_path --quiet "s|^! \(.*\)|\1|p" ${1}.${2} 2>/dev/null \
                 | $head_path --lines=1)
  if test -n "$latex_error"
  then \
    line=$($sed_path --quiet "s|^l\.\([0-9]\+\).*|\1|p" ${1}.${2} \
            | $tr_path '\n' ' ' \
            | $cut_path --fields=1 --delimiter=' ')
    linenum_in_log=$($grep_path --line-number '^! .*' ${1}.${2} \
                      | $head_path --lines=1 \
                      | $sed_path --quiet 's|^\([0-9]*\):.*|\1|p')
    linenum_in_log=$($expr_path $linenum_in_log - 1)
    runaway_line=$($nl_path --body-numbering=a ${1}.${2} \
                | $sed_path --quiet "s|^ *\([0-9]\+\).*Runaway argument?|\1|p" \
                | $tr_path '\n' ' ' \
                | $cut_path --fields=1 --delimiter=' ')
    if test -n "$runaway_line"
    then
      cutting_line_in_log=$runaway_line
    else
      cutting_line_in_log=$linenum_in_log
    fi
    SET_FILE ${1}.${2}
    if test -z "$FILE" -o ! -e "$FILE"
    then
      in_file=
    else
      in_file=" in $FILE"
    fi
    if test "$latex_error" = "Undefined control sequence."
    then
      line_after=$($grep_path --after-context=1 "^! .*" ${1}.${2} 2>/dev/null \
                    | $head_path --lines=2 \
                    | $tail_path --lines=1)
      error_line=$($sed_path --quiet 's|^l.[[:digit:]]\+ \(.*\)|\1|p' ${1}.${2} \
                    | $head_path --lines=1)
      if test "$line_after" = "$error_line"
      then
        undefined_macro=$($echo_path $error_line \
                           | $sed_path --quiet 's|.*\(\\[^ ]\+\)$|\1|p')
      else
        undefined_macro=$($echo_path $line_after \
                           | $sed_path --quiet 's|.*\(\\[^ ]\+\)$|\1|p')
      fi
      if test -n "$undefined_macro"
      then
        latex_error="Undefined control sequence $undefined_macro."
      fi
    fi
    if test -z "$line"
    then
      if test -z "$FILE" -a -z "$runaway_line"
      then
        $echo_path "  [E] $latex_error"
      else
        $echo_path "  [E] Error$in_file:"
        if test -n "$runaway_line"
        then
         tmp=$($grep_path --after-context=1 "^Runaway argument?" ${1}.${2} 2>/dev/null \
                | $tail_path --lines=1 \
                | $sed_path --quiet "s|^\(.*\)$|      \1|p")
         $echo_path "$tmp"
        fi
        $echo_path "      $latex_error"
      fi
    else
      $echo_path "  [E] Error$in_file at line ${line}:"
      $echo_path "      $latex_error"
    fi
    linenum_in_log=$($expr_path $linenum_in_log + 1)
    $echo_path "      => Check line $linenum_in_log in ${1}.log."
    if test -n "$runaway_line" -a "$FILE" != ${1}.tex
    then
      $echo_path "      => Check the included or input file."
    fi
  fi
}

# --------------------------------------------------------------------
# Citations
#
function advice {
  if test -z "${3}"
  then
    $echo_path "      => Create a bibliography and/or call \\bibliography in ${2}.tex."
  else
    all_bib=$($ls_path *.bib 2>/dev/null)
    if test -z "$all_bib"
    then
      case ${1} in
        1) $echo_path "      => Check spelling or add corresponding entry.";;
        *) $echo_path "      => Check spellings or add corresponding entries.";;
      esac
    else
      case ${1} in
        1) $echo_path "      => Check spelling, add entry or add an argument to \\bibliography.";;
        *) $echo_path "      => Check spellings, add entries or add arguments to \\bibliography.";;
      esac
    fi
  fi
}

function PRINT_undef_cit {
  slave_bib=$($ls_path *.bib 2>/dev/null)
  case $($echo_path ${3} | $wc_path --words | $tr_path --delete ' ') in
    0|"") ;;
    1) $echo_path "  [W] Undefined citation \`${3}'."
       advice 1 ${2} "$slave_bib";;
    2) $echo_path "  [W] Undefined citations \`"$($echo_path ${3} | $sed_path "s| |' and \`|g")"'."
       advice 2 ${2} "$slave_bib";;
    *) $echo_path "  [W] Undefined citations: \`"$($echo_path ${3} | $sed_path "s| |',\`|g")"'."
       advice many ${2} "$slave_bib";;
  esac
  case $($echo_path ${3} | $wc_path --words | $tr_path --delete ' ') in
    0|"") ;;
    *) SET_slave_tex ${2}
       if test -n "$slave_tex" -o -n "$slave_bib"
       then
         for file in $slave_tex $slave_bib; do
           citations=
           for citation in ${3}; do
             if $grep_path "\\\cite\(\[.*\]\)\?[{].*,\?$citation,\?.*[}]" $file >/dev/null 2>&1
             then
               citations="$citations $citation"
             fi
           done
           citations=$($echo_path $citations | $sed_path --quiet "s|^ *\(.*\) *$|\1|p")
           case $($echo_path $citations | $wc_path --words | $tr_path --delete ' ') in
             0|"") ;;
             1) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                     "cites \`$citations'.";;
             2) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                     "cite \`"$($echo_path $citations | $sed_path "s| |' and \`|g")"'.";;
             *) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                     "cite \`"$($echo_path $citations | $sed_path "s| |',\`|g")"'.";;
           esac
         done
       fi;;
  esac
}

# --------------------------------------------------------------------
# Warnings and errors in bibliographies
#
function SET_slave_base_blg {
  slave_blg=$($ls_path *.blg 2>/dev/null)
  if test -n "$slave_blg"
  then 
    slave_base_blg=
    for base_blg in $slave_blg; do
      if test -z "$slave_base_blg"
      then
        slave_base_blg=$($basename_path $base_blg .blg)
      else
        slave_base_blg="$slave_base_blg $($basename_path $base_blg .blg)"
      fi
    done
  fi
}

function SET_warnings_in_blg {
  blg_files=
  for blg_base_file in ${1}; do
    if test -z "$blg_files"
    then
      blg_files=$blg_base_file.blg
    else
      blg_files="$blg_files $blg_base_file.blg"
    fi
  done
  warnings_in_blg=$($sed_path --quiet 's|^Warning--\(.*\)|\1|p' $blg_files 2>/dev/null)
}

function SET_generic_warnings_in_blg {
  if test -n "$warnings_in_blg"
  then
    generic_warnings_in_blg=$($echo_path "$warnings_in_blg" \
      | $grep_path --invert-match \
         --regexp="I didn.t find a database entry for .*" 2>/dev/null)
  fi
}

function PRINT_generic_warnings_in_blg {
  if test -n "$generic_warnings_in_blg"
  then
      $echo_path "$generic_warnings_in_blg" \
    | while read warning; do
        entry=$($echo_path $warning \
                | $sed_path --quiet 's|.* in \(.*\)|\1|p' 2>/dev/null)
        if test -n "$entry"
        then
          bibs_with_entry=$($grep_path --files-with-matches "{$entry," $bib 2>/dev/null)
          case $($echo_path $bibs_with_entry | $wc_path --words | $tr_path --delete ' ') in
            0|"") $echo_path "  [W] $warning.";;
            1) case $($echo_path $bib | $wc_path --words | $tr_path --delete ' ') in
                 0|"");;
                 1) $echo_path "  [W] $warning.";;
                 *) $echo_path "  [W] $warning of $bibs_with_entry.";;
               esac;;
            2) $echo_path "  [W] $warning of $($echo_path $bibs_with_entry | $sed_path 's| | and |g').";;
            *) $echo_path "  [W] $warning of $($echo_path $bibs_with_entry | $sed_path 's| |,|g').";;
          esac
        else
          $echo_path "  [W] $warning."
        fi
      done
  fi
}

function errors_in_blg {
  case ${1} in
    0|"") ;;
    *) if $grep_path 'I found no \\bibstyle command' $blg >/dev/null 2>&1;
       then
         $echo_path "  [E] \\bibliographystyle is missing:"
         $echo_path "      => Choose a style and add a call."
       fi
       if $grep_path 'I found no \\citation commands' $blg  >/dev/null 2>&1
       then
         $echo_path "  [E] \\cite calls are missing:"
         $echo_path "      => Add citations or use \\nocite."
       fi
       missing_bst=$($sed_path --quiet \
         "s|I couldn't open style file \(.*\)|\1|p" $blg)
       if test "$missing_bst" != ""
       then
         $echo_path "  [E] Bibliography style file ${missing_bst} is missing."
         $echo_path "      (This error is counted two times.)"
         $echo_path "      => Check spelling or make this file available."
       fi
       all_errors=$($grep_path --regexp="^[[:alpha:]][^-]*---line .*" $blg 2>/dev/null)
       for b in $BIB_BASENAMES; do
         repeated=$($echo_path "$all_errors" \
           | $sed_path --quiet \
               "s|Repeated entry---line \(.*\) of file $b.bib|\1|p" $blg \
           | $sort_path --numeric-sort \
           | $uniq_path \
           | $tr_path '\n' ' ' \
           | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
         repeated_entries=
           for line in $repeated; do
             repeated_entry=$($nl_path --body-numbering=a $b.bib \
               | $sed_path --quiet "s|^ *$line[^0-9].*@.*[{]\(.*\),.*|\1|p")
              repeated_entries="$repeated_entries"$'\n'"$repeated_entry"
           done
         repeated_entries=$($echo_path "$repeated_entries" \
                            | $sort_path --dictionary-order \
                            | $uniq_path \
                            | tr '\n' ' ' \
                            | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
         case $($echo_path $repeated_entries | $wc_path --words | $tr_path --delete ' ') in
           0|"") ;;
           1) aux="\`$repeated_entries'"
              $echo_path "  [E] Repeated entry $aux"};;
           2) aux="\`"$($echo_path $repeated_entries | $sed_path "s| |' and \`|g")"'"
              $echo_path "  [E] Repeated entries $aux";;
           *) aux="\`"$($echo_path $repeated_entries | $sed_path "s| |',\`|g")"'"
              $echo_path "  [E] Repeated entries: $aux"};;
         esac
         case $($echo_path $repeated | $wc_path --words | $tr_path --delete ' ') in
           0|"") ;;
           1) $echo_path " in ${b}.bib at line $repeated."
              $echo_path "      => Merge entries or rename or remove the redundant one.";;
           2) $echo_path "      in ${b}.bib at lines $($echo_path $repeated | $sed_path 's| | and |g')."
              $echo_path "      => Merge entries or rename or remove the redundant one.";;
           *) $echo_path "      in ${b}.bib at lines $($echo_path $repeated | $sed_path 's| |,|g')."
              $echo_path "      => Merge entries or rename or remove the redundant one.";;
         esac
       done
       other_errors=$($echo_path "$all_errors" \
         | $grep_path --invert-match --regexp="^Repeated entry---line .*" \
                2>/dev/null)
       if test -n "$other_errors"
       then
           $echo_path "$other_errors" \
         | while read err; do
              msg=$($echo_path $err \
                     | $sed_path "s=^\(.*\)---line \([0-9]\+\) \(of file \([[:alnum:]\|_\|\.]\+\)\)\?.*=  [E] \1 at line \2 in file \4.=g" \
                           2>/dev/null)
              $echo_path "$msg"
           done
       else
         two_lines_errors=$($grep_path --before-context=1 "^[-][-][-]line" $blg \
                             | $sed_path 's|^--$||g')
         if test -n "$two_lines_errors"
         then
             skip_iter=no
             $echo_path "$two_lines_errors" \
           | while read err; do
               if test $skip_iter = no
               then
                 if test -n "$err"
                 then
                   aux=$($expr_path match "$err" "I couldn't open style file \(.*\)")
                   if test -z "$aux"
                   then
                     skip_iter=no
                     msg=$($echo_path "$err" \
                            | $sed_path --quiet "s|^\([^-].*\)|  [E] \1|p")
                     if test -n "$msg"
                     then
                       $echo_path "$msg"
                     else
                       msg=$($echo_path "$err" \
                              | $sed_path --quiet "s%^---line \([0-9]\+\) \(of file \([[:alnum:]\|_\|\.]\+\)\)\?.*%      => Check line \1 in file \3.%p" \
                                    2>/dev/null)
                       if test -n "$msg"
                       then
                         $echo_path "$msg"
                       fi
                     fi
                   else
                     skip_iter=yes
                   fi
                 fi
               else
                 skip_iter=no
               fi
             done
         fi
       fi;;
  esac        
}

# --------------------------------------------------------------------
# Labels and references
#
function SET_slave_tex {
  slave_tex=$($ls_path *.tex 2>/dev/null \
              | $tr_path ' ' '\n' \
              | $grep_path \.tex \
              | while read tex_file; do \
                  if test $tex_file != ${1}; \
                  then \
                    $echo_path $tex_file; \
                  fi; \
                done)
}

function undefined_ref {
  undefined_ref=$($sed_path --quiet \
    "s|LaTeX Warning: Reference \`\([^ ]\+\)' .*|\1|p" ${1}.log \
    2>/dev/null \
  | $sort_path --dictionary-order \
  | $uniq_path \
  | $tr_path '\n' ' ' \
  | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
  case $($echo_path $undefined_ref | $wc_path --words | $tr_path --delete ' ') in
    0|"") ;;
    1) $echo_path "  [W] Undefined reference \`$undefined_ref'."
       $echo_path "      => Check spelling or add corresponding \\label.";;
    2) $echo_path "  [W] Undefined references" \
            "\`"$($echo_path $undefined_ref | $sed_path "s| |' and \`|g")"'."
       $echo_path "      => Check spellings or add corresponding \\label.";;
    *) $echo_path "  [W] Undefined references:" \
            "\`"$($echo_path $undefined_ref | $sed_path "s| |',\`|g")"'."
       $echo_path "      => Check spellings or add corresponding \\label.";;
  esac
  case $($echo_path $undefined_ref | $wc_path --words | $tr_path --delete ' ') in
    0|"") ;;
    *) SET_slave_tex ${1}
       slave_bib=$($ls_path *.bib 2>/dev/null)
       for file in $slave_tex $slave_bib; do
         references=
         for reference in $undefined_ref; do
           if $grep_path "\\\\\(page\)\?ref[{]$reference[}]" $file > /dev/null 2>&1
           then
             references="$references $reference"
           fi
         done
         references=$($echo_path $references | $sed_path --quiet "s|^ *\(.*\) *$|\1|p")
         case $($echo_path $references | $wc_path --words | $tr_path --delete ' ') in
           0|"") ;;
           1) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                   "refers to \`$references'.";;
           2) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                   "refers to" "\`"$($echo_path $references | $sed_path "s| |' and \`|g")"'.";;
           *) $echo_path "         *" $($echo_path $file | $sed_path 's| |,|g') \
                   "refers to:" "\`"$($echo_path $references | $sed_path "s| |',\`|g")"'.";;
         esac
       done;;
  esac
}

function SET_undefined_citations {
  undefined_citations=$($sed_path --quiet \
    "s|LaTeX Warning: Citation \`\([^ ]\+\)' .*|\1|p" ${1}.log 2>/dev/null \
  | $sort_path --dictionary-order \
  | $uniq_path \
  | $tr_path '\n' ' ' \
  | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
}

function find_all_missing_cit {
  SET_undefined_citations ${1}
  PRINT_undef_cit undefined_citations ${1} "$undefined_citations"
}

function find_add_missing_cit {
  SET_undefined_citations ${1}
  SET_slave_base_blg ${1}
  SET_warnings_in_blg "$slave_base_blg"
  if test -n "$warnings_in_blg"
  then
    missing_entries=$($echo_path "$warnings_in_blg" \
    | $sed_path --quiet 's|I didn.t find a database entry for "\(.*\)"|\1|p' 2>/dev/null \
    | $sort_path --dictionary-order \
    | $uniq_path \
    | $tr_path '\n' ' ' \
    | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
  fi
  additional_citations=
  remaining_entries=$missing_entries
  for citation in $undefined_citations; do
    if test -z "$remaining_entries"
    then
      additional_citations="$additional_citations $citation"
    else
      fst_entry=$($echo_path $remaining_entries | $cut_path --fields=1 --delimiter=' ')
      if test "$fst_entry" = "$citation"
      then
        remaining_entries=$($echo_path $remaining_entries | $cut_path --fields=2- --delimiter=' ')
      else
        additional_citations="$additional_citations $citation"
      fi
    fi
  done
  additional_citations=$($echo_path $additional_citations \
                         | $sed_path --quiet "s|^ *\(.*\) *$|\1|p")
  PRINT_undef_cit additional_citations ${1} "$additional_citations"
}

# --------------------------------------------------------------------
# Detailed report of underfulls and overfulls in .log
#
function SET_page {
  page=$($head_path --lines=${2} ${1} 2>/dev/null\
          | $tr_path --delete '\n' | $tr_path --delete ' ' \
          | $sed_path --quiet 's|.*\[\([0-9]\+\)\].*|\1|p' 2>/dev/null)
  page=$($expr_path $page + 1 2>/dev/null)
  if test $? -ne 0
  then
    page=
  fi
}

function hbox_init {
  linenum_in_log=$($echo_path "$line" | $sed_path --quiet 's|^\(.*\):.*|\1|p')
  prefix=$($head_path --lines=$linenum_in_log ${1}.log1)
  line_in_log=$($echo_path "$prefix" | $tail_path --lines=1)
  linenum_in_tex=$($echo_path "$line_in_log" \
                   | $sed_path --quiet 's|.* \(line [0-9]\+\)|\1|p')
  if test -z "$linenum_in_tex"
  then
    linenum_in_tex=$($echo_path "$line_in_log" \
                   | $sed_path --quiet 's|.* \(lines [0-9]\+--[0-9]\+\)|\1|p')
  fi
}

function SET_message {
  file_size=$($wc_path --lines ${1}.log1 | awk '{ print $1 }')
  suffix=$($tail_path --lines=$($expr_path $file_size - $linenum_in_log) ${1}.log1)
  message_size=$($echo_path "$suffix" \
                  | $grep_path "^ \[\]\$" --line-number --max-count=1 \
                  | $sed_path 's|\([0-9]\+\):.*|\1|g' \
                 )
  message_size=$($expr_path $message_size - 1)
  message=$($echo_path "$suffix" | $head_path --lines=$message_size)
  new_suffix_size=$($expr_path $file_size - $linenum_in_log - $message_size)
  new_suffix=$($tail_path --lines=$new_suffix_size ${1}.log1)
  prefix=$($head_path --lines=$linenum_in_log ${1}.log1)
}

function clean_message {
  $echo_path "$prefix" >| ${1}.log1
  for ((num=$message_size;$num>0;num--)) do
    $echo_path | ($cat_path >> ${1}.log1)
  done
  $echo_path "$new_suffix" >> ${1}.log1
}

function u_vbox {
  linenum_in_log=$($echo_path "$line" | $sed_path --quiet 's|^\(.*\):.*|\1|p')
  line_in_log=$($head_path --lines=$linenum_in_log ${1}.log1 \
                 | $tail_path --lines=1)
  badness=$($echo_path "$line_in_log" \
            | $sed_path --quiet 's|.*\((badness .*)\).*|\1|p')
  SET_page ${1}.log1 $linenum_in_log
  cutting_line_in_log=$linenum_in_log
  SET_FILE ${1}.log1 $linenum_in_log
  if test -z "$FILE"
  then
    $echo_path "  [W] Underfull \\vbox $badness at page $page."
  else
    $echo_path "  [W] Underfull \\vbox $badness in $FILE."
    $echo_path "      => Check page $page."
  fi
}

function PRINT_message {
  filtered=$($echo_path "$message" \
              | $tr_path --delete '\n' \
              | $sed_path -e 's+\\OT1/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\OMS/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\OML/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\T1/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\OMS/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\OMX/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\U/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\OT2/[[:alnum:]\|/\|\.]*++g' \
                    -e 's+\\PD1/[[:alnum:]\|/\|\.]*++g' \
                    -e 's|\[\]||g' \
                    -e 's|$[^$]*\$|<maths>|g' \
                    -e 's|$[^$]*$|<maths>|g' \
                    -e 's|\^^[[:alnum:]]*||g' \
                    -e 's|^ *||g' \
                    -e 's|[ ]\+| |g')
  if test -n "$filtered"
  then $echo_path $':\n' "     $filtered"
  else $echo_path "."
  fi
}

function u_hbox {
  hbox_init ${1}
  badness=$($echo_path "$line_in_log" \
             | $sed_path --quiet 's|.*\((badness .*)\).*|\1|p')
  SET_page ${1}.log1 $linenum_in_log
  cutting_line_in_log=$linenum_in_log
  SET_FILE ${1}.log1 $linenum_in_log
  SET_message ${1} $linenum_in_log
  if test -z "$FILE"
  then
    $echo_path "  [W] Underfull \\hbox $badness at page $page."
  else
    if test -z "$linenum_in_tex"
    then
      $echo_path -n "  [W] Underfull \\hbox $badness in $FILE"
    else
      $echo_path -n "  [W] Underfull \\hbox $badness in $FILE at $linenum_in_tex"
    fi
    PRINT_message
    $echo_path "      => Check page $page and line $linenum_in_log in ${1}.log."
  fi
  clean_message ${1}
}

function o_hbox {
  hbox_init ${1}
  too_wide=$($echo_path "$line_in_log" \
              | $sed_path --quiet 's|.*(\(.*pt\) too wide).*|\1|p')
  SET_page ${1}.log1 $linenum_in_log
  cutting_line_in_log=$linenum_in_log
  SET_FILE ${1}.log1
  SET_message ${1}
  if test -z "$FILE"
  then
    $echo_path "  [W] Overfull \\hbox ($too_wide) at page $page."
  else
    if test -z "$linenum_in_tex"
    then
      $echo_path -n "  [W] Overfull \\hbox ($too_wide) in $FILE"
    else
      $echo_path -n "  [W] Overfull \\hbox ($too_wide) in $FILE at $linenum_in_tex"
    fi
    PRINT_message
    $echo_path "      => Check page $page and line $linenum_in_log in ${1}.log."
  fi
  clean_message ${1}
}

function o_vbox {
  linenum_in_log=$($echo_path "$line" | $sed_path --quiet 's|^\(.*\):.*|\1|p')
  line_in_log=$($head_path --lines=$linenum_in_log ${1}.log1 \
                 | $tail_path --lines=1)
  too_high=$($echo_path "$line_in_log" \
              | $sed_path --quiet 's|.*(\(.*pt\) too high).*|\1|p')
  SET_page ${1}.log1 $linenum_in_log
  cutting_line_in_log=$linenum_in_log
  SET_FILE ${1}.log1
  if test -z "$FILE"
  then
    $echo_path "  [W] Overfull \\vbox ($too_high) at page $page."
  else
    $echo_path "  [W] Overfull \\vbox ($too_high) in $FILE."
    $echo_path "      => Check page $page."
  fi
}

function slide_overfull {
  linenum_in_log=$($echo_path "$line" \
                    | $sed_path --quiet 's|^\(.*\):LaTeX Warning:.*|\1|p')
  line_in_log=$($head_path --lines=$linenum_in_log ${1}.log1 \
                 | $tail_path --lines=1)
  points=$($echo_path "$line_in_log" \
            | $sed_path --quiet 's|.* by \(.*pt\) .*|\1|p')
  SET_page ${1}.log1 $linenum_in_log
  cutting_line_in_log=$linenum_in_log
  SET_FILE ${1}.log1
  if test -z "$FILE"
  then
    $echo_path "  [W] Slide overfull ($points) at page $page."
  elif test -e "$FILE"
    then
      $echo_path "  [W] Slide overfull ($points) in $FILE."
      $echo_path "      => Check page $page."
    else
      $echo_path "  [W] Slide overfull ($points) at page $page."
      $echo_path "      => Correct previous warning to get the file name."
  fi
}

function show_detailed_under_overfulls {
  $cp_path --force ${1}.log ${1}.log1
    $grep_path --line-number rfull ${1}.log 2>&1 \
  | while read line; do
      slide_overfull=$($echo_path "$line" \
                        | $sed_path --quiet 's|LaTeX Warning: Slide .* overfull .*|yes|p')
      if test -z "$slide_overfull"
      then
        kind=$($echo_path "$line" | awk '{ print $1 $2 }')
      else
        kind=slide_overfull
      fi
      case "$kind" in
        [0-9]*:Underfull\hbox) u_hbox ${1};;
        [0-9]*:Underfull\vbox) u_vbox ${1};;
        [0-9]*:Overfull\hbox) o_hbox ${1};;
        [0-9]*:Overfull\vbox) o_vbox ${1};;
        slide_overfull) slide_overfull ${1};;
      esac
    done
}

# --------------------------------------------------------------------
# Missing files
#
function SET_filtered_missing_files {
  missing_files=$($sed_path --quiet 's|^No file \([^ ]\+\)\.|\1|p' ${2}.log 2>/dev/null)
  filtered_missing_files=
  for file in $missing_files; do
    reject=false
    for suffix in ${1}; do
      without_suffix=$($basename_path $file .$suffix)
      if test "$file" != "$without_suffix"
      then
        reject=true
        break
      fi
    done
    if test "$reject" = "false"
    then
      filtered_missing_files="$filtered_missing_files $file"
    fi
  done
}

function no_file_warning {
  SET_filtered_missing_files "aux lof bbl toc ind" ${1}
  case $($echo_path $filtered_missing_files | $wc_path --words | $tr_path --delete ' ') in
    0|"");;
    1) $echo_path "  [W] Missing file$filtered_missing_files."
       $echo_path "      => Check the included or input file name.";;
    2) $echo_path "  [W] Missing files $($echo_path $filtered_missing_files | $sed_path 's| | and |g')."
       $echo_path "      => Check the included or input file names.";;
    *) $echo_path "  [W] Missing files $($echo_path $filtered_missing_files | $sed_path 's| |,|g')."
       $echo_path "      => Check the included or input file names.";;
  esac
}

# --------------------------------------------------------------------
# Unused LaTeX option
#
function unused_latex_option {
  unused=$($grep_path --context=1 "Unused global option(s):" ${1}.log)
  if test -n "$unused"
  then
    $echo_path "  [W]" $($echo_path $unused | $tr_path '\n' ' ')
  fi
}

# --------------------------------------------------------------------
# Hyperref warnings
#
function hyperref_warnings {
    $grep_path --after-context=1 "Package hyperref Warning: Token not allowed" ${1}.log 2>&1 \
  | while read warning; do
      if test "$warning" != "--"
      then
        $echo_path "  [W] $warning" 
      fi
    done
    bookmark=$($grep_path "Package hyperref Warning: bookmark level" ${1}.log 2>&1)
    if test -n "$bookmark"
    then
      $echo_path "  [W] $bookmark"
    fi
}

# --------------------------------------------------------------------
# Status of document processing
#
function show_common_status {
  if test ! -e ${1}.log
  then
    $echo_path "TeX log file $($basename_path ${1}.log) is missing."
  else
    show_detailed_under_overfulls ${1}
    find_latex_error ${1} log1
    if test -z "$latex_error"
    then
      toc=$($grep_path '^ *\\tableofcontents' *.tex 2>/dev/null)
      if test -n "$toc"
      then
        packages=$($sed_path --quiet \
                       's|^ *\\usepackage\(\[.*\]\)*[{]\(.*\)[}]|\2|p' \
                       ${1}.tex 2>/dev/null \
                   | $tr_path ',' ' ' \
                   | $tr_path ' ' '\n' \
                   | $sort_path --dictionary-order \
                   | $uniq_path) \
        tocbibind=$($echo_path $packages | $grep_path tocbibind 2>/dev/null)
        if test -z "$tocbibind"
        then
          $echo_path "  [W] Use \usepackage[nottoc]{tocbibind} to get the table of contents right."
        fi
      fi
      no_file_warning ${1}
      undefined_ref ${1}
      find_all_missing_cit ${1}
      multiple_labels=$($sed_path --quiet \
        "s|LaTeX Warning: Label \`\(.*\)' multiply defined.|\1|p" ${1}.log 2>/dev/null \
      | $sort_path --dictionary-order \
      | $uniq_path \
      | $tr_path '\n' ' ' \
      | $sed_path --quiet "s|^ *\(.*\) $|\1|p")
      case $($echo_path $multiple_labels | $wc_path --words | $tr_path --delete ' ') in
        0|"") ;;
        1) $echo_path "  [W] Multiply-defined label \`$multiple_labels'."
           $echo_path "      => Check spellings or rename or remove all but one occurrence.";;
        2) $echo_path "  [W] Multiply-defined labels" \
                "\`"$($echo_path $multiple_labels | $sed_path "s| |' and \`|g")"'."
           $echo_path "      => Check spellings or rename or remove all but one occurrence.";;
        *) $echo_path "  [W] Multiply-defined labels" \
                "\`"$($echo_path $multiple_labels | $sed_path "s| |',\`|g")"'."
           $echo_path "      => Check spellings or rename or remove all but one occurrence.";;
      esac
      unused_latex_option ${1}
      hyperref_warnings ${1}
    fi
  fi
}

function bib_status {
  bib=${1}.bib
  case $($echo_path $bib | $wc_path --words | $tr_path --delete ' ') in
    1) $echo_path "Status of bibliography $bib:";;
    2) $echo_path "Status of bibliographies" \
            $($echo_path $bib | $sed_path 's| | and |g'):;;
    *) $echo_path "Status of bibliographies" \
            $($echo_path $bib | $sed_path 's| |,|g'):;;
  esac
}

function show_status {
  show_common_status ${1}
  SET_slave_base_blg ${1}
  if test -n "$slave_base_blg"
  then
    for base_blg in $slave_base_blg; do
      blg=$base_blg.blg
      if test ! -e $blg
      then
        $echo_path "BibTeX log file $blg is missing."
      else
        bib_status $base_blg
        SET_warnings_in_blg $base_blg
        SET_generic_warnings_in_blg
        PRINT_generic_warnings_in_blg
        num_errors=$($sed_path --quiet 's=.*\(was\|were\) \(.*\) error.*=\2=p' \
                                 $blg 2>/dev/null)
        errors_in_blg $num_errors
        if test -z "$num_errors" -a -z "$generic_warnings_in_blg"
        then
          $echo_path "nothing to report."
        fi
      fi
    done
  fi
}

show_status $base_arg
