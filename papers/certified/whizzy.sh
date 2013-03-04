# Include TEXINPUTS setting from Makefile.local.

# Do not include all of Makefile.local, because both whizzytex and Makefile
# rely on NAME (for different purposes).

if [ -f Makefile.local ]
  then
    echo "Extracting TEXINPUTS setting from Makefile.local..."
    `grep TEXINPUTS Makefile.local`
  fi

# Set DVICOPY.
# Using dvicopy provides support for virtual fonts, which advi lacks.
# Renaming $1 (which typically stands for "main.dvi") allows Christian's
# Makefile not to become confused.

mvdvicopy () { mv "$1" "/tmp/$1" && dvicopy "/tmp/$1" "$2"; }
DVICOPY=mvdvicopy

