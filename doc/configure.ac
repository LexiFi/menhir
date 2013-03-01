# -*-autoconf-*-

# Autoconfiguration of AutomaTeX, a generic makefile for building LaTeX
# documents.
#
# Copyright 2003-2006 Christian Rinderknecht
#
# You may modify and redistribute with no charges this file as long as
# this copyright notice remains, the name "AutomaTeX" changes, the
# e-mail address changes in the AC_INIT macro call and the name in the
# `echo' shell command below is kept.

# Documentation is to be found in the companion file `Makefile.in'.

# Process this file with `autoconf' to produce a `configure' script.

# $Id: configure.ac,v 1.2 2008/02/01 20:06:42 fpottier Exp $

# ------------------------------------------------------------------------
# General settings
#
AC_INIT(AutomaTeX,0.5,rinderkn@konkuk.ac.kr)
AC_PREREQ(2.53)
AC_CONFIG_SRCDIR

ac_version=$(autoconf -V | grep GNU | sed -n 's|.*) \(.*\)$|\1|p')

echo "Autoconfiguration of AutomaTeX,"
echo "a generic makefile for building LaTeX documents."
echo "(c) 2003-2006 Christian Rinderknecht"
echo
echo "This is autoconf (GNU Autoconf)" $ac_version

echo -----------------------------------------------------------------
echo System tools

AC_PATH_PROG(SHELL,bash,none)
if test "$SHELL" = "none";
then
  echo "  => You must install \`bash'!"
fi

AC_PATH_PROG(GMKDIR,gmkdir,none)
if test "$GMKDIR" = "none";
then
  AC_PATH_PROG(MKDIR,mkdir,none)
else
  MKDIR=$GMKDIR
fi

AC_PATH_PROG(GMV,gmv,none)
if test "$GMV" = "none";
then
  AC_PATH_PROG(MV,mv,none)
else
  MV=$GMV
fi

AC_PATH_PROG(GCP,gcp,none)
if test "$GCP" = "none";
then
  AC_PATH_PROG(CP,cp,none)
else
  CP=$GCP
fi

AC_PATH_PROG(GCAT,gcat,none)
if test "$GCAT" = "none";
then
  AC_PATH_PROG(CAT,cat,none)
else
  CAT=$GCAT
fi

AC_PATH_PROG(GTR,gtr,none)
if test "$GTR" = "none";
then
  AC_PATH_PROG(TR,tr,none)
else
  TR=$GTR
fi

AC_PATH_PROG(GUNIQ,guniq,none)
if test "$GUNIQ" = "none";
then
  AC_PATH_PROG(UNIQ,uniq,none)
else
  UNIQ=$GUNIQ
fi

AC_PATH_PROG(GSED,gsed,none)
if test "$GSED" = "none";
then
  AC_PATH_PROG(SED,sed,none)
else
  SED=$GSED
fi

AC_PATH_PROG(GSORT,gsort,none)
if test "$GSORT" = "none";
then
  AC_PATH_PROG(SORT,sort,none)
else
  SORT=$GSORT
fi

AC_PATH_PROG(GHEAD,ghead,none)
if test "$GHEAD" = "none";
then
  AC_PATH_PROG(HEAD,head,none)
else
  HEAD=$GHEAD
fi

AC_PATH_PROG(GTAIL,gtail,none)
if test "$GTAIL" = "none";
then 
  AC_PATH_PROG(TAIL,tail,none)
else
  TAIL=$GTAIL
fi

AC_PATH_PROG(GWC,gwc,none)
if test "$GWC" = "none";
then
  AC_PATH_PROG(WC,wc,none)
else
  WC=$GWC
fi

AC_PATH_PROG(GAWK,gawk,none)
if test "$GAWK" = "none";
then
  AC_PATH_PROG(AWK,awk,none)
else
  AWK=$GAWK
fi

AC_PATH_PROG(GLS,gls,none)
if test "$GLS" = "none";
then
  AC_PATH_PROG(LS,ls,none)
else
  LS=$GLS
fi

AC_PATH_PROG(GMAKE,gmake,none)
if test "$GMAKE" = "none";
then 
  AC_PATH_PROG(MAKE,make,none)
  if test "$MAKE" = "none"; 
  then
    echo "  => You must install \`gmake\'!"
  else 
    $MAKE --version | grep GNU &> /dev/null
    if test $? != 0; 
    then
      echo "  => You must install \`GNU make\'!"
    else 
      $MAKE --version | grep 'GNU Make 3.80' &>/dev/null
       if test $? != 0
        then
          echo "  => You need GNU Make 3.80 or upper."
       fi
    fi
  fi
else
  MAKE=$GMAKE
fi

AC_PATH_PROG(PERL,perl,none)
if test "$PERL" = "none";
then
  echo "  => You must install \`perl\'!"
fi

AC_PATH_PROG(GEXPR,gexpr,none)
if test "$GEXPR" = "none";
then
  AC_PATH_PROG(EXPR,expr,none)
else
  EXPR=$GEXPR
fi

AC_PATH_PROG(GECHO,gecho,none)
if test "$GECHO" = "none";
then
  AC_PATH_PROG(ECHO,echo,none)
else
  ECHO=$GECHO
fi

AC_PATH_PROG(GPASTE,gpaste,none)
if test "$GPASTE" = "none";
then
  AC_PATH_PROG(PASTE,paste,none)
else
  PASTE=$GPASTE
fi

AC_PATH_PROG(GCUT,gcut,none)
if test "$GCUT" = "none";
then
  AC_PATH_PROG(CUT,cut,none)
else
  CUT=$GCUT
fi

AC_PATH_PROG(GNL,gnl,none)
if test "$GNL" = "none";
then
  AC_PATH_PROG(NL,nl,none)
else
  NL=$GNL
fi

AC_PATH_PROG(RMGNU,rm.gnu,none)
if test "$RMGNU" = "none";
then
  AC_PATH_PROG(RM,rm,none)
else
  RM=$RMGNU
fi

AC_PATH_PROG(PSNUP,psnup,none)
if test "$PSNUP" = "none"; 
then echo "  => You should install \`psnup\' for 4-up thumbnails."
fi

AC_PATH_PROG(BZIP2,bzip2,none)
if test "$BZIP2" = "none";
then echo "  => You should install \`bzip2\'."
fi

AC_PATH_PROG(GTAR,gtar,none)
if test "$GTAR" = "none";
then
  AC_PATH_PROG(TAR,tar,none)
else
  TAR=$GTAR
fi

echo -----------------------------------------------------------------
echo LaTeX tools

AC_PATH_PROG(LATEX,latex,none)
AC_PATH_PROG(PDFTEX,pdftex,none)
AC_PATH_PROG(PDFLATEX,pdflatex,none)
AC_PATH_PROG(BIBTEX,bibtex,none)
AC_PATH_PROG(MAKEINDEX,makeindex,none)
AC_PATH_PROG(HEVEA,hevea,none)
AC_PATH_PROG(OCAMLWEB,ocamlweb,none)

echo -----------------------------------------------------------------
echo Other tools

AC_PATH_PROG(CONVERT,convert,none)
AC_PATH_PROG(JPEG2EPS,jpeg2eps,none)
AC_PATH_PROG(FIG2DEV,fig2dev,none)
AC_PATH_PROG(DVIPS,dvips,none)
AC_PATH_PROG(DVIPDFMX,dvipdfmx,none)
if test "$DVIPDFMX" = "none";
then
  AC_PATH_PROG(DVIPDFM,dvipdfm,none)
else
  DVIPDFM=$DVIPDFMX
fi
AC_PATH_PROG(DVIPDF,dvipdf,none)
AC_PATH_PROG(PS2PDF,ps2pdf,none)
AC_PATH_PROG(JAVA,java,none)
AC_PATH_PROG(SAXON,saxon8.jar,none)
AC_PATH_PROG(XMLLINT,xmllint,none)
AC_PATH_PROG(ERLC,erlc,none)

echo -----------------------------------------------------------------
echo Directories and paths

echo "  => The source directory is" \'$srcdir\'

# ------------------------------------------------------------------------
# Explicit variable substitutions
#
AC_SUBST(SHELL)
AC_SUBST(MKDIR)
AC_SUBST(MV)
AC_SUBST(CP)
AC_SUBST(CAT)
AC_SUBST(TR)
AC_SUBST(UNIQ)
AC_SUBST(SED)
AC_SUBST(SORT)
AC_SUBST(HEAD)
AC_SUBST(TAIL)
AC_SUBST(WC)
AC_SUBST(AWK)
AC_SUBST(LS)
AC_SUBST(EXPR)
AC_SUBST(ECHO)
AC_SUBST(MAKE)
AC_SUBST(PERL)
AC_SUBST(PASTE)
AC_SUBST(CUT)
AC_SUBST(NL)
AC_SUBST(RM)
AC_SUBST(PSNUP)
AC_SUBST(LATEX)
AC_SUBST(BIBTEX)
AC_SUBST(MAKEINDEX)

AC_SUBST(CONVERT)
AC_SUBST(JPEG2EPS)
AC_SUBST(FIG2DEV)
AC_SUBST(DVIPS)
AC_SUBST(HEVEA)
AC_SUBST(HACHA)

AC_SUBST(OCAMLWEB)

AC_SUBST(DVIPDFM)
AC_SUBST(DVIPDF)
AC_SUBST(PS2PDF)
AC_SUBST(PDFTEX)

AC_SUBST(BZIP2)
AC_SUBST(TAR)

AC_SUBST(srcdir)

AC_SUBST(ac_version)

echo -----------------------------------------------------------------
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
chmod a-w Makefile
