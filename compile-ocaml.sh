#!/bin/bash
set -euo pipefail
IFS=$' \n\t'

# This script compiles and tests OCaml using the current version of Menhir.

MENHIR_ROOT=$(pwd)

# We use a dedicated opam switch where it is permitted to uninstall/reinstall
# Menhir.

if opam switch list | grep 'test-menhir' >/dev/null ; then
  echo "The switch test-menhir already exists." ;
else
  echo "Creating switch test-menhir..." ;
  opam switch create test-menhir 4.11.1 ;
  echo "Installing required packages..." ;
  opam install --yes dune ;
fi

eval $(opam env --set-switch --switch test-menhir)

# Uninstall Menhir if it is installed.

echo "Removing menhir if already installed..."
#   read -p "Can I remove it [Enter/^C]?" -n 1 -r ;
(opam remove menhir || /bin/true) >/dev/null 2>&1
make unpin > /dev/null 2>&1

# Check if everything has been committed.
# This seems required for [opam pin] to work properly.

if git status --porcelain | grep -v compile-ocaml ; then
  echo "Error: there remain uncommitted changes." ;
  git status ;
  exit 1 ;
  fi

# This functions runs a command silently, and prints its execution time.

execute () {
  echo "$1" > .command
  if gtime --format "%e" -o .time bash -c "$1" >log.out 2>log.err ; then
    echo " $(cat .time) seconds." ;
  else
    echo " failure."
    cat log.err
  fi
}

# Check out a fresh copy of OCaml at a specific tag.

TEMPDIR=/tmp/menhir-test-ocaml
mkdir -p $TEMPDIR
cd $TEMPDIR
rm -rf ocaml

echo -n "Cloning OCaml..."
execute "git clone git@github.com:ocaml/ocaml.git --depth 1 --branch 4.11.1"

cd ocaml

# Configure and compile OCaml. This step does not depend on Menhir.

echo -n "Configuring OCaml..."
execute "./configure"

echo -n "Compiling OCaml..."
execute "make -j"

echo -n "Testing OCaml..."
execute "make -C testsuite parallel"

# Install Menhir.

echo -n "Installing Menhir..."
execute "make -C $MENHIR_ROOT install"

# Re-compile OCaml's parser using Menhir.

echo -n "Recompiling OCaml's parser using Menhir..."
execute "make promote-menhir"

echo -n "Committing the recompiled parser..."
execute "git add boot/menhir && git commit -m 'make promote-menhir'"

# Take a snapshot of the ASTs produced by the current parser.

echo -n "Constructing ASTs for all source files..."
execute "make -j build-all-asts"

echo -n "Committing all ASTs..."
execute "make list-all-asts | xargs git add && git commit -m 'Build all ASTs.'"

# Compile OCaml (again).

echo -n "Compiling OCaml..."
execute "make -j"

echo -n "Testing OCaml..."
execute "make -C testsuite parallel"

# Compare the ASTs produced by the current parser with the snapshot.

echo -n "Constructing ASTs for all source files..."
execute "make -j build-all-asts"

rm -f .command .time log.{err,out}

if git diff --exit-code >/dev/null ; then
  echo "Success: the original parser and the recompiled parser agree."
else
  echo "Failure: the original parser and the recompiled parser disagree."
  echo "cd $TEMPDIR/ocaml && git status"
fi
