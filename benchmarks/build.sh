# Call this file when changing anything in the `template` dir, or adding a new
# benchmark.

root=$(pwd);
mkdir -p $root/build &&
cp -r $root/template $root/build/ &&
cd $root/build/template/backends &&
make &&
cd $root/build/template/ &&
ocaml dune.ml &&
cd $root &&
for dir in *; do
  if [ -d $dir ] && [ $dir != template ] && [ $dir != build ]
  then mkdir -p build/$dir &&  \
       cp -r build/template/* build/$dir && \
       cp -r $dir/* build/$dir/src
  fi
done &&
rm -r build/template
