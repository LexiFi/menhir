# Call this file when changing anything in the `template` dir, or adding a new
# benchmark.

cd template/backends && make ;

cd ../..;

for dir in *; do
  if [ -d $dir ] && [ $dir != template ] && [ $dir != build ]
  then mkdir -p build/$dir && cp -r template/* build/$dir && cp -r $dir/* build/$dir/src
  fi
done