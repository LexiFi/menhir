DUNEBUILD := dune build --no-print-directory

.PHONY: test depend expected

# [make test] runs the tests.

test:
	@ $(DUNEBUILD) --display short @@test

# [make depend] regenerates the file dune.auto. This command should be
# run every time new files are added or removed or renamed in the
# subdirectory data/.

depend:
	@ echo > dune.auto
	@ $(DUNEBUILD) @depend --auto-promote || true

# [make expected] updates the contents of the reference files (those
# that contain the expected output of every test). This command should
# be run only when you trust that every test produces correct output.

expected:
	@ $(DUNEBUILD) @@test --auto-promote
