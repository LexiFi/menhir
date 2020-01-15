DUNEBUILD := dune build --no-print-directory

.PHONY: all test expected depend

# [make all] compiles the test script.

all:
	@ $(DUNEBUILD)

# [make test] runs the tests.

test:
	@ $(DUNEBUILD) --force --display short @test

# [make expected] updates the contents of the reference files (those
# that contain the expected output of every test). This command should
# be run only when you trust that every test produces correct output.

expected:
	@ $(DUNEBUILD) @test --auto-promote

# [make depend] regenerates the file dune.auto. This command should be
# run every time new files are added or removed or renamed in the
# subdirectories good/ and bad/.

depend:
	@ echo > dune.auto
	@ $(DUNEBUILD) @depend --auto-promote || true
