.PHONY: all clean test

EXECUTABLE := calc.exe

all:
	jbuilder build $(EXECUTABLE)

clean:
	jbuilder clean
	rm -f *~ .*~

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(EXECUTABLE)
