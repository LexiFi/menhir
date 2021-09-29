.PHONY: all clean test

DEMOS := \
  calc \
  calc-alias \
  calc-ast \
  calc-incremental \
  calc-inspection \
  calc-mini \
  calc-new-syntax \
  calc-param \
  calc-syntax-errors \
  calc-two \
  coq-minicalc \
  coq-syntax-errors \
  generate-printers \


all clean test:
	@ for d in $(DEMOS) ; do make -C $$d $@ ; done
