
.PHONY: clean

README::
	emacs --batch pcmpl-args.el -l pcmpl-args.el \
		-f pcmpl-args--print-readme > README

clean:
	-rm README
	-rm *.elc
