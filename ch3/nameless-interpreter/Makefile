OBJS = syntax.cmo translator.cmo core.cmo  main.cmo

.PHONY: clean
let : $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

syntax.cmi:
syntax.cmo: syntax.cmi
main.cmo: syntax.cmi core.cmi
translator.cmi: translator.cmi
translator.cmo: syntax.cmi translator.cmi
core.cmi: syntax.cmi translator.cmi
core.cmo: syntax.cmi translator.cmi core.cmi

%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
