OBJS = syntax.cmo cps_out.cmo cps_in.cmo main.cmo

.PHONY: clean
let : $(OBJS)
	@echo Linking $@
	ocamlc -o $@ -I `camlp5 -where` gramlib.cma $(OBJS)

syntax.cmi:
syntax.cmo: syntax.cmi
cps_out.cmi:
cps_out.cmo: cps_out.cmi
cps_in.cmi: syntax.cmi cps_out.cmi
cps_in.cmo: cps_in.cmi syntax.cmi cps_out.cmi
main.cmo: syntax.cmi cps_in.cmi cps_out.cmi
#core.cmi: syntax.cmi
#core.cmo: syntax.cmi core.cmi

%.cmi: %.mli
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $<

%.cmo: %.ml
	ocamlc -c -I `camlp5 -where` -pp 'camlp5o pa_extend.cmo' $< 

clean:
	rm -f *.cmo *.cmi
