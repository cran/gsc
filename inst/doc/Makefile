bldcmd=R CMD Sweave

default:
	make draft
	make clean

enhanced:
	make draft bldcmd=makesweave

draft:
	make gsc.pdf

gsc.pdf: gsc.tex maia2.sty
	pdflatex gsc.tex

gsc.tex: gsc.rnw
	$(bldcmd) gsc.rnw

init:
	rm -f /tmp/makesweavepipe
	mkfifo /tmp/makesweavepipe
	echo -e "0\n" > /tmp/makesweavestatus
	makesweave -i &

clean:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f tmp*
	rm -f Rplots*

