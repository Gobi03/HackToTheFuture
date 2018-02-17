.PHONY: all clean submit
all:
	./all-compile.sh

submit:
	./make-submit.sh

clean:
	${RM} *.class

test:
	./all-compile.sh
	scala Main < ../resource/example_01.txt > example_01_out.txt
