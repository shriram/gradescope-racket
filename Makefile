# Run with
#   make grade s=DIR a=ASSIGNMENT-TAG
# where DIR is the current subdirectory that holds the student's work:
# e.g.,
#   make grade a=sq s=sq/s1
# for now `grade` is the default target, so
#   make a=sq s=sq/s1
# suffices

# Grade with reasonable simulation of Gradescope's script setup
grade: grader-image
	rm -rf autograder
	mkdir autograder/
	mkdir autograder/results/
	#
	# docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/$(s):/autograder/submission ubuntu-racket /bin/bash
	docker run -ti \
		-v `pwd`/tests/$(s):/autograder/submission \
		-v `pwd`/autograder/results:/autograder/results \
		shriramk/gradescope-racket-$(a) /autograder/run_autograder
	printf "\n\n"
	cat autograder/results/results.json
	printf "\n\n"

.PHONY: grade.rkt

grade.rkt: grade-$(a).rkt
	cp $< $@

base-image:
	docker build -f Dockerfile.base-image -t gradescope-racket-base .

grader-image: grade.rkt
	docker build -f Dockerfile.grader-image -t shriramk/gradescope-racket-$(a) .

zip: gradescope-$(a).zip

gradescope-$(a).zip: grade.rkt
	rm -rf setup.sh
	echo '#!/bin/bash' > setup.sh
	tail -n +2 Dockerfile.base-image | cut -f 2- -d ' ' >> setup.sh
	zip -r upload-to-gradescope.zip setup.sh run_autograder grade.rkt lib-grade.rkt

