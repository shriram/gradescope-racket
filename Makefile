# Run with
#   make grade s=DIR
# where DIR is the current subdirectory that holds the student's work:
# e.g.,
#   make grade s=sq/s1
# for now `grade` is the default target, so
#   make s=sq/s1
# suffices

# Grade with reasonable simulation of Gradescope's script setup
grade:
	rm -rf autograder
	#
	mkdir autograder/
	cp run_autograder autograder/
	#
	mkdir autograder/source/
	cp grade.rkt lib-grade.rkt autograder/source/
	#
	mkdir autograder/results/
	# docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/$(s):/autograder/submission ubuntu-racket /bin/bash
	docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/tests/$(s):/autograder/submission gr-base /autograder/run_autograder
	printf "\n\n"
	cat autograder/results/results.json
	printf "\n\n"

base-image:
	docker build -f Dockerfile.base-image -t gr-base .
grader-image:
	docker build -f Dockerfile.grader-image -t gr-grade .

zip:
	zip -r upload-to-gradescope.zip setup.sh run_autograder grade.rkt lib-grade.rkt

