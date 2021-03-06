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
	mkdir autograder/
	mkdir autograder/results/
	#
	# docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/$(s):/autograder/submission ubuntu-racket /bin/bash
	docker run -ti \
		-v `pwd`/tests/$(s):/autograder/submission \
		-v `pwd`/autograder/results:/autograder/results \
		shriramk/gradescope-racket /autograder/run_autograder
	printf "\n\n"
	cat autograder/results/results.json
	printf "\n\n"

base-image:
	docker build -f Dockerfile.base-image -t gradescope-racket-base .
grader-image:
	docker build -f Dockerfile.grader-image -t shriramk/gradescope-racket .

zip:
	zip -r upload-to-gradescope.zip setup.sh run_autograder grade.rkt lib-grade.rkt

