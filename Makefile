# Run with
# make grade s=DIR    OR    make s=DIR
# where DIR is the current subdirectory that holds the student's work:
# e.g.,
# make grade s=s1

grade.cont:
	docker run -ti -v `pwd`/$(s):/autograder/submission rg-grade

grade.scr:
	rm -rf autograder
	#
	mkdir autograder/
	cp run_autograder autograder/
	#
	mkdir autograder/source/
	cp grade.rkt autograder/source/
	#
	mkdir autograder/results/
	docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/$(s):/autograder/submission ubuntu-racket /run_autograder

base-image:
	docker build -f Dockerfile.base-image -t rg-base .
grader-image:
	docker build -f Dockerfile.grader-image -t rg-grade .


