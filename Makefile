# Run with
# make grade.XXX s=DIR
# where DIR is the current subdirectory that holds the student's work:
# e.g.,
# make grade.scr s=s1

# Grade with container
grade.cont:
	docker run -ti -v `pwd`/$(s):/autograder/submission rg-grade

# Grade with simulation of Gradescope's script
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
	docker run -ti -v `pwd`/autograder:/autograder -v `pwd`/$(s):/autograder/submission ubuntu-racket /autograder/run_autograder
	cat autograder/results/results.json
	echo " "

base-image:
	docker build -f Dockerfile.base-image -t rg-base .
grader-image:
	docker build -f Dockerfile.grader-image -t rg-grade .


