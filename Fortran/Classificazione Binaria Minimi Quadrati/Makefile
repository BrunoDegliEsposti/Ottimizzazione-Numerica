main: main.f line_search.f backtrack.f prob_class_log.f
	gfortran -O2 -Wall -Wextra -Wsurprising -lblas -o $@ $^

.PHONY: run clean

run: main
	./main

clean:
	rm -f main output-parameters.txt
