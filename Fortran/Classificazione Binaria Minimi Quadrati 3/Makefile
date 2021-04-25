all: mainBB mainLM

mainBB: mainBB.f line_search.f backtrack.f prob_class_log.f
	gfortran -O2 -Wall -Wextra -Wsurprising -lblas -o $@ $^

mainLM: mainLM.f lmdif1.f prob_class_log.f
	gfortran -O2 -Wall -Wextra -Wsurprising -lblas -o $@ $^

.PHONY: clean all

clean:
	rm -f mainBB mainLM output-parameters.txt mush.mod
