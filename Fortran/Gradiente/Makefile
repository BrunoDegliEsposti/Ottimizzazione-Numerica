main: main.f discesa_gradiente.f backtrack.f rosenbrock.f
	gfortran -O2 -Wall -Wextra -Wsurprising -lblas -o $@ $^

paraboloide: main.f discesa_gradiente.f backtrack.f paraboloide.f
	gfortran -O2 -Wall -Wextra -Wsurprising -lblas -o $@ $^

.PHONY: run clean

run: main
	./main

clean:
	rm -f main paraboloide
