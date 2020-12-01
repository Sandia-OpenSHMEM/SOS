CC = oshcc

.PHONY: all
all:
	${CC} hello.c -o hello
	${CC} pi.c -o pi
	${CC} pi_reduce.c -o pi_reduce

hello: hello.c
	${CC} hello.c -o $@

pi: pi.c
	${CC} pi.c -o $@

pi_reduce: pi_reduce.c
	${CC} pi_reduce.c -o $@

.PHONY: clean
clean:
	${RM} *.o hello pi pi_reduce
