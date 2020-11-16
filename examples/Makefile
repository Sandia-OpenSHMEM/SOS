CC = oshcc

.PHONY: all
all:
	${CC} hello.c -o hello
	${CC} pi.c -o pi
	${CC} pi_reduce.c -o pi_reduce
	${CC} pi_reduce_array.c -o pi_reduce_array

hello: hello.c
	${CC} hello.c -o $@

pi: pi.c
	${CC} pi.c -o $@

pi_reduce: pi_reduce.c
	${CC} pi_reduce.c -o $@

pi_reduce_array: pi_reduce_array.c
	${CC} pi_reduce_array.c -o $@

.PHONY: clean
clean:
	${RM} *.o hello pi pi_reduce pi_reduce_array
