EXE = EXE
OBJS = mkctl.o vars.o main.o

FC = ifort

ifeq (${FC}, ifort)
	FLAGS = -warn all -O0 -convert little_endian -assume byterecl -traceback -check all
endif

%.o : %.f90
	${FC} -c $< ${FLAGS}

all : ${EXE}

${EXE} : ${OBJS}
	${FC} -o $@ $^

.PHONY : clean re run

clean :
	rm -fv *.o *.mod ${EXE}

re : clean all

run :
	./${EXE}

