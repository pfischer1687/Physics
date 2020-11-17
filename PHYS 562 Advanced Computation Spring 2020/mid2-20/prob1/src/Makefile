
objs1 = numtype.o hrmosc.o

prog1 = hrm

f90 = gfortran

f90flags = -O3 -funroll-loops -ftree-vectorize \
	-fexternal-blas 

libs = -framework Accelerate 

ldflags = $(libs)

all: $(prog1)

$(prog1): $(objs1)
	$(f90) $(ldflags) -o $@ $(objs1)

clean: 
	rm -f $(prog1) *.{o,mod} fort.*

.suffixes: $(suffixes) .f90

%.o: %.f90
	$(f90) $(f90flags) -c $<
