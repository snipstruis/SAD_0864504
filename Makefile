SRC = $(shell ls -1 | egrep '^[^.]+\.[^.]+\.fs$$') \
      $(shell ls -1 | egrep '^[^.]+\.fs$$')

all: $(SRC)
	fsharpc $(SRC)
