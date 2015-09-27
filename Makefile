SRC = $(shell ls -1 | egrep '^[^.]+\.[^.]+\.fs$$') \
      $(shell ls -1 | egrep '^[^.]+\.fs$$')

all:
	fsharpc $(SRC)
