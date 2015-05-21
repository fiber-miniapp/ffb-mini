MAKEMESH = ../bin/makemesh

default: test

test:
	./go.sh

test_k:
	pjsub --interact ./go_k.sh

test_fx10:
	pjsub --interact ./go_fx10.sh

clean:
	rm -f FLOW.P* les3x.log.P* MESH.P* BOUN.P* PARMLES3X.P* maprof_output.yaml go_*.sh.*
