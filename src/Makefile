defaults: all


MORFETTE=../src/dist/build/morfette/morfette
TRAINDATA=ftb4
 #put ftbmax if you want to train for ftbmax tagset

all: configure build
		
	
check: train


configure:
	runghc Setup.lhs configure --user

build: configure
	runghc Setup.lhs build
	
install:
	runghc Setup.lhs install
	
clean:
	runghc Setup.lhs clean
 




train:
	${MORFETTE} train --iter-pos=20 --iter-lemma=10 \
	../DATA/${TRAINDATA}/ftb_1.pos.utf8.morpheteready ../DATA/${TRAINDATA}/ftb_1.i20x10.model \
	--dict=../DATA/lexicon/lexicon.ftb4.4morfette.csv +RTS -K100m -sstderr
	
	
eval_dev:
	${MORFETTE} predict  ../DATA/${TRAINDATA}/ftb_1.i20x10.model < ../DATA/${TRAINDATA}/ftb_2.pos.utf8.morpheteready > ../DATA/${TRAINDATA}/ftb_2.pos.utf8.morpheteready.tagged
	${MORETTE}  eval ../DATA/${TRAINDATA}/ftb_1.i20x10.model ../DATA/${TRAINDATA}/ftb_2.pos.utf8.morpheteready |\
	tee ../DATA/${TRAINDATA}/ftb_2.pos.utf8.morpheteready.result
	
	

eval_gold:
	${MORFETTE} predict  ../DATA/${TRAINDATA}/ftb_1.i20x10.model < ../DATA/${TRAINDATA}/ftb_3.pos.utf8.morpheteready > ../DATA/${TRAINDATA}/ftb_3.pos.utf8.morpheteready.tagged
	${MORETTE}  eval ../DATA/${TRAINDATA}/ftb_1.i20x10.model ../DATA/${TRAINDATA}/ftb_3.pos.utf8.morpheteready |\
	tee ../DATA/${TRAINDATA}/ftb_3.pos.utf8.morpheteready.result