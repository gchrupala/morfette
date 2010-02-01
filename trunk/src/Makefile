defaults: all


#program name
MORFETTE=../src/dist/build/morfette/morfette


#parameters for training and eval
# modify to suit your needs
# for french TYPE can be either ftb4 or ftbmax

TYPE=ftb4
TRAINDATADIR=../DATA/
PREF=${TRAINDATADIR}/${TYPE}
TRAINSET=${PREF}/ftb_1.pos.utf8.morpheteready
DEVSET=${PREF}/ftb_2.pos.utf8.morpheteready
GOLDSET=${PREF}/ftb_3.pos.utf8.morpheteready

LEXICON=${TRAINDATADIR}/lexicon/lexicon.ftb4.4morfette.csv
ITERPOS=5
ITERLEMMA=2
MODELNAME=${PREF}/${TYPE}.${ITERPOS}x${ITERLEMMA}.model

all: configure build
		
	
check: train eval_dev


configure:
	runghc Setup.lhs configure --user

build: configure
	runghc Setup.lhs build
	
install:
	runghc Setup.lhs install
	
clean:
	runghc Setup.lhs clean
 




train:
	${MORFETTE} train --iter-pos=${ITERPOS} --iter-lemma=${ITERLEMMA} \
	${TRAINSET} ${MODELNAME} \
	--dict=${LEXICON} +RTS -K100m -sstderr
	
	
eval_dev:
	${MORFETTE} predict  ${MODELNAME} < ${DEVSET} > ${DEVSET}.tagged
	${MORFETTE}  eval --ignore-case ${TRAINSET} ${DEVSET} ${DEVSET}.tagged |\
	tee ${DEVSET}.tagged.result.${TYPE}.${ITERPOS}x${ITERLEMMA}.model
	
	

eval_gold:
	${MORFETTE} predict  ${MODELNAME} < ${GOLDSET} > ${GOLDSET}.tagged
	${MORFETTE}  eval --ignore-case ${TRAINSET} ${GOLDSET} ${GOLDSET}.tagged |\
	tee ${GOLDSET}.tagged.result.${TYPE}.${ITERPOS}x${ITERLEMMA}.model
	