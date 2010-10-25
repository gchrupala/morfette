defaults: all





#program name
MORFETTE=dist/build/morfette/morfette


#parameters for training and eval
# modify to suit your needs
# for french TYPE can be either ftb4 or ftbmax

TYPE=ftb4
TRAINDATADIR=DATA/
PREF=${TRAINDATADIR}/${TYPE}
TRAINSET=${PREF}/ftb_1.pos.utf8.morpheteready
DEVSET=${PREF}/ftb_2.pos.utf8.morpheteready
GOLDSET=${PREF}/ftb_3.pos.utf8.morpheteready

LEXICON=${TRAINDATADIR}/lexicon/lexicon.ftb4.4morfette.csv
ITERPOS=10
ITERLEMMA=3
MODELNAME=${PREF}/${TYPE}.${ITERPOS}x${ITERLEMMA}.model

all: configure build
		
	
check: train eval_dev



# note that if you want to install in your own home directory
# add a --prefix=DIRTOINSTALL  option right after the --user
# as in 
# runghc Setup.lhs configure --user --prefix=/home/foo/bar
# the install target will then install the morfette compiled binary
# in /home/foo/bar/bin  (bin must exist)

configure:
	runghc Setup.lhs configure --user


build: configure
	runghc Setup.lhs build
	
install: configure build
	runghc Setup.lhs install
	
	
install_home:
                                                                                                                                                          	
	
clean:
	runghc Setup.lhs clean
 




train:
	${MORFETTE} train --iter-pos=${ITERPOS} --iter-lemma=${ITERLEMMA} \
	${TRAINSET} ${MODELNAME} \
	--dict=${LEXICON} +RTS -K100m -sstderr
	
	
eval_dev:
	#${MORFETTE} predict  ${MODELNAME} < ${DEVSET} > ${DEVSET}.tagged
	${MORFETTE}  eval --ignore-case /dev/null ${DEVSET} ${DEVSET}.tagged |\
	tee ${DEVSET}.tagged.result.${TYPE}.${ITERPOS}x${ITERLEMMA}.model
	
	

eval_gold:
	${MORFETTE} predict  ${MODELNAME} < ${GOLDSET} > ${GOLDSET}.tagged
	${MORFETTE}  eval --ignore-case ${TRAINSET} ${GOLDSET} ${GOLDSET}.tagged |\
	tee ${GOLDSET}.tagged.result.${TYPE}.${ITERPOS}x${ITERLEMMA}.model
	