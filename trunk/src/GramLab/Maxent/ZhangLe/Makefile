test : test.o IntModel.a
	ghc -lstdc++  -o test test.o IntModel.a

test.o : test.hs IntModel.o
	ghc -i/home/gchrupala/work/gramlab/maxent/src -c test.hs

IntModel.a : IntModel.o c_maxentmodel.o
	ghc -lstdc++  IntModel.o c_maxentmodel.o libmaxent.a -lz -lm   -lgfortran -lm -lgcc_s

IntModel.o : IntModel.hs
	ghc -c IntModel.hs

c_maxentmodel.o : c_maxentmodel.cpp c_maxentmodel.h config.h display.hpp eventspace.hpp\
                  eventspace.tcc ext_algorithm.hpp finite.h getopt.h gistrainer.hpp\
                  hash_map.hpp itemmap.hpp itemmap.tcc lbfgs.h lbfgstrainer.hpp\
                  line_mem_iterator.hpp line_stream_iterator.hpp\
                  maxent_cmdline.h maxentmodel.hpp meevent.hpp mmapfile.h\
                  mmapfile.hpp modelfile.hpp rfevent.hpp\
                  token_mem_iterator.hpp trainer.hpp 
	c++ -c -o c_maxentmodel.o -g -O2 -Wall -DNDEBUG -DHAVE_CONFIG_H -DBOOST_DISABLE_THREADS -O3  -I. c_maxentmodel.cpp


clean : 
	rm c_maxentmodel.o IntModel.o IntModel.a test
