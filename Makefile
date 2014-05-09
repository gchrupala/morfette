
# Train Spanish model

data/es/model: data/es/train.tokpos data/es/freeling-dict/dicc-2.0.src
	morfette train --iter-pos 10 --iter-lemma 3 \
                       --dict-file  data/es/freeling-dict/dicc-2.0.src \
                       --language-conf es data/es/train.tokpos $@ +RTS -s -H2G

# Eval Spanish model

data/es/%.predict: data/es/model data/es/%.tokpos
	cut -f1 -d' ' data/es/$*.tokpos | morfette predict data/es/model > $@

%-es-eval: data/es/train.tokpos data/es/%.tokpos data/es/%.predict
	morfette eval data/es/train.tokpos data/es/$*.tokpos data/es/$*.predict \
                 --ignore-punctuation --ignore-case


# Train French model

data/fr/model: data/fr/ftb_1.tokpos data/fr/lexicon.ftb4.4morfette.csv
	morfette train --iter-pos 10 --iter-lemma 2 \
	               --dict-file data/fr/lexicon.ftb4.4morfette.csv \
                       --language-conf fr data/fr/ftb_1.tokpos $@ +RTS -s -H2G



# Eval French model 

data/fr/%.predict: data/fr/model data/fr/%.tokpos
	cut -f1 -d' ' data/fr/$*.tokpos | morfette predict data/fr/model > $@

%-fr-eval: data/fr/ftb_1.tokpos data/fr/%.tokpos data/fr/%.predict
	morfette eval data/fr/ftb_1.tokpos data/fr/$*.tokpos data/fr/$*.predict \
                 --ignore-punctuation --ignore-case


.PHONY: devel-es-eval ftb_2-fr-eval
