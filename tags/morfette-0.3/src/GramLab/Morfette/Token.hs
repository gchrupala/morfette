module GramLab.Morfette.Token ( Token
                              , Sentence
                              , tokenForm
                              , tokenLemma
                              , tokenPOS
                              , parseToken
                              , nullToken
                              , isNullToken 
                              )
                               
where


type Token      = (String,Maybe String,Maybe String)
type Sentence   = [Token]

tokenForm (form,_,_)   = form
tokenLemma (_,lemma,_) = lemma
tokenPOS   (_,_,pos)   = pos

parseToken line =
    case words line of 
      form:lemma:pos:_    -> (form,Just lemma,Just pos)
      [form,lemma]        -> (form,Just lemma,Nothing)
      [form]              -> (form,Nothing,Nothing)
      otherwise           -> nullToken

nullToken = ("",Nothing,Nothing)
isNullToken ("",Nothing,Nothing) = True
isNullToken _ = False
