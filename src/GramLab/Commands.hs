module GramLab.Commands ( Command
                        , CommandName
                        , Help
                        , CommandSpec (..)
                        , module System.Console.GetOpt
                        , defaultMain
)
where
import Text.PrettyPrint(renderStyle,render,nest,vcat,hsep,style,Mode(..),mode,text,(<>),($$),($+$),(<+>))
import System.Console.GetOpt
import System
import qualified Data.List as List


type Command a = ([a] -> [String] -> IO ())
type CLArgName   = String
type CommandName = String
type Help        = String
data CommandSpec a =  CommandSpec (Command a)
                                  Help     
                                  [OptDescr a]
                                  [CLArgName]

defaultMain commands header = do
  args <- getArgs
  let theUsage = usage commands header
  case args of
    []           -> theUsage []
    command:opts -> case List.lookup command commands of
                      Nothing   -> theUsage  ["Invalid command: " ++ command]
                      Just spec -> runCommand theUsage spec opts

runCommand theUsage (CommandSpec command help optDesc argnames) args =             
    case getOpt Permute optDesc args of
          (o,n,[]  ) ->  command o n
          (_,_,errs) -> theUsage errs


usage commands header errs = ioError  (userError (render $     (text "")
                                                  $$  (vcat (List.map text errs)) $$ (usageMsg commands header)))

usageMsg commands header = 
        text header
    $+$ (vcat (List.map commandUsage commands))

commandUsage (name , CommandSpec command help optionDesc args) = 
    text name <> text ":"
    $$ (nest 10 (text help))
    $$ (text name <+> text "[OPTION...]" <+> hsep (map text args))
    <+>  (nest 10 (text $ usageInfo "" optionDesc))

