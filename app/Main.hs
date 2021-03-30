module Main where

import Control.Monad ((>=>))
import Data.Char (toLower)

import qualified Lexer
import qualified Parser

import Ourlude
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrint, pPrintString)

data StagedError = StagedError String String
--                             nome stage, errore

stageEither :: Show e => String -> Either e a -> Either StagedError a
stageEither name (Left error) = Left (StagedError name (show error))
stageEither _    (Right a)    = Right a

printStagedError :: StagedError -> IO ()
printStagedError (StagedError name error) = do
    putStrLn name
    putStrLn " ERROR: "
    pPrintString error

-- i = Input dello stage, o = output dello stage
data Stage i o = Stage
    { name     :: String,
      runStage :: i -> Either StagedError o }

makeStage :: Show e => String -> (i -> Either e o) -> Stage i o
makeStage name stageFn = Stage name (stageFn >>> stageEither name)

lexerStage :: Stage String [Lexer.Token]
lexerStage = makeStage "Lexer" Lexer.lexer

parserStage :: Stage [Lexer.Token] Parser.AST
parserStage = makeStage "Parser" Parser.parser

(>->) :: Stage a b -> Stage b c -> Stage a c
(Stage _ stageFn1) >-> (Stage name2 stageFn2) = Stage name2 (stageFn1 >=> stageFn2)

printStage :: Show b => Stage a b -> a -> IO ()
printStage (Stage name stageFn) a = case stageFn a of
    Left err -> do
        printStagedError err
        exitFailure
    Right b -> do
        putStrLn ("name" ++ ":")
        pPrint b

data Args = Args FilePath (String -> IO ())

readStage ::  String -> Maybe (String -> IO ())
readStage "lex"   = lexerStage |> printStage |> Just
readStage "parse" = lexerStage >-> parserStage |> printStage |> Just
readStage _       = Nothing

process :: Args -> IO ()
process (Args path stage) = do
    content <- readFile path
    stage content

parseArgs :: [String] -> Maybe Args
parseArgs (stageName : file : _) =
    Args file <$> readStage (map toLower stageName)
parseArgs _ = Nothing

main :: IO ()
main = do
    stringArgs <- getArgs
    case parseArgs stringArgs of
        Nothing   -> putStrLn  "Unrecognized arguments"
        Just args -> process args
