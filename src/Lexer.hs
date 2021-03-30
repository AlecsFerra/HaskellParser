{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lexer (Token(..), lexer) where

import Control.Applicative (Alternative (..), liftA2) 
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State (State, gets, modify', runState)
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (foldl', foldl1', length, reverse, )
import Data.Maybe (listToMaybe, maybeToList)
import Ourlude

data LexerError
    = Unexpected Char  -- Carattere incontrato non valido
    | UnexpectedEOF    -- Finito il file nel mezzo di un token
    | UnmatchedLayout  -- Inferenza degli sapzi
    deriving (Show, Eq)

unexpected :: String -> LexerError
unexpected []      = UnexpectedEOF
unexpected (c : _) = Unexpected c

-- a, il tipo che voglio parsare
newtype Lexer a = Lexer {
    runLexer :: String -> Either LexerError (a, String)
    --          input                       oggetto parsato, resto dello stream
}

-- Posso applicare una funzione al risultato subito dopo il parsing
instance Functor Lexer where
    -- :: (a -> b) -> Lexer a -> Lexer b
    fmap f (Lexer lexFn) = Lexer (lexFn >>> fmap (first f))

instance Applicative Lexer where
    -- a -> Lexer a
    -- Crea un lexer che non consuma nulla e restituisce a sempre
    pure a = Lexer (\stream -> Right (a, stream))

    -- :: Lexer (a -> b) -> Lexer a -> Lexer b
    -- Combina 2 lexer (posso derivare both :: Lexer a -> Lexer b -> Lexer (a, b))
    Lexer lF <*> Lexer lA =
        Lexer <| \stream -> do
            (fn, rest) <- lF stream
            (a, rest') <- lA rest
            return (fn a, rest')

instance Alternative Lexer where
    -- Lexer a
    -- Lexer che matcha sempre (unit value)
    -- empty <|> aLexer = aLexer <|> empty = aLexer
    empty = Lexer (unexpected >>> Left)

    -- Parsa A o B oppure errore
    Lexer lA <|> Lexer lB =
        Lexer <| \stream -> case (lA stream, lB stream) of
            (res, Left _) -> res -- Se ha fallito lB restituisce (a o il suo errore)
            (Left _, res) -> res -- Se ha fallito lA restituisce (b o il suo errore)
            (a@(Right (_, restA)), b@(Right (_, restB))) ->
                if length restA <= length restB then a else b
            -- Se hanno matchato entrambi restituisco chi ha il pattern migliore
            -- migliore: ha consumato più stream

-- Parsa un carattere che rispetta una condizione
satisfies :: (Char -> Bool) -> Lexer Char
satisfies condition =
    Lexer <| \case
        c : cs | condition c -> Right (c, cs)
        rest                 -> Left (unexpected rest)

-- Parsa un carattere ben definito
char :: Char -> Lexer Char
char target = satisfies (== target)

string :: String -> Lexer String
string = traverse char

-- Applica il parser migliore della lista (migliore: consuma più stream)
oneOf :: Alternative f => [f a] -> f a
oneOf = foldl1' (<|>)

data Token 
    = Let                -- let
    | Where              -- where
    | In                 -- in
    | Data               -- data
    | Type               -- type
    | If                 -- if
    | Then               -- then
    | Else               -- else
    | Case               -- case
    | Of                 -- of
    | Underscore         -- _
    | OpenParens         -- (
    | CloseParens        -- )
    | OpenBrace          -- {
    | CloseBrace         -- }
    | Semicolon          -- ;
    | DoubleColon        -- ::
    | ThinArrow          -- ->
    | VBar               -- |
    | BSlash             -- \
    | FSlash             -- /
    | Plus               -- +
    | PlusPlus           -- ++
    | Dash               -- -
    | Asterisk           --  *
    | Equal              -- =
    | Dollar             --  $
    | LeftAngle          -- <
    | Dot                -- .
    | LeftAngleEqual     -- <=
    | EqualEqual         -- ==
    | RightAngle         -- >
    | RightAngleEqual    -- >=
    | FSlashEqual        -- /=
    | VBarVBar           -- ||
    | AmpersandAmpersand -- &&
    | IntLit Int         -- Integer literal (0, 32 777)
    | StringLit String   -- String literal ("", "ciao")
    | BoolLit Bool       -- Boolean literal (True, False)
    | IntTypeName        -- Il tipo Int
    | StringTypeName     -- Il tipo String
    | BoolTypeName       -- Il tipo Bool
    | UpperName String   -- Un identifier che inizia con una maiuscola (Monad, Eq)
    | LowerName String   -- Un identifier che inizia con una minuscola (foo, bar)
    deriving (Eq, Show)

token :: Lexer (Token, String)
token = keyword <|> operator <|> literal <|> name
    where
        -- inietta b
        with :: Functor f => b -> f a -> f (b, a)
        with b = fmap (b,)

        keyword :: Lexer (Token, String)
        keyword =
            oneOf
                  -- string ritorna la stringa lexata ma la sostituisco con
                  -- la sua versione token
                [ Let        `with` string "let",
                  Where      `with` string "where",
                  In         `with` string "in",
                  Data       `with` string "data",
                  Type       `with` string "type",
                  If         `with` string "if",
                  Then       `with` string "then",
                  Else       `with` string "else",
                  Case       `with` string "case",
                  Of         `with` string "of",
                  Underscore `with` string "_" ]
        
        operator :: Lexer (Token, String)
        operator =
            oneOf
                [ OpenParens         `with` string "(",
                  CloseParens        `with` string ")",
                  OpenBrace          `with` string "{",
                  CloseBrace         `with` string "}",
                  Semicolon          `with` string ";",
                  DoubleColon        `with` string "::",
                  ThinArrow          `with` string "->",
                  VBar               `with` string "|",
                  BSlash             `with` string "\\",
                  FSlash             `with` string "/",
                  Plus               `with` string "+",
                  PlusPlus           `with` string "++",
                  Dash               `with` string "-",
                  Asterisk           `with` string "*",
                  Equal              `with` string "=",
                  Dot                `with` string ".",
                  Dollar             `with` string "$",
                  LeftAngle          `with` string "<",
                  LeftAngleEqual     `with` string "<=",
                  RightAngle         `with` string ">",
                  RightAngleEqual    `with` string ">=",
                  FSlashEqual        `with` string "/=",
                  EqualEqual         `with` string "==",
                  VBarVBar           `with` string "||",
                  AmpersandAmpersand `with` string "&&" ]

        literal :: Lexer (Token, String)
        literal = intLit <|> stringLit <|> boolLit
            where
                intLit :: Lexer (Token, String)
                intLit =
                    some (satisfies isDigit)
                        |> fmap (\matched -> (IntLit (read matched), matched))
        
                stringLit :: Lexer (Token, String)
                stringLit =
                    char '"' *> many (satisfies (/= '"')) <* char '"'
                        |> fmap (\matched -> (StringLit matched, matched))

                boolLit :: Lexer (Token, String)
                boolLit = (BoolLit True `with` string "True")
                      <|> (BoolLit False  `with` string "False")

        name :: Lexer (Token, String)
        name = primName <|> upperName <|> lowerName
            where
                primName :: Lexer (Token, String)
                primName = (IntTypeName    `with` string "Int")
                       <|> (StringTypeName `with` string "String")
                       <|> (BoolTypeName   `with` string "Bool")

                continuesName :: Lexer Char
                continuesName = satisfies isAlphaNum <|> char '\''

                followedBy :: Lexer Char -> Lexer Char -> Lexer String
                followedBy l1 l2 = liftA2 (:) l1 (many l2)

                upperName :: Lexer (Token, String)
                upperName =
                    (satisfies isUpper `followedBy` continuesName)
                        |> fmap (\x -> (UpperName x, x))

                lowerName :: Lexer (Token, String)
                lowerName =
                    (satisfies isLower `followedBy` continuesName)
                        |> fmap (\x -> (LowerName x, x))


data LinePosition = Start | Middle deriving (Eq, Show)

data Positioned a = Positioned a LinePosition Int deriving (Eq, Show)

data RawToken
    = Blankspace String
    | Comment String
    | Newline
    | NormalToken Token String

-- Raccolgo informazioni sul posizionamento dei token
rawLexer :: Lexer [RawToken]
rawLexer = some (whitespace <|> comment <|> rawToken)
    where
        -- Wrap di un token normale
        rawToken = fmap (uncurry NormalToken) token
        comment = Comment <$> (string "--" *> many (satisfies (/= '\n')))
        whitespace = blankspace <|> newline
        blankspace =
            Blankspace <$>
                some (satisfies (\x -> isSpace x && x /= '\n'))
        newline = Newline <$ char '\n'

type PosState = (LinePosition, Int)

position :: [RawToken] -> [Positioned Token]
position = foldl' go ((Start, 0), []) >>> snd >>> reverse
    where
        eat :: PosState -> RawToken -> (PosState, Maybe (Positioned Token))
        eat (pos, col) = \case
            -- Torniamo alla posizione 0
            Newline         -> ((Start, 0), Nothing)
            Comment _       -> ((Start, 0), Nothing)
            -- Sposta la posizione di s caratteri
            Blankspace s    -> ((pos, col + length s), Nothing)
            NormalToken t s ->
                let token = Positioned t pos col
                in ((Middle, col + length s), Just token)
        
        -- Dato il token precedente posso ottenere la posizione del prossimo token
        -- e wrapparlo nella posizione
        go :: (PosState, [Positioned Token]) -> RawToken -> (PosState, [Positioned Token])
        go (p, acc) raw =
            let (p', produced) = eat p raw
            in (p', maybeToList produced <> acc)

-- Explicit: graffe messe da utente
-- Implicit: graffe implicit e la loro profondità in caratteri
data Layout = Explicit | Implicit Int

data LayoutState = LayoutState
    { layouts         :: [Layout], -- stack dei layout
      tokens          :: [Token],
      expectingLayout :: Bool
    }

-- Combino manipolazione dello stato del layout alla possibilità di parsare
-- cose non valide
type LayoutM a = ExceptT LexerError (State LayoutState) a

runLayoutM :: LayoutM a -> Either LexerError [Token]
runLayoutM = --                Stato di inizio: non ho layout, stack e accetto
             --                nuovi layout
    runExceptT >>> (`runState` LayoutState [] [] True) >>> \case
        -- Se ho fallito propago l'errore
        (Left e, _) -> Left e
        -- Altrimetni restituisco i token
        (Right _, LayoutState _ tokens _) -> Right (reverse tokens)

yieldToken :: Token -> LayoutM ()
yieldToken t = modify' (\s -> s {tokens = t : tokens s})

pushLayout :: Layout -> LayoutM ()
pushLayout l = modify' (\s -> s {layouts = l : layouts s})

popLayout :: LayoutM ()
popLayout = modify' (\s -> s {layouts = drop 1 (layouts s)})

currentLayout :: LayoutM (Maybe Layout)
currentLayout = gets layouts |> fmap listToMaybe

compareIndentation :: Int -> LayoutM Ordering
compareIndentation col =
    -- Un layout esplicito è sempre più grande
    let cmp Nothing                  = GT
        cmp (Just Explicit)          = GT
        -- Altrimenti confronto la profondità
        cmp (Just (Implicit deepth)) = compare col deepth
    in fmap cmp currentLayout

layout :: [Positioned Token] -> Either LexerError [Token]
layout inputs =
    runLayoutM <| do
        mapM_ step inputs
        closeImplicitLayouts
    where
        closeImplicitLayouts =
            currentLayout >>= \case
                -- Non ho layout da chiudere
                Nothing            -> return ()
                -- Non devo chiudere layout espliciti ci deve aver pensato il
                -- programmatore
                Just Explicit      -> throwError UnmatchedLayout
                Just (Implicit _ ) -> do
                    yieldToken CloseBrace -- Chiudo il layout
                    popLayout             -- Smetto di considerarlo
                    closeImplicitLayouts  -- Chiudo i layout successivi
        
        -- Ad iniziare un layout sono le keyword let where of
        startsLayout :: Token -> Bool
        startsLayout = (`elem` [Let, Where, Of])

        step :: Positioned Token -> LayoutM ()
        step (Positioned t linePos col) = do
            -- Sto aspettando un layout?
            expectingLayout' <- gets expectingLayout
            case t of
                -- Se devo ciudere un layout esplicito lo faccio
                CloseBrace -> closeExplicitLayout
                -- Se lo devo aprire e lo posso fare lo faccio
                OpenBrace | expectingLayout' -> startExplicitLayout
                _ -- Se il token vuole aprire un layout implicito me lo aspetto
                    | startsLayout t   -> modify' (\s -> s {expectingLayout = True})
                    | expectingLayout' -> startImplicitLayout col
                    | linePos == Start -> continueImplicitLayout col
                    | otherwise        -> return ()
            yieldToken t
        
        closeExplicitLayout :: LayoutM ()
        closeExplicitLayout =
            currentLayout >>= \case
                Just Explicit -> popLayout
                -- Non posso chiudere in modo esplicito un layout implicito
                _             -> throwError (Unexpected '}')

        startExplicitLayout :: LayoutM ()
        startExplicitLayout = do
            -- Non mi aspetto altri layout
            modify' (\s -> s {expectingLayout = False})
            -- Creo il layout esplicito
            pushLayout Explicit

        startImplicitLayout :: Int -> LayoutM ()
        startImplicitLayout col = do
            modify'  (\s -> s {expectingLayout = False})
            compareIndentation col >>= \case
                -- Posso aprire un layout nuovo
                GT -> do
                    yieldToken OpenBrace
                    pushLayout (Implicit col)
                _ -> do
                    -- layout vuoto
                    yieldToken OpenBrace
                    yieldToken CloseBrace
                    continueImplicitLayout col

        continueImplicitLayout :: Int -> LayoutM ()
        continueImplicitLayout col = do
            -- Chiudo tutti i layout impliciti
            closeFurtherLayouts
            compareIndentation col >>= \case
                -- Se sono allo stesso livello sono sequenze
                EQ -> yieldToken Semicolon
                _  -> return ()
            where
                closeFurtherLayouts =
                    compareIndentation col >>= \case
                        -- Ho un layout da chiudere
                        LT -> do
                            yieldToken CloseBrace
                            popLayout
                            closeFurtherLayouts
                        -- Fine layout
                        _ -> return ()

lexer :: String -> Either LexerError [Token]
lexer input = runLexer rawLexer input >>= (fst >>> position >>> layout)