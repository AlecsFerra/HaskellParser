{-# LANGUAGE LambdaCase #-}

module Parser 
    ( AST (..),
      ValName,
      ConstructorName,
      Name,
      Definition (..),
      ConstructorDefinition (..),
      ValueDefinition (..),
      Expr (..),
      Literal (..),
      BinOp (..),
      Pattern (..),
      parser
    )
where

import Control.Applicative (Alternative (..), liftA2)
import Data.List (foldl')
import Lexer (Token(..))

import Ourlude
import Types (Type (..), TypeName, TypeVar)

newtype Parser a = Parser {
    runParser :: [Token] -> [(a, [Token])]}
    --                      Un parser può essere in uno stato ambiguo
    --                      quindi ritrono tutti i possibili tree

instance Functor Parser where
    -- Applico f a tutti i risultati possibili
    fmap f (Parser p) = Parser (p >>> fmap (first f))

instance Applicative Parser where
    pure a = Parser (\stream -> [(a, stream)])

    (Parser fab) <*> (Parser fa) =
        Parser <| \stream -> do
            (ab, rest) <- fab stream
            (a, rest') <- fa rest
            return (ab a, rest')

instance Alternative Parser where
    -- Fallisco sempre a parsare ([])
    empty = Parser (const [])

    -- Combina i parsing possibili
    Parser pA <|> Parser pB =
        Parser <| \stream -> pA stream ++ pB stream 

satisfies :: (Token -> Bool) -> Parser Token
satisfies p =
    Parser <| \case
        t : ts | p t -> [(t, ts)] -- Ho token e soffisfa la condizione
        _            -> []

token :: Token -> Parser Token
token = (==) >>> satisfies

-- Satisfies con return diverso da token
-- es. estrazione da un IntLiteral
pluck :: (Token -> Maybe a) -> Parser a
pluck f =
    Parser <| \case
        t : ts -> case f t of
            Just res -> [(res, ts)] -- Estrai il valore
            _ -> []
        _ -> []

data ParseError 
    = FailedParse
    | AmbiguousParse [(AST, [Token])]
    deriving (Show)

newtype AST = AST [Definition] deriving (Eq, Show)

type ConstructorName = String

data ConstructorDefinition
    --                     Left             a b
    = ConstructorDefinition ConstructorName [Type]
    deriving (Eq, Show)

data ValueDefinition
    --               fib    :: Int -> Int -> Int
    = TypeAnnotation ValName Type
    --               fib      0 0      1
    | NameDefinition ValName [Pattern] Expr
    deriving (Eq, Show)

data Definition
    -- type       Age     = Int
    = TypeSynonym TypeName Type
    -- data          Either   a b      = Left a | Right b
    | DataDefinition TypeName [TypeVar] [ConstructorDefinition]
    -- fib :: Int -> Int -> Int, fib 0 0 = 1
    | ValueDefinition ValueDefinition
    deriving (Eq, Show)

data Literal
    = IntLiteral Int
    | StringLiteral String
    | BoolLiteral Bool
    deriving (Eq, Ord, Show)

type Name = String

type ValName = String

data BinOp
    = Add        -- +
    | Sub        -- -
    | Mul        --  *
    | Div        -- \
    | Concat     -- ++
    | Less       -- <
    | LessEq     -- <=
    | Greater    -- >
    | GreaterEq  -- >=
    | EqualTo    -- ==
    | NotEqualTo -- /=
    | Or         -- ||
    | And        -- &&
    | Cash       --  $
    | Compose    -- .
    deriving (Show, Eq)

data Pattern
    = WildcardPattern                              -- _
    | NamePattern ValName                          -- x
    | LiteralPattern Literal                       -- 12, "foo"
    | ConstructorPattern ConstructorName [Pattern] -- (Just pattern pattern)
    deriving (Eq, Show)

data Expr
    = LitExpr Literal
    | NameExpr Name                    -- foo pi
    | IfExpr Expr Expr Expr            -- if Expr then Expr else Expr
    | LambdaExpr [ValName] Expr        -- \x y z -> Expr
    | ApplyExpr Expr [Expr]            -- add Expr Expr
    | NegateExpr Expr                  -- - Expr (numeri negativi, ecc...)
    | BinExpr BinOp Expr Expr          -- Expr BinOp Expr (1 + 2, g . f)
    | CaseExpr Expr [(Pattern, Expr)]  -- case Expr of
                                       --    Pattern -> Expr
                                       --    ...
    | LetExpr [ValueDefinition] Expr   -- let [ValueDefinition] in Expr
    | WhereExpr Expr [ValueDefinition] -- Expr where [ValueDefinition]
    deriving (Eq, Show)

parser :: [Token] -> Either ParseError AST
parser input = case runParser ast input of
    []         -> Left FailedParse
    [(res, _)] -> Right res
    tooMany    -> Left (AmbiguousParse tooMany)

lowerName :: Parser ValName
lowerName =
    pluck <| \case
        LowerName n -> Just n
        _           -> Nothing

upperName :: Parser TypeName
upperName =
    pluck <| \case
        UpperName n -> Just n
        _           -> Nothing

typeVar :: Parser TypeVar
typeVar = lowerName

typeName :: Parser TypeName
typeName = upperName

valName :: Parser ValName
valName = lowerName

constructorName :: Parser ConstructorName
constructorName = upperName

name :: Parser Name
name = valName <|> constructorName

literal :: Parser Literal
literal = intLit <|> stringLit <|> boolLit
    where
        intLit =
            pluck <| \case
                IntLit i -> Just (IntLiteral i)
                _        -> Nothing
        stringLit =
            pluck <| \case
                StringLit s -> Just (StringLiteral s)
                _           -> Nothing
        boolLit =
            pluck <| \case
                BoolLit b -> Just (BoolLiteral b)
                _         -> Nothing

-- Trasforma un parser di "x" in un parser di "(x)"
parensed :: Parser a -> Parser a
parensed p = token OpenParens *> p <* token CloseParens

-- Parsa a[sep a ...]
sepBy1 :: Parser a -> Parser sep -> Parser [a]
                 -- Combina pa e many (psep pa)
sepBy1 pa psep = liftA2 (:) pa (many (psep *> pa))

-- Combina operatori associativi a sinistra (+, -, ecc)
--              separatori      elementi
opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
                             -- Invece di salvare l'elemento e basta
                             -- salvo anche il separatore vicino
opsL psep pa = liftA2 squash pa (many (liftA2 (,) psep pa))
    where
        -- Combino gli elementi con separatore in una sola espressione
        squash = foldl' (\acc (combine, a) -> combine acc a)

-- opsL ma associativo a destra
opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR psep pa = liftA2 squash pa (many (liftA2 (,) psep pa))
    where
        -- In sostanza trasformo A  (-> B), ... in """"B (-> A), ..."""""
        shift (oldStart, stack) (combine, a) =
            (a, (combine, oldStart) : stack)
        
        squash start annotated =
            let (start', annotated') = foldl' shift (start, []) annotated
             in foldl' (\acc (combine, a) -> combine a acc) start' annotated'

expr :: Parser Expr
expr = notWhereExpr <|> whereExpr
    where
        -- Contiene tutto per non loopare su left recursion nella grammatica
        notWhereExpr = letExpr <|> ifExpr <|> lambdaExpr <|> binExpr <|> caseExpr
        whereExpr    = liftA2 WhereExpr
        --                where          {[arg = x], ...}
            notWhereExpr (token Where *> braced valueDefinition)


binExpr :: Parser Expr
binExpr = cashExpr
    where 
        -- dalla minor precedenza alla massima 
        cashExpr       = opsR (BinExpr Cash    <$ token Dollar)             orExpr
        orExpr         = opsR (BinExpr Or      <$ token VBarVBar)           andExpr
        andExpr        = opsR (BinExpr And     <$ token AmpersandAmpersand) comparisonExpr
        comparisonExpr = opsL comparisonOperator concatExpr
            where
                -- Siccome hanno la stessa precedenza
                comparisonOperator =
                        (BinExpr Less       <$ token LeftAngle)
                    <|> (BinExpr LessEq     <$ token LeftAngleEqual)
                    <|> (BinExpr Greater    <$ token RightAngle)
                    <|> (BinExpr GreaterEq  <$ token RightAngleEqual)
                    <|> (BinExpr EqualTo    <$ token EqualEqual)
                    <|> (BinExpr NotEqualTo <$ token FSlashEqual)
        concatExpr     = opsL (BinExpr Concat <$ token PlusPlus) addSubExpr
        addSubExpr     = opsL addSubOperator mulDivExpr
            where addSubOperator =  (BinExpr Add <$ token Plus)
                                <|> (BinExpr Sub <$ token Dash)
        mulDivExpr     = opsL mulDivOperator composeExpr
            where mulDivOperator =  (BinExpr Mul <$ token Asterisk)
                                <|> (BinExpr Div <$ token FSlash)
        composeExpr    = opsR (BinExpr Compose <$ token Dot) unaryExpr

unaryExpr :: Parser Expr
unaryExpr = negateExpr <|> appExpr
    where
        --        posso skippare la negazione se non necessaria
        negateExpr = (token Dash *> appExpr) |> fmap NegateExpr

appExpr :: Parser Expr
appExpr = some factor |> fmap extract
    where
        extract []       = error "appExpr: No element produced after some"
        extract [e]      = e -- id
        extract (e : es) = ApplyExpr e es

factor :: Parser Expr
factor = littExpr <|> nameExpr <|> parensed expr
    where
        littExpr = fmap LitExpr  literal
        nameExpr = fmap NameExpr name

letExpr :: Parser Expr
letExpr = liftA2 LetExpr 
--         let          {[var = x], ...}  in expr
    (token Let *> braced valueDefinition) (token In *> expr)

ifExpr :: Parser Expr
ifExpr =
    IfExpr
        <$> (token If   *> expr)
        <*> (token Then *> expr)
        <*> (token Else *> expr)

lambdaExpr :: Parser Expr
lambdaExpr = token BSlash *>
    liftA2 LambdaExpr (some valName) (token ThinArrow *> expr)

onePattern :: Parser Pattern
--           a                   Some _
onePattern = unspacedPattern <|> argfulPattern
    where
        argfulPattern =
            liftA2 ConstructorPattern constructorName (some unspacedPattern)

unspacedPattern :: Parser Pattern
unspacedPattern = simplePattern <|> parensed onePattern
    where
        simplePattern =  singleConstructor
                     <|> wildCardPattern
                     <|> varPattern
                     <|> litPattern
        singleConstructor = fmap (`ConstructorPattern` []) constructorName
        wildCardPattern   = WildcardPattern <$ token Underscore
        varPattern        = fmap NamePattern valName
        litPattern        = fmap LiteralPattern literal

braced :: Parser a -> Parser [a]
braced p = token OpenBrace *> sepBy1 p (token Semicolon) <* token CloseBrace

caseExpr :: Parser Expr
caseExpr =
    --                case          Expr     of       [pattern -> Expr]
    liftA2 CaseExpr (token Case *> expr <* token Of) (braced patternDef)
        where
            patternDef = liftA2 (,) onePattern (token ThinArrow *> expr)

typeExpr :: Parser Type
typeExpr = opsR ((:->) <$ token ThinArrow) baseType
    where
        baseType = singleType <|> customType
        customType = liftA2 CustomType typeName (many typeArgument)

typeArgument :: Parser Type
typeArgument = namedType <|> singleType
    where
        namedType = fmap (`CustomType` []) typeName

singleType :: Parser Type
singleType = fmap TVar typeVar <|> primType <|> parensed typeExpr
    where
        primType =  (IntT    <$ token IntTypeName)
                <|> (StringT <$ token StringTypeName)
                <|> (BoolT   <$ token BoolTypeName)

valueDefinition :: Parser ValueDefinition
valueDefinition = nameDefinition <|> typeAnnotation
    where
        nameDefinition =
            NameDefinition
                <$> valName               -- fib 
                <*> many unspacedPattern  -- 0 0
                <*> (token Equal *> expr) -- = 1
        
        typeAnnotation =
            --                    fib            ::             Int -> Int -> Int
            liftA2 TypeAnnotation valName (token DoubleColon *> typeExpr)

constructorDefinition :: Parser ConstructorDefinition
constructorDefinition =
    liftA2 ConstructorDefinition constructorName (many typeArgument)

definition :: Parser Definition
definition = valueDefinition' <|> dataDefinition <|> typeSynonym
    where
        valueDefinition' = fmap ValueDefinition valueDefinition
        dataDefinition =
            DataDefinition
                <$> (token Data *> typeName) -- Maybe
                <*> many typeVar -- a
                <*> (token Equal *> sepBy1 constructorDefinition (token VBar)) -- Just a
        typeSynonym = liftA2 TypeSynonym
            (token Type *> typeName) (token Equal *> typeExpr)
        --  type             X        =               Int

ast :: Parser AST
ast = fmap AST (braced definition)