module TrackDefinitions (Message, MessageValue (..), run) where

import Control.Monad.State
import Data.List (find)
import Data.Maybe (isJust)
import Grammar
import Pos

type Message = Tag MessageValue
data MessageValue = UnboundVariable String
                  | WrongNumberOfArguments String Int
  deriving (Show, Eq)

-- | Analyze a Program to track definitions and where there are
-- undefined functions used.
run :: Program -> [Message]
run = flip evalState [("+", BoundFunction 0 Nothing)] . analyzeProgram

type Binding = (String, BindingValue)
data BindingValue = BoundVariable
                  | BoundFunction Int (Maybe Int)
bindingName :: Binding -> String
bindingName = fst
lengthMatches :: Int -> BindingValue -> Bool
lengthMatches _ BoundVariable = False
lengthMatches args (BoundFunction min max) =
  if args >= min
  then case max of
         Just max' -> if args <= max' then True
                      else False
         Nothing -> True
  else False
findBinding :: String -> [Binding] -> Maybe BindingValue
findBinding = lookup
bound :: String -> [Binding] -> Bool
bound name = isJust . findBinding name

boundVariable :: String -> Binding
boundVariable name = (name, BoundVariable)
boundFunction :: String -> Int -> Maybe Int -> Binding
boundFunction name min max = (name, BoundFunction min max)

bindVariable :: String -> Expression -> State [Binding] ()
bindVariable v (Constant _) =
  modify (boundVariable v:)
bindVariable v (Variable var) = do
  bindings <- get
  case findBinding var bindings of
    Just x -> modify ((v, x):)
    Nothing -> modify (boundVariable v:)
bindVariable v (List (Tag _ (Variable "lambda"):
                             Tag _ (List params):
                             _)) =
  modify (boundFunction v (length params) (Just (length params)):)
bindVariable v _ =
  modify (boundVariable v:)


analyzeProgram :: Program -> State [Binding] [Message]
analyzeProgram program = do
  messages <- mapM analyzeExpression program
  return . concat $ messages

analyzeExpression :: Tag Expression -> State [Binding] [Message]
analyzeExpression (Tag pos (Constant _)) = return []
analyzeExpression (Tag pos (Variable var)) = do
  bindings <- get
  if bound var bindings
    then return []
    else return [Tag pos $ UnboundVariable var]
analyzeExpression (Tag pos (List [Tag _ (Variable "define"),
                                  Tag _ (Variable v),
                                  body])) = do
  bindVariable v . unwrapTag $ body
  analyzeExpression body
analyzeExpression (Tag pos (List (Tag _ (Variable "lambda"):
                                  Tag _ (List params):
                                  body))) = do
  state <- get
  mapM_ (\param -> case unwrapTag param of
                     Variable var -> modify (boundVariable var:)
                     _ -> return ())
        params
  messages <- analyzeProgram body
  put state
  return messages
analyzeExpression (Tag pos (List (Tag _ (Variable "let"):
                                  Tag _ (List bindings):
                                  body))) = do
  state <- get
  mapM_ (\param -> case unwrapTag param of
                     Variable var -> modify (boundVariable var:)
                     List [Tag _ (Variable v), Tag _ value] ->
                       bindVariable v value
                     _ -> return ())
        bindings
  messages <- analyzeProgram body
  put state
  return messages
analyzeExpression (Tag pos (List (Tag _ (Variable var) : args))) = do
  bindings <- get
  let x = case findBinding var bindings of
            Just binding -> if lengthMatches (length args) binding
                            then []
                            else [Tag pos $ WrongNumberOfArguments var (length args)]
            Nothing -> [Tag pos $ UnboundVariable var]
  fmap (x ++) $ analyzeProgram args
analyzeExpression (Tag _ (List args)) = analyzeProgram args
