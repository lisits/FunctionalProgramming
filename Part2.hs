import Text.Read (readMaybe)
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Control.Monad (foldM)

-- Тип для представления стека
type Stack = [Int]

-- Результат выполнения программы
data Result = Ok | Error String deriving (Show, Eq)

-- Тип команд
data Command = Push Int      -- Операция помещения числа в стек
             | Add           -- Операция сложения
             | Subtract      -- Операция вычитания
             | Multiply      -- Операция умножения
             | Divide        -- Операция деления
             | Modulo        -- Операция взятия остатка
                -- команды для манипулирования стеком
             | Dup           -- Дублирование вершины стека
             | Drop          -- Удаление вершины стека
             | Swap          -- Обмен местами двух элементов
             | Over          -- Копирование второго элемента
             | Rot           -- Вращение верхних трёх элементов стека
                -- команды для сравнения
             | Equals        -- Сравнение - равно
             | LessThan      -- Сравнение - меньше чем
             | GreaterThan   -- Сравнение - больше чем
                -- алгебра логики
             | And           -- Булевское И
             | Or            -- Булевское ИЛИ
             | Invert        -- Булевское НЕ
                -- ввод вывод
             | Print         -- Вывод числа
             | CR            -- Перевод строки
             | Emit          -- Вывод символа, код которого на вершине стека
             | Key           -- Ждём нажатие клавиши
                -- обработка слов
             | Define String [Command] -- Объявление нового слова
             | Execute String          -- Обработка нового слова
             | PrintString String      -- Вывод строки
             deriving Show

-- Словарь для новых слов: слово -> последовательность команд
type Dictionary = Map.Map String [Command]

-- Функция выполнения команды
execute :: Dictionary -> Stack -> Command -> IO (Result, Stack, Dictionary)
execute dict stack (Push n) = return (Ok, n : stack, dict)

-- Арифметика
execute dict (x:y:stack) Add = return (Ok, (y + x) : stack, dict)
execute dict (x:y:stack) Subtract = return (Ok, (y - x) : stack, dict)
execute dict (x:y:stack) Multiply = return (Ok, (y * x) : stack, dict)
execute dict (x:y:stack) Divide
    | x == 0    = return (Error "Division by zero", stack, dict)
    | otherwise = return (Ok, (div y x) : stack, dict)
execute dict (x:y:stack) Modulo
    | x == 0    = return (Error "Modulo by zero", stack, dict)
    | otherwise = return (Ok, (mod y x) : stack, dict)

-- Манипулирование стеком
execute dict stack Dup
    | null stack = return (Error "Stack underflow", stack, dict)
    | otherwise  = return (Ok, head stack : stack, dict)
execute dict stack Drop
    | null stack = return (Error "Stack underflow", stack, dict)
    | otherwise  = return (Ok, tail stack, dict)
execute dict (x:y:stack) Swap = return (Ok, y : x : stack, dict)
execute dict (x:y:stack) Over = return (Ok, y : x : y : stack, dict)
execute dict (x:y:z:stack) Rot = return (Ok, z : x : y : stack, dict)

-- Булевские операции
execute dict (x:y:stack) Equals = return (Ok, (if x == y then -1 else 0) : stack, dict)
execute dict (x:y:stack) LessThan = return (Ok, (if y < x then -1 else 0) : stack, dict)
execute dict (x:y:stack) GreaterThan = return (Ok, (if y > x then -1 else 0) : stack, dict)
execute dict (x:y:stack) And = return (Ok, (if x /= 0 && y /= 0 then -1 else 0) : stack, dict)
execute dict (x:y:stack) Or = return (Ok, (if x /= 0 || y /= 0 then -1 else 0) : stack, dict)
execute dict (x:stack) Invert = return (Ok, (if x == 0 then -1 else 0) : stack, dict)

-- Ввод-вывод
execute dict (x:stack) Print = do
    putStr (show x ++ " ")
    return (Ok, stack, dict)
execute dict stack CR = do
    putStrLn ""
    return (Ok, stack, dict)
execute dict (x:stack) Emit = do
    putChar (chr x)
    return (Ok, stack, dict)
execute dict stack Key = do
    hFlush stdout
    c <- getChar
    return (Ok, ord c : stack, dict)

-- Вывод строкового значения 
execute dict stack (PrintString str) = do
  putStr str
  return (Ok, stack, dict)

-- Определение нового слова
execute dict stack (Define name cmds) =
  return (Ok, stack, Map.insert name cmds dict)

-- Выполнение команд для сущестующего слова
execute dict stack (Execute name) =
  case Map.lookup name dict of
    Just cmds -> do
      (result, newStack, newDict) <- processProgram dict stack cmds
      return (result, newStack, newDict)
    Nothing -> return (Error ("Unknown word: " ++ name), stack, dict)
-- Если чуда не произошла – ошибка
execute _ _ _ = return (Error "Stack underflow", [], Map.empty)

-- Последовательное выполнение списка команд
processProgram :: Dictionary -> Stack -> [Command] -> IO (Result, Stack, Dictionary)
processProgram dict stack cmds =
  foldM execOne (Ok, stack, dict) cmds
  where
    execOne (Ok, st, d) cmd = execute d st cmd
    execOne (err, st, d) _  = return (err, st, d)

-- Расчленение строки
parseStringLiteral :: String -> [String] -> Either String (String, [String])
parseStringLiteral part rest =
  let initial = drop 2 part  -- удаляем первые два символа: ."
  in if not (null initial) && last initial == '"'
       then Right (init initial, rest)
       else collectString initial rest
  where
    collectString acc [] = Left "Unterminated string literal"
    collectString acc (t:ts) =
      let newAcc = if null acc then t else acc ++ " " ++ t
      in if not (null t) && last t == '"'
           then Right (init newAcc, ts)
           else collectString newAcc ts

-- Расчленение введённой строки по командам и числам/словам
parseCommand :: Dictionary -> String -> Either String Command
-- Если введённое значение число, помещаем его в стек
parseCommand _ s | Just n <- readMaybe s = Right (Push n)
parseCommand _ "+"     = Right Add
parseCommand _ "-"     = Right Subtract
parseCommand _ "*"     = Right Multiply
parseCommand _ "/"     = Right Divide
parseCommand _ "MOD"   = Right Modulo
parseCommand _ "DUP"   = Right Dup
parseCommand _ "DROP"  = Right Drop
parseCommand _ "SWAP"  = Right Swap
parseCommand _ "OVER"  = Right Over
parseCommand _ "ROT"   = Right Rot
parseCommand _ "="     = Right Equals
parseCommand _ "<"     = Right LessThan
parseCommand _ ">"     = Right GreaterThan
parseCommand _ "AND"   = Right And
parseCommand _ "OR"    = Right Or
parseCommand _ "INVERT"= Right Invert
parseCommand _ "."     = Right Print
parseCommand _ "CR"    = Right CR
parseCommand _ "EMIT"  = Right Emit
parseCommand _ "KEY"   = Right Key
-- Обработка строки (команда .")
parseCommand _ part | take 2 part == ".\"" =
  if length part > 2 && last part == '"'
    then Right (PrintString (drop 2 (init part)))
    else Left "Invalid string literal (unterminated)"
-- Если слово найдено в словаре, его надо выполнить
parseCommand dict name =
  if Map.member name dict
    then Right (Execute name)
    else Left ("Unknown word: " ++ name)

-- обработка новых слов
parseInput :: Dictionary -> String -> Either String [Command]
parseInput dict input = go dict (words input) []
  where
    go d [] acc = Right (reverse acc)
    -- Определение нового слова начинается с :
    go d (":":name:rest) acc =
      case parseDefinition rest of
        Left err -> Left err
        Right (def, remaining) ->
          go (Map.insert name def d) remaining (Define name def : acc)
    go d (part:rest) acc
      | take 2 part == ".\"" =
          case parseStringLiteral part rest of
            Left err -> Left err
            Right (str, newRest) ->
              go d newRest (PrintString str : acc)
      | otherwise =
          case parseCommand d part of
            Left err -> Left err
            Right cmd -> go d rest (cmd : acc)
    -- Разбор слова до ;
    parseDefinition :: [String] -> Either String ([Command], [String])
    parseDefinition parts = def parts []
      where
        def [] acc = Right (reverse acc, [])
        def (";":rest) acc = Right (reverse acc, rest)
        def (part:rest) acc
          | take 2 part == ".\"" =
              case parseStringLiteral part rest of
                Left err -> Left err
                Right (str, newRest) -> def newRest (PrintString str : acc)
          | otherwise =
              case parseCommand Map.empty part of
                Left err -> Left err
                Right cmd -> def rest (cmd : acc)

-- Основной цикл
main :: IO ()
main = do
  let loop stack dict = do
        putStrLn "Введите программу или 'exit' для выхода:"
        input <- getLine
        if input == "exit"
          then putStrLn "Завершение работы."
          else do
            case parseInput dict input of
              Left err -> putStrLn ("> " ++ err)
              Right cmds -> do
                (result, newStack, newDict) <- processProgram dict stack cmds
                putStrLn ("> " ++ show result)
                if result == Ok
                  then putStrLn $ "| " ++ unwords (map show (reverse newStack)) ++ " <- Top"
                  else return ()
                loop newStack newDict
  loop [] Map.empty
