import Text.Read (readMaybe)
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import qualified Data.Map as Map
import Control.Monad (foldM)

-- Тип для представления стека
type Stack = [Int]

-- Тип результата выполнения
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
                -- сравнение
             | Equals        -- Сравнение - равно
             | LessThan      -- Сравнение - меньше чем
             | GreaterThan   -- Сравнение - больше чем
                -- алгебра логики
             | And           -- Булевское И
             | Or            -- Булевское ИЛИ
             | Invert        -- Булевское НЕ
                -- ввод-вывод
             | Print         -- Вывод строк
             | CR            -- Перевод строки
             | Emit          -- Вывод символа, код которого на вершине стека
             | Key           -- Ждём нажатие первой клавиши
             | Define String [Command] -- Объявление нового слова
             | Execute String -- Обработка нового слова
             | PrintString String -- Вывод строки
             deriving Show

-- Словарь для переменных, заданных пользователем
type Dictionary = Map.Map String [Command]

-- Получение нужной переменной
execute :: Dictionary -> Stack -> Command -> IO (Result, Stack, Dictionary)
execute dict stack (Push n) = return (Ok, n : stack, dict)

-- Арифметика
execute dict (x:y:stack) Add = return (Ok, (y + x) : stack, dict)
execute dict (x:y:stack) Subtract = return (Ok, (y - x) : stack, dict)
execute dict (x:y:stack) Multiply = return (Ok, (y * x) : stack, dict)
execute dict (x:y:stack) Divide
    | x == 0    = return (Error "Division by zero", stack, dict)
    | otherwise = return (Ok, (y `div` x) : stack, dict)
execute dict (x:y:stack) Modulo
    | x == 0    = return (Error "Modulo by zero", stack, dict)
    | otherwise = return (Ok, (y `mod` x) : stack, dict)

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

-- Вывод строки
execute dict stack (PrintString str) = do
    putStr str
    return (Ok, stack, dict)

-- Объявление новых слов
execute dict stack (Define name cmds) = return (Ok, stack, Map.insert name cmds dict)
execute dict stack (Execute name) = 
    case Map.lookup name dict of
        Just cmds -> do
            (result, newStack, newDict) <- processProgram dict stack cmds
            return (result, newStack, newDict)
        Nothing -> return (Error ("Unknown word: " ++ name), stack, dict)

execute _ _ _ = return (Error "Stack underflow", [], Map.empty)

-- Обработка программы
processProgram :: Dictionary -> Stack -> [Command] -> IO (Result, Stack, Dictionary)
processProgram dict stack = foldM runCommand (Ok, stack, dict)
  where
    runCommand (Ok, st, d) cmd = execute d st cmd
    runCommand (err, st, d) _  = return (err, st, d)

-- Разбор текста в команды
parseCommand :: Dictionary -> String -> Either String Command
parseCommand _ s | Just n <- readMaybe s = Right (Push n)
parseCommand _ "+" = Right Add
parseCommand _ "-" = Right Subtract
parseCommand _ "*" = Right Multiply
parseCommand _ "/" = Right Divide
parseCommand _ "MOD" = Right Modulo
parseCommand _ "DUP" = Right Dup
parseCommand _ "DROP" = Right Drop
parseCommand _ "SWAP" = Right Swap
parseCommand _ "OVER" = Right Over
parseCommand _ "ROT" = Right Rot
parseCommand _ "=" = Right Equals
parseCommand _ "<" = Right LessThan
parseCommand _ ">" = Right GreaterThan
parseCommand _ "AND" = Right And
parseCommand _ "OR" = Right Or
parseCommand _ "INVERT" = Right Invert
parseCommand _ "." = Right Print
parseCommand _ "CR" = Right CR
parseCommand _ "EMIT" = Right Emit
parseCommand _ "KEY" = Right Key
parseCommand _ ('.' : '"' : str) = Right (PrintString (init str))
parseCommand dict name = 
    if Map.member name dict 
    then Right (Execute name) 
    else Left ("Unknown word: " ++ name)
parseCommand _ name = Right (Execute name)

-- Основной цикл
main :: IO ()
main = do
    let loop stack dict = do
        putStrLn "Введите программу или 'exit' для выхода:"
        input <- getLine
        if input == "exit"
            then putStrLn "Завершение работы."
            else do
                let commands = parseInput dict input
                case commands of
                    Left err -> putStrLn ("> " ++ err)
                    Right cmds -> do
                        (result, newStack, newDict) <- processProgram dict stack cmds
                        putStrLn ("> " ++ show result)
                        if result == Ok then putStrLn $ "| " ++ unwords (map show (reverse newStack)) ++ " <- Top" else return ()
                        loop newStack newDict
    loop [] Map.empty

-- Разбор ввода
parseInput :: Dictionary -> String -> Either String [Command]
parseInput dict input = parseWords dict (words input) []
  where
    parseWords _ [] acc = Right (reverse acc)
    parseWords dict (":":name:rest) acc =
        let (def, remaining) = parseDefinition rest []
        in parseWords (Map.insert name def dict) remaining (Define name def : acc)
    parseWords dict (word:rest) acc = case parseCommand dict word of
        Left err -> Left err
        Right cmd -> parseWords dict rest (cmd : acc)

    -- Разбор определения нового слова (до ;)
    parseDefinition (";":rest) acc = (reverse acc, rest)
    parseDefinition (('.' : '"' : str):rest) acc = 
        parseDefinition rest (PrintString (init str) : acc)
    parseDefinition (word:rest) acc = case parseCommand Map.empty word of
        Left err -> ([], [])  
        Right cmd -> parseDefinition rest (cmd : acc)
    parseDefinition [] acc = (reverse acc, [])