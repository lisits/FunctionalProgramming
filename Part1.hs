import Text.Read (readMaybe)

-- Тип для представления стека
type Stack = [Int]

-- Тип для представления результата выполнения
data Result = Ok | Error String deriving (Show, Eq)

-- Тип для представления команд
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
             deriving Show

-- Выполнение команды на основе текущего состояния стека
execute :: Stack -> Command -> (Result, Stack)
execute stack (Push n) = (Ok, n : stack)

-- Реализация команды сложения
execute (x:y:stack) Add = (Ok, (y + x) : stack)

-- Реализация команды вычитания
execute (x:y:stack) Subtract = (Ok, (y - x) : stack)

-- Реализация команды умножения
execute (x:y:stack) Multiply = (Ok, (y * x) : stack)

-- Реализация команды деления
execute (x:y:stack) Divide 
    | x == 0    = (Error "Division by zero", stack)
    | otherwise = (Ok, (y `div` x) : stack)

-- Реализация команды остатка от деления
execute (x:y:stack) Modulo
    | x == 0    = (Error "Modulo by zero", stack)
    | otherwise = (Ok, (correctModulo y x) : stack)
  where
    -- Функция для корректировки знака остатка
    correctModulo a b
      | a < 0 && result > 0 = result - b
      | a > 0 && result < 0 = result + b
      | otherwise = result
      where result = a `mod` b

-- Команды для манипулирования стеком

-- Реализация команды дублирования последнего элемента
execute stack (Dup) 
    | null stack = (Error "Stack underflow", stack)
    | otherwise  = (Ok, head stack : stack)

-- Реализация команды удаления последнего элемента
execute stack Drop 
    | null stack = (Error "Stack underflow", stack)
    | otherwise  = (Ok, tail stack)

-- Реализация команды смены мест двух последних чисел
execute (x:y:stack) Swap = (Ok, y : x : stack)

-- Реализация команды дублирования предпоследнего числа
execute (x:y:stack) Over = (Ok, y : x : y : stack)

-- Реализация команды смены мест трёх элементов
execute (x:y:z:stack) Rot = (Ok, z : x : y : stack)

execute _ _ = (Error "Stack underflow", [])


-- Обработка программы
processProgram :: [String] -> (Result, Stack)
processProgram = foldl runCommand (Ok, [])
  where
    runCommand (_, stack) word
      | Just n <- readMaybe word = execute stack (Push n)
      -- случаи работы с командами арифметики
      | word == "+" = execute stack Add
      | word == "-" = execute stack Subtract
      | word == "*" = execute stack Multiply
      | word == "/" = execute stack Divide
      | word == "MOD" = execute stack Modulo
      -- случаи работы с командами стека
      | word == "DUP" = execute stack Dup
      | word == "DROP" = execute stack Drop
      | word == "SWAP" = execute stack Swap
      | word == "OVER" = execute stack Over
      | word == "ROT" = execute stack Rot
      -- обработка не найденной команды
      | otherwise = (Error $ "Unknown command: " ++ word, stack)

-- Вывод стека
printStack :: Stack -> String
printStack stack = unwords (map show (reverse stack)) ++ " <- Top"

main :: IO ()
main = do
    -- Главный цикл программы, чтоб её можно было запустить и не закрывать
    let loop stack = do
        putStrLn "Введите программу (например: 1 2 3 +) или 'exit' для выхода:"
        input <- getLine  -- Чтение строки
        if input == "exit"
            then putStrLn "Завершение работы."
            else do
                let (result, newStack) = processProgram (words input)
                putStrLn ("> " ++ show result)  -- выводим результат
                if result == Ok -- Если ошибок не возикло, выводим стек, иначе не выводим
                    then putStrLn $ "| " ++ printStack newStack 
                    else return ()
                loop newStack  -- Рекурсивный вызов для продолжения работы с новым стеком

    -- Запуск цикла с пустым стеком
    loop []
