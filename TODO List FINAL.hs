putTodo :: (Int, String) -> IO ()
putTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

-----------------------------------------------------
--В этой отведённой части есть ошибка, которая дублирует значение "Current TODO list"
--К сожалению, моих знаний недостаточно, чтобы понять, в чём именно проблема
showelems :: [String] -> IO ()
showelems elems = do
    putStrLn ""
    putStrLn "Current list:"
    mapM_ putTodo (zip [0..] elems)
    command <- getLine
    interpret command elems
-----------------------------------------------------

main :: IO () 
main = do
    putStrLn "TODO list in Haskell"
    putStrLn "+ <element> to add element"
    putStrLn "- <index> to remove element"
    putStrLn "q - to quit from this programm"
    showelems []

interpret :: String -> [String] -> IO ()
interpret ('+':' ':todo) elems = showelems (todo:elems)
interpret ('-':' ':num ) elems =
    case delete (read num) elems of
        Nothing -> do
            putStrLn "No TODO entry matches the given number"
            showelems elems
        Just elems' -> showelems elems'
interpret  "q"           elems = return ()
interpret  command       elems = do
    showelems elems

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    let n' = n - 1
    as' <- n' `seq` delete n' as
    return (a:as')
delete _  []    = Nothing

--Знаю, код взят из интернета и переделан под вариант, который мы делали на практиках.
--Я пытался хотя бы на его основе понять принцып работы с TODO листом и пытался убрать двойной показ
--строки "Current TODO list", но попытки были неудачными.