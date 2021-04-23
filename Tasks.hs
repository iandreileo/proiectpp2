{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

module Tasks where

import Dataset
import Text.Printf
import Data.List
type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
-- Operatia de impartire dintre 2 int
divide :: Integer -> Integer -> Float
divide a b = (fromIntegral a) / (fromIntegral b)

-- Operatia de impartire dintre float si int
divideFloat :: Float -> Integer -> Float
divideFloat a b = a / (fromIntegral b)

-- Auxiliar pentru stringtoint
readInt :: String -> Integer
readInt "" = 0
readInt x = read x

-- Cast de la string la int
stringtoint :: [String] -> [Integer]
stringtoint = map readInt

-- Cast de la String la float
stringtofloat :: String -> Float
stringtofloat "" = 0
stringtofloat x = read x

-- Functia pentru creearea unui rand
get_comprimed_row :: Row -> Row
get_comprimed_row [] = []
get_comprimed_row (r:rs) = r : [(printf "%.2f" ((divide (sum (stringtoint (drop 0 (take 6 rs)))) 4) + (stringtofloat (last rs))))]

-- Functia care itereaza prin randuri
get_completed_rows :: Table -> Table
get_completed_rows [] = []
get_completed_rows (t:ts) = get_comprimed_row t : get_completed_rows ts

-- Functia care creeaza tabelul
compute_exam_grades :: Table -> Table
compute_exam_grades (x:xs) = ["Nume", "Punctaj Exam"] : get_completed_rows xs

-- Task 2
-- Number of students who have passed the exam:
-- Testam daca nota unui student > 2.5
checkGrade :: Row -> Int
checkGrade [] = 0
checkGrade ["Nume", "Punctaj Exam"] = 0
checkGrade h = (if ((stringtofloat (last h)) > 2.5) then 1 else 0)

-- Iteram prin toti studentii
iterateRowsGrade :: Table -> Int
iterateRowsGrade [] = 0
iterateRowsGrade (r:rs) = checkGrade r + iterateRowsGrade rs

-- Numaram studentii care au nota de trecere
get_passed_students_num :: Table -> Int
get_passed_students_num [] = 0
get_passed_students_num table = iterateRowsGrade(compute_exam_grades table)

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage t = (divide (toInteger (get_passed_students_num t)) (toInteger(length t - 1)))

-- Sum of grades
-- Functia care primeste un rand si returneaza nota de pe randul respectiv
sumOfGradesAux :: Row -> Float
sumOfGradesAux [] = 0
sumOfGradesAux ["Nume", "Punctaj Exam"] = 0
sumOfGradesAux r = stringtofloat (last r)

--
-- Folosind functia de mai sus facem suma notelor
sumOfGradex :: Table -> Float
sumOfGradex [] = 0
sumOfGradex (h:t) = sumOfGradesAux h + sumOfGradex t

-- Average exam grade
-- Facem media folosind suma si numarul de studenti
get_exam_avg :: Table -> Float
get_exam_avg t = divideFloat (sumOfGradex (compute_exam_grades t)) (toInteger(length t - 1))

-- Cast de la String la Float
readFloat :: String -> Float
readFloat "" = 0
readFloat x = read x

-- Cast din lista de String in lista de Float
stringtofloatlist :: [String] -> [Float]
stringtofloatlist = map readFloat

-- Check Homework
-- Adunam suma notelor
-- Si returnam 1/0 in functie daca a trecut sa unu
hw_check :: Row -> Int
hw_check [] = 0
hw_check r = (if (sum (stringtofloatlist (drop 2 (take 5 r)))) >= 1.5 then 1 else 0)

-- Iterate Rows Homeworks
-- Iteram prin toti studentii si vedem cati au nota de trecere
hw_rows_iterate :: Table -> Int
hw_rows_iterate [] = 0
hw_rows_iterate (h:t) = hw_check h + hw_rows_iterate t

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num (h:t) = hw_rows_iterate t

-- Functie auxiliara prin care primi un rand si un intreg
-- Si impartim randul la intreg
dividelistbyvalue :: Integer -> [String] -> Row
dividelistbyvalue a [] = []
dividelistbyvalue a (x:xs) = printf "%.2f"(divideFloat (stringtofloat x) a) : dividelistbyvalue a xs

-- Functie prin care adunam 2 stringuri si returnam stringul rezultatului
sumOfTwoStrings :: String -> String -> String
sumOfTwoStrings a b = printf "%.2f"((stringtofloat a) + (stringtofloat b))

-- Functie prin care primim un rand si returnam un rand doar cu coloanele de care avem nevoie
format_qw_row :: Row -> Row
format_qw_row r = (drop 1 (take 7 r))

-- Format qs table
-- Formam un tabel doar cu coloanele de care avem nevoie
format_qs_table :: Table -> Table
format_qs_table [] = []
format_qs_table (h:t) = format_qw_row h : format_qs_table t

--
-- Folosind tabelul de mai sus doar cu coloanele de Qs facem pe fiecare coloana
-- suma Qs-urilor
sumOfRow_qs :: Table -> Row
sumOfRow_qs t = (foldr (zipWith sumOfTwoStrings) ["0","0","0","0","0","0"] (format_qs_table t))

-- Task 3
-- Functie prin care primim un tabel si returnam un tabel cu 2 randuri primul rand capul de tabel
-- si al doilea rand cu mediile raspunsurilor per intrebare
get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs (h:t) = ["Q1","Q2","Q3","Q4","Q5","Q6"] : [dividelistbyvalue (toInteger (length t)) (sumOfRow_qs t)]

-- Functie prin care primim un intreg si un rand si
-- returnam de cate ori se gaseste intregul in rand
return_count :: Integer -> Row -> Int
return_count a l = length (filter (==a) (stringtoint l))

-- Functie prin care formam un tabel cu cate raspunsuri de 0 1 2 sunt per fiecare intrebare
create_summary :: Table -> Table
create_summary [] = []
create_summary list = map(\curr -> [head curr, show (return_count 0 (tail curr)), show (return_count 1 (tail curr)), show (return_count 2 (tail curr))]) list

-- Task 4
-- Trimitem catre create_summary coloanele de care avem nevoie din transupsa matricei
-- pentru ca le putea calcula cate sunt pe fiecare col
get_exam_summary :: Table -> Table
get_exam_summary l = ["Q", "0", "1", "2"] : create_summary (drop 1 (take 7 (transpose l)))

-- Functie auxiliara de comparare
-- Care primeste 2 liste de string-uri si facem comparatia intre coloane
compare_aux l1 l2 = 
    if (stringtofloat(last l1) < stringtofloat(last l2)) then LT 
    else if (stringtofloat(last l1) == stringtofloat(last l2)) && ((head l1) < (head l2)) then LT
    else GT

-- Task 5
-- Folosindu-ne de functia de comparare folosim sortBy si sortam conform cerintei
get_ranking :: Table -> Table
get_ranking l = ["Nume", "Punctaj Exam"] : sortBy compare_aux (tail (compute_exam_grades l))

-- Functie prin care, unde avem numere de forma 4.4, adaugam un 0 ca sa uniformizam la 2 zecimale
addzero_tostring :: String -> String
addzero_tostring string = 
    if ((length string) == 3) then (string ++ "0")
    else string

-- Functie prin care realizam tabelul fara header cu informatiile necesare
calculate_diff :: Table -> Table
calculate_diff = map (\list -> [head list, printf "%.2f" (sum(stringtofloatlist (drop 1 (take 7 list))) / 4), addzero_tostring (last list), printf "%.2f" (abs ((sum(stringtofloatlist (drop 1 (take 7 list))) / 4) - (stringtofloat (last list))))])

-- Task 6
-- Formam cu calculate_diff tabelul si apoi il sortam folosind aceeasi functie auxiliara
get_exam_diff_table :: Table -> Table
get_exam_diff_table list = ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"] : (sortBy compare_aux (calculate_diff (drop 1 list)))


-- Task Sets 2
-- Read CSV
read_csv :: CSV -> Table
read_csv = undefined


-- Write CSV
write_csv :: Table -> CSV
write_csv = undefined




-- Task 1
as_list :: String -> Table -> [String]
as_list = undefined



-- Task 2
tsort :: String -> Table -> Table
tsort = undefined


-- Task 3
vmap :: (Value -> Value) -> Table -> Table
vmap = undefined
--- An example use of this would be:

-- Task 4
rmap :: (Row -> Row) -> [String] -> Table -> Table
get_hw_grade_total :: Row -> Row
get_hw_grade_total = undefined
rmap = undefined




-- Task 5
vunion :: Table -> Table -> Table
vunion = undefined


-- Task 6
hunion :: Table -> Table -> Table
hunion = undefined



-- Task 7
tjoin :: String -> Table -> Table -> Table
tjoin = undefined





-- Task 8
cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian = undefined






-- Task 9
projection :: [String] -> Table -> Table
projection = undefined