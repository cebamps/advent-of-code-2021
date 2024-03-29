module D04.Input (input, testInput) where

type Input = ([Int], [[Int]])

input = parseInput inputString1 inputString2

testInput = parseInput testString1 testString2

parseInput :: String -> String -> Input
parseInput str1 str2 =
  ( read <$> splitOn (== ',') str1,
    fmap read <$> rows str2
  )

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn pred = go
  where
    safeTail [] = []
    safeTail (_ : xs) = xs
    readItem xs = let (ls, rs) = break pred xs in (ls, safeTail rs)
    go [] = []
    go xs = case readItem xs of
      (ls, rs) -> ls : go rs

rows :: String -> [[String]]
rows = fmap splitRow . filter (not . null) . splitOn (== '\n')
  where
    splitRow = filter (not . null) . splitOn (== ' ')

---

testString1 = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"

testString2 =
  "\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7\n\
  \"

inputString1 = "31,88,35,24,46,48,95,42,18,43,71,32,92,62,97,63,50,2,60,58,74,66,15,87,57,34,14,3,54,93,75,22,45,10,56,12,83,30,8,76,1,78,82,39,98,37,19,26,81,64,55,41,16,4,72,5,52,80,84,67,21,86,23,91,0,68,36,13,44,20,69,40,90,96,27,77,38,49,94,47,9,65,28,59,79,6,29,61,53,11,17,73,99,25,89,51,7,33,85,70"

inputString2 =
  "\
  \50 83  3 31 16\n\
  \47  9 94 10 86\n\
  \61 22 53 46 74\n\
  \77 41 79 55 62\n\
  \97 78 43 73 40\n\
  \\n\
  \99 96 20 35 21\n\
  \38 17 48 69 68\n\
  \ 9 51 32 52 11\n\
  \67  8 42 89 27\n\
  \39 62 66 72 43\n\
  \\n\
  \33 16  4 78 31\n\
  \96 66 13 55 18\n\
  \47 89 83 99 85\n\
  \50 43 39 34 98\n\
  \81 65  7 23 17\n\
  \\n\
  \24 13 57 84 50\n\
  \83 86 98 92  7\n\
  \28 31 85 21 12\n\
  \37 48 43 47 67\n\
  \19 27  1 20 16\n\
  \\n\
  \38 75  3 14  4\n\
  \ 8 86 98 94 83\n\
  \60 46 63 85 20\n\
  \69 26 73 40 29\n\
  \48 84 33 18 74\n\
  \\n\
  \13 33 37 45 22\n\
  \19 28 61 58 69\n\
  \42 14 23 39 88\n\
  \92 81 54 99 52\n\
  \57  3 34 29 62\n\
  \\n\
  \19 71 46 13 81\n\
  \99 34  8  7 89\n\
  \72 56 38 22 27\n\
  \52  2 44 12  4\n\
  \53 86 45 95 39\n\
  \\n\
  \67 12 16 60 47\n\
  \79 21 99 15 59\n\
  \81 13 64 83  4\n\
  \85 48 17 29 66\n\
  \41 97 80 51 68\n\
  \\n\
  \72 19 67  6  9\n\
  \63 80 78 97 43\n\
  \53 73 91 44 47\n\
  \ 3 54 41 61 70\n\
  \69 36 57 55 45\n\
  \\n\
  \97  7 39 48 10\n\
  \77 42 65 89 79\n\
  \24 58 23 37 15\n\
  \26 71 41 18 87\n\
  \50 88 98 43  1\n\
  \\n\
  \76 50 48 10 77\n\
  \27 13 18 35 24\n\
  \31 72 41 64  2\n\
  \16 43 36 81 26\n\
  \66 51 30 34 74\n\
  \\n\
  \93 99 19 72 58\n\
  \ 7 76 80 94 23\n\
  \87 59 30 77 49\n\
  \53 88 51  4 36\n\
  \90 38 64 70 46\n\
  \\n\
  \36 38 45 13 68\n\
  \12 35 57 64 29\n\
  \71 74 15  0 49\n\
  \77 21 27 84 65\n\
  \22 23 60 17 10\n\
  \\n\
  \84 31 99 93 98\n\
  \71 73 48 38 83\n\
  \12 74 34 57 45\n\
  \ 2  9 76 79 77\n\
  \ 0 51 72 33 29\n\
  \\n\
  \65 43 15 53 89\n\
  \75 55 99 59 48\n\
  \ 6 85 68 30 39\n\
  \ 5  0 47 81 95\n\
  \96 31 23 87 73\n\
  \\n\
  \23 88 98 43 56\n\
  \ 4 89 53 34 41\n\
  \33 37 24 27 19\n\
  \22 83 72 75 31\n\
  \68 95 77  1 49\n\
  \\n\
  \14 48 20 73 11\n\
  \27  1  5 61 60\n\
  \96 99  9 64 29\n\
  \42 92 59 95 81\n\
  \69 97 78 86 16\n\
  \\n\
  \69 10 41 13 90\n\
  \75 95 99 72 29\n\
  \ 7 85 42 77 16\n\
  \88 19  2 45 64\n\
  \14  0 83 43 70\n\
  \\n\
  \74 85 45 50  6\n\
  \92 19 43 97 65\n\
  \56 11 77  5 28\n\
  \16 10 54 44 63\n\
  \ 3 93 75 12 51\n\
  \\n\
  \40 43 92 21 90\n\
  \ 5 62 74 34 25\n\
  \88 47 65 37 83\n\
  \15  6 10 99 89\n\
  \78 56 35 75  9\n\
  \\n\
  \96 74 41 81 31\n\
  \35 94 48 44 21\n\
  \99 11 32 15 43\n\
  \91 34 85 23  7\n\
  \54 77 89 13 26\n\
  \\n\
  \18 58 19 28 69\n\
  \74 41 91 88 15\n\
  \11 86 44 99 45\n\
  \79 93 80 55  4\n\
  \70 56 37 84 78\n\
  \\n\
  \48 95 57 70 84\n\
  \31 73 35 77 68\n\
  \ 4 53 32 63 13\n\
  \46  3 71 88 37\n\
  \72 65 36 50 49\n\
  \\n\
  \75  9 36 94  3\n\
  \71 62 65  0 15\n\
  \18 31 57 35 38\n\
  \ 6 16 22 34 95\n\
  \66 29 52 73 68\n\
  \\n\
  \83 54 24 26 96\n\
  \36  2  3 34 95\n\
  \16 77 11 56 91\n\
  \80 10 93 42 59\n\
  \88 47 76 55 79\n\
  \\n\
  \51 76  4 75  7\n\
  \17 98 78 12 66\n\
  \ 1 31 52 30 45\n\
  \74 29  6 87 90\n\
  \32  9 88 13 34\n\
  \\n\
  \97 92 40 73 76\n\
  \21 15 34 35 45\n\
  \ 1 27 48 78 46\n\
  \95 43 17 16 20\n\
  \62 28 52 56 68\n\
  \\n\
  \16 86 55 23 30\n\
  \20 73 83 89 35\n\
  \42 38 87 59 69\n\
  \ 3 79 85 43 78\n\
  \84 19 18 17 33\n\
  \\n\
  \10 43  3 68 56\n\
  \16 52 45 77 25\n\
  \75 73 66 46 82\n\
  \41 80 99 11 93\n\
  \71 79 37  5 84\n\
  \\n\
  \ 9 76 96 14 52\n\
  \67 74 86 32  4\n\
  \ 6 28 31 27 23\n\
  \56 58 25 69 38\n\
  \82 91 26 15 57\n\
  \\n\
  \96 62 34 67 53\n\
  \99  5 27 45 63\n\
  \80 38  0 71 43\n\
  \75 49 33 36  2\n\
  \15 21 54 20 81\n\
  \\n\
  \96 59 72  6 38\n\
  \60 70 76 82 46\n\
  \47 53 51 64 98\n\
  \44 25 69 81 33\n\
  \73 52 10 74 55\n\
  \\n\
  \52 25 99 11 60\n\
  \56 63 39 43  2\n\
  \34 45 59  8 30\n\
  \51 92 90 86 98\n\
  \19 80 47 69 13\n\
  \\n\
  \11 98 55  6 39\n\
  \70 26 99 57 75\n\
  \52 41 81  3  5\n\
  \96 92 94 35 46\n\
  \24 78 40 58 95\n\
  \\n\
  \81 87 93 88 29\n\
  \61  2 11 72 31\n\
  \60 76 19 36 58\n\
  \71 43 69 94 45\n\
  \99  9 62 48 30\n\
  \\n\
  \84 87 15 67 54\n\
  \13 81 97  8 92\n\
  \43 60  5 19  0\n\
  \91 20 69  1 29\n\
  \23  7 74 28 53\n\
  \\n\
  \73 68 24 64 47\n\
  \81 35 23 95 39\n\
  \51 69 94 37 21\n\
  \97 48 66 91 55\n\
  \56 18 49  9 86\n\
  \\n\
  \67 96 91 73 44\n\
  \77 10 50 81 19\n\
  \63 55 46 95 97\n\
  \ 8 69 40 70 61\n\
  \31 20 92 98 72\n\
  \\n\
  \36 81 69 98 59\n\
  \39 15 96  9 23\n\
  \14 84 88 89 90\n\
  \45 34 22 64 50\n\
  \86 32 53 77 55\n\
  \\n\
  \20 62 23 29 77\n\
  \13  0 14 92 42\n\
  \ 5 88  8  1 16\n\
  \80 79 84 49 40\n\
  \46 96 71 76 25\n\
  \\n\
  \17 65 37  3 35\n\
  \23 22 95 91 36\n\
  \61 11 51 64 85\n\
  \81 75 53 88 62\n\
  \59 14 29 73 57\n\
  \\n\
  \91 87 11 35 98\n\
  \34  1 28 27 10\n\
  \92 40 64 24 43\n\
  \55 49 42  0 36\n\
  \93 19 45 21 71\n\
  \\n\
  \82 86 14 10 43\n\
  \44 87 62 85 38\n\
  \31 67  3 68 64\n\
  \56 36 79 78 58\n\
  \21 95 35 90 18\n\
  \\n\
  \90 60 20 27 80\n\
  \39 30 12 83 96\n\
  \49  4 11 98 76\n\
  \74 37 54 26 19\n\
  \35 43 92 62 34\n\
  \\n\
  \36 23 45 24 63\n\
  \66 34 32 67 30\n\
  \26  0  5 69 50\n\
  \21 80 96 38 93\n\
  \49 46 61 41 16\n\
  \\n\
  \52 97 64 34 74\n\
  \28 46 31 56 75\n\
  \44 35 63 77  8\n\
  \ 7 68 71 18 38\n\
  \61 91 49 26 15\n\
  \\n\
  \83 80 10 38 45\n\
  \81 99 30  3 63\n\
  \57 96 82 55 76\n\
  \75 41 86 94 46\n\
  \59 42 40 68 48\n\
  \\n\
  \48 43 92 50 21\n\
  \37 56  8 38 94\n\
  \73 74 35  3 52\n\
  \ 7 29 82 98 86\n\
  \57 79 22  1 14\n\
  \\n\
  \53 46  4 76 28\n\
  \30 80 13 69 86\n\
  \54 70 40 77 71\n\
  \58 24 59 37 91\n\
  \45 51 43 90 74\n\
  \\n\
  \ 5 33 59 78 84\n\
  \ 1 90 49 72 27\n\
  \76 12 31 86 11\n\
  \74 18 52 47 19\n\
  \17 16 34 25 82\n\
  \\n\
  \41 42 21 31 44\n\
  \70 10  8 16 55\n\
  \82 60 77 89 43\n\
  \38  4 58 90 94\n\
  \74 71 93 88 61\n\
  \\n\
  \60 95 12 74 56\n\
  \82  3 48 22 27\n\
  \67 49  4 42 39\n\
  \18 35 43 87 45\n\
  \76 63 54 21 19\n\
  \\n\
  \35 89 76 86 32\n\
  \49  9  0 91 99\n\
  \87 26 97 22 44\n\
  \21 19 48 84 33\n\
  \98 30 50 90 53\n\
  \\n\
  \62 77  8 16 96\n\
  \73 65 39 79 78\n\
  \12 55 86 99 60\n\
  \ 9 22 71 98  2\n\
  \24 70 75 50 41\n\
  \\n\
  \46 55 77 38 26\n\
  \70 19 72 88 23\n\
  \91 84 56 51 99\n\
  \49 69 90 48 14\n\
  \93 76 63 92 71\n\
  \\n\
  \16 76 31 17 24\n\
  \14 95 34 12 75\n\
  \37 50 74 73 41\n\
  \68 56 58 23 84\n\
  \63 26 55 15 54\n\
  \\n\
  \35 65 20 19 61\n\
  \56  3 40 66 26\n\
  \36 44 13 18 78\n\
  \ 8 12  9 48 51\n\
  \ 0 93 53 71 95\n\
  \\n\
  \ 6 63  5 47 48\n\
  \81 86 43 73 69\n\
  \55 83 36  4 33\n\
  \23 96 88 38 32\n\
  \52 85 60 53  2\n\
  \\n\
  \27 88 14 49 89\n\
  \17 75 34 87 96\n\
  \76 48 95 60 98\n\
  \46 22 29 30  6\n\
  \ 3 94 63 77 83\n\
  \\n\
  \63 98 18 73 80\n\
  \37 56 95 60 53\n\
  \ 6 97 59 17 55\n\
  \20 74 24 96 79\n\
  \19 31 61  0 38\n\
  \\n\
  \93 52 54 25 51\n\
  \97 94 76 31 82\n\
  \53 74 87 65 89\n\
  \22 62 92 15 73\n\
  \17 95  1 32 43\n\
  \\n\
  \ 5 44 76 22 33\n\
  \16 91 48 42 29\n\
  \10 13 25 69 51\n\
  \97  7 64 60 88\n\
  \32 86 74 39 68\n\
  \\n\
  \15 60 30 58 32\n\
  \ 2 92 49 70  1\n\
  \29 90 85 93 59\n\
  \88 95 61 55 57\n\
  \19  8 97 10 45\n\
  \\n\
  \49 83 66 38 97\n\
  \68 81 69 92 47\n\
  \70 32 98  4 63\n\
  \37 25 84 80 54\n\
  \31 56 51 74 57\n\
  \\n\
  \86 75 61 68 26\n\
  \82 81 25 69 44\n\
  \62 70 23 37 43\n\
  \29 98 39 54 33\n\
  \87 93 15 79 58\n\
  \\n\
  \50 54 78 51 91\n\
  \71 70 27 28 76\n\
  \49  1 48 11 83\n\
  \98  4 56 86 67\n\
  \44 23 16 17 94\n\
  \\n\
  \84 78  3 44 96\n\
  \59 86 70 80 48\n\
  \93 88 52 43 61\n\
  \95 66 46 62 58\n\
  \ 5 25  6 85 99\n\
  \\n\
  \66 40 33 10 52\n\
  \38 30 99 79 60\n\
  \75 72 59  2 53\n\
  \20 83 43 76 44\n\
  \48 46 63 15 84\n\
  \\n\
  \54 80 53 36 95\n\
  \59 41  5 82 52\n\
  \55 56 22 33 15\n\
  \37 10 81 79 27\n\
  \42 98 83 23 28\n\
  \\n\
  \94 26 80 60 62\n\
  \91 57 58 59 39\n\
  \38 29 41 86 88\n\
  \11 46 66 73 95\n\
  \78 63 12 40 89\n\
  \\n\
  \57 77 46 88 69\n\
  \45 89 71 43 35\n\
  \56 52 30 29  8\n\
  \68 39 64 66 28\n\
  \10 47 80  7 19\n\
  \\n\
  \57 37 63 90 88\n\
  \47 10 22 58 46\n\
  \95 71 24 60 23\n\
  \ 0 45 75 50 77\n\
  \73 26 36  7 79\n\
  \\n\
  \26 79 66 87 72\n\
  \94 29 17 57 81\n\
  \64 91 28 27 89\n\
  \95 25  4 31 86\n\
  \85 34  6 21 76\n\
  \\n\
  \70 35 89 57 42\n\
  \34 54 64 71 61\n\
  \11 97 92 22 10\n\
  \ 0 81 78  7 53\n\
  \63 65 39  2 25\n\
  \\n\
  \11  5 24 28 10\n\
  \63 35 69 49 65\n\
  \42  4 60 57  6\n\
  \ 1  2 22 81 66\n\
  \70  9 86 50 64\n\
  \\n\
  \ 9 73 85  6 43\n\
  \74 24 30 76 89\n\
  \38 67 60 42 78\n\
  \34 22 20 69 92\n\
  \71 79 35 17  0\n\
  \\n\
  \66 61 87 49  7\n\
  \60 25 39 69 27\n\
  \41 76 59 95 45\n\
  \16 99 64 34  1\n\
  \74 62  9 75 18\n\
  \\n\
  \24 15 47 80  0\n\
  \99 92 29 67 64\n\
  \94 27 85 97 19\n\
  \55 75 46 91 52\n\
  \32  8 76 61 14\n\
  \\n\
  \95 10 21 53 63\n\
  \94 90 56 13 71\n\
  \76 42 17 35 65\n\
  \31 29 57  8 64\n\
  \77 30 16 79 61\n\
  \\n\
  \85 57  3 67 31\n\
  \62 46 55 63 18\n\
  \95 37 71  0 24\n\
  \23 32 12 96 89\n\
  \29 17 79 82  6\n\
  \\n\
  \21 53 44 78 99\n\
  \73 98 85 41  8\n\
  \39 19 28 27 81\n\
  \75 38 37 74 66\n\
  \47 46  6 29 14\n\
  \\n\
  \58 13 76 91 23\n\
  \ 1 99 81 69 86\n\
  \45 36 22 53 16\n\
  \30 71 89 18 49\n\
  \87 95 60 75 98\n\
  \\n\
  \30 61 64 54 80\n\
  \22 47 84 16  8\n\
  \83 18 65 70 11\n\
  \81 23 98 26 82\n\
  \45 69  6 53 68\n\
  \\n\
  \38 29 43 78 85\n\
  \67 39 99 98 52\n\
  \76 82 51  3 72\n\
  \46 19 65 93 34\n\
  \90  0  7 20 74\n\
  \\n\
  \85  6 67 50 45\n\
  \75 79 32  2 94\n\
  \22 60 95 34 78\n\
  \90  3 58 98 61\n\
  \63 26 76 42 89\n\
  \\n\
  \28 64 47 36  5\n\
  \76 41 26 79 10\n\
  \14 56 92 95 22\n\
  \32 54 13 98 19\n\
  \45 11 69 71 20\n\
  \\n\
  \90 46 64 38 73\n\
  \48 49 28 45 98\n\
  \77 30 35 81 78\n\
  \32 92 19 34 12\n\
  \69 74  6 89 61\n\
  \\n\
  \36 10 29 33 37\n\
  \64  7 81 31 79\n\
  \56 15 28 51 78\n\
  \ 2 92 50  9 23\n\
  \48 73 32  4 39\n\
  \\n\
  \86 82 78 41 21\n\
  \22 66 65  0 47\n\
  \46 43 29 77 45\n\
  \37 88 49 90 19\n\
  \40 10 96 13 38\n\
  \\n\
  \96 30 45 80 77\n\
  \27 82 83 64 22\n\
  \24 56 11 20 51\n\
  \55 54  2 59 14\n\
  \76 67 90 93 46\n\
  \\n\
  \11 50 90 29 33\n\
  \92 81  8 19 47\n\
  \25 66 74 22 73\n\
  \28  3 97 40 67\n\
  \53 71 48 49 57\n\
  \\n\
  \26 78 35 27 66\n\
  \98 10 88 43 86\n\
  \93 30 75 46 56\n\
  \23 92 34  4 85\n\
  \28 38 42  3 39\n\
  \\n\
  \28 96 83 99 97\n\
  \61 41 73 48 23\n\
  \44  7 89 49 60\n\
  \39 76 85 26  9\n\
  \82 53 98  2 15\n\
  \\n\
  \84 57 27 91 69\n\
  \20 43 13  9 61\n\
  \28 18 17 71  6\n\
  \48 58 55 96 24\n\
  \56 95 34 33 15\n\
  \\n\
  \24 49 88 55 75\n\
  \39 95 59 80 51\n\
  \35  0 56  7 25\n\
  \ 9  1 77 64 18\n\
  \50 34 54 57 99\n\
  \\n\
  \60 78 56 14 90\n\
  \44 30 48 15 12\n\
  \22 54  2 33 79\n\
  \34  4 76 93 29\n\
  \38 58 35 18  5\n\
  \\n\
  \81 22  3 41 80\n\
  \ 0 77 72 87 30\n\
  \97 99 38 69 13\n\
  \91 71 24 56  9\n\
  \36 44 21 79 53\n\
  \\n\
  \88 31 62 15 77\n\
  \25 39 37 53 20\n\
  \44  0 48  4 47\n\
  \29 73 49  8 72\n\
  \68 79 84 56 41\n\
  \\n\
  \86 48 70 56 67\n\
  \68  7 73 55 10\n\
  \38 82 65 22 62\n\
  \51  2 34 17 53\n\
  \47  0 28 39 83\n\
  \\n\
  \27 18 39  0 48\n\
  \84 74 64 80 60\n\
  \28 96 37 65 57\n\
  \53 79 89 32 14\n\
  \55 63 50  7 62\n\
  \"
