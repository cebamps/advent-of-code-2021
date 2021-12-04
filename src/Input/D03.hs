module Input.D03 (input, testInput) where

input :: [[Char]]
input = lines inputString

testInput :: [[Char]]
testInput = lines testInputString

testInputString = "\
\00100\n\
\11110\n\
\10110\n\
\10111\n\
\10101\n\
\01111\n\
\00111\n\
\11100\n\
\10000\n\
\11001\n\
\00010\n\
\01010\n\
\"

inputString = "\
\111100000101\n\
\001110100010\n\
\101110110011\n\
\100000001101\n\
\001101010011\n\
\101111110000\n\
\011101110001\n\
\000000010111\n\
\011011000010\n\
\110110010000\n\
\011101000000\n\
\110010001001\n\
\101110100111\n\
\001001101111\n\
\101001011010\n\
\101001000001\n\
\101110111101\n\
\010100101100\n\
\001000110010\n\
\010100011110\n\
\100100000001\n\
\000110011000\n\
\110100100001\n\
\111000110000\n\
\000111100100\n\
\101100110100\n\
\101111101001\n\
\101110101001\n\
\010010110001\n\
\000111100001\n\
\001100001000\n\
\100011101001\n\
\100010111000\n\
\000001011000\n\
\001011110100\n\
\011110010001\n\
\110000011010\n\
\100011011111\n\
\001001110101\n\
\001010000101\n\
\100000010010\n\
\000011100010\n\
\101010110101\n\
\001110000101\n\
\000111101101\n\
\010100101010\n\
\101111101011\n\
\010001100000\n\
\110101100010\n\
\001001001111\n\
\001101011010\n\
\111010010101\n\
\110001000011\n\
\000011110100\n\
\100010111101\n\
\111100101110\n\
\110110001101\n\
\001000011011\n\
\101110101110\n\
\111011101110\n\
\010000110001\n\
\101111011111\n\
\000001001110\n\
\111101100011\n\
\000100000000\n\
\100001001111\n\
\110010000010\n\
\101111010011\n\
\010000101000\n\
\011010101111\n\
\110101101100\n\
\101101011100\n\
\100101111101\n\
\011101011110\n\
\111001001000\n\
\100111001101\n\
\101101000100\n\
\110111111010\n\
\011001101001\n\
\111101010110\n\
\001111001110\n\
\111100010111\n\
\000100111100\n\
\011100101001\n\
\111100111100\n\
\101000011100\n\
\011100111100\n\
\011010110010\n\
\101111101101\n\
\010111101010\n\
\001011000111\n\
\000010011111\n\
\101000100110\n\
\000100110101\n\
\100101001100\n\
\010100110110\n\
\011111100110\n\
\000000101011\n\
\111111101110\n\
\001011101101\n\
\000010001101\n\
\001100100100\n\
\101010010111\n\
\100010011001\n\
\010100000110\n\
\010000001101\n\
\010001101011\n\
\000111111001\n\
\101110010010\n\
\011100101000\n\
\010101010001\n\
\111011110111\n\
\000010110001\n\
\101111000111\n\
\001101110000\n\
\111111011011\n\
\100010011010\n\
\001011011010\n\
\110111001001\n\
\110101110100\n\
\101100111100\n\
\011110101010\n\
\010000001010\n\
\001111100001\n\
\000110000110\n\
\111010111010\n\
\101110100100\n\
\000001110010\n\
\110111001101\n\
\000001101000\n\
\000010001111\n\
\110000100011\n\
\000000001001\n\
\000111111110\n\
\111101111010\n\
\010110101010\n\
\110101011111\n\
\000111000110\n\
\111001100001\n\
\101111100000\n\
\011011010111\n\
\101011010111\n\
\110011101111\n\
\001110001111\n\
\010001100100\n\
\111001010000\n\
\100111000100\n\
\110101010100\n\
\000011111101\n\
\100010010000\n\
\110110100100\n\
\100111011110\n\
\101011001101\n\
\111011100110\n\
\100000010000\n\
\110000001111\n\
\001000000111\n\
\111011011011\n\
\101111101000\n\
\001110001010\n\
\100110000000\n\
\110001100110\n\
\011110100010\n\
\000001010110\n\
\010100000101\n\
\101010101010\n\
\001101000110\n\
\110100001111\n\
\110000001101\n\
\101101000000\n\
\101101101011\n\
\111001100101\n\
\101010001111\n\
\101101001101\n\
\010111101011\n\
\101000101101\n\
\001010000111\n\
\011110111001\n\
\001011101000\n\
\101101000010\n\
\110111111110\n\
\110001011011\n\
\101111101100\n\
\101110110101\n\
\101100111101\n\
\001110001101\n\
\010111000110\n\
\110101110110\n\
\101001101001\n\
\010100001000\n\
\011011111110\n\
\111010010001\n\
\101110100000\n\
\000101111100\n\
\001111110111\n\
\001010110011\n\
\010011011001\n\
\101111100001\n\
\100101111110\n\
\010001010011\n\
\001101110001\n\
\011100001000\n\
\101100000101\n\
\000111111101\n\
\010101110000\n\
\100110001100\n\
\010101111100\n\
\100101110100\n\
\110111000001\n\
\101110101000\n\
\000011010000\n\
\101011110101\n\
\100011110100\n\
\111100010110\n\
\110100011001\n\
\001001001100\n\
\010110111000\n\
\001000101111\n\
\111101001110\n\
\011110000011\n\
\000111000100\n\
\001101110101\n\
\110111011101\n\
\100000111001\n\
\100000001001\n\
\110110111011\n\
\010011000110\n\
\000100010101\n\
\100101010001\n\
\100100101100\n\
\010001001100\n\
\000010001010\n\
\101100101101\n\
\011011101111\n\
\100010111011\n\
\111111101111\n\
\110001100001\n\
\101011101111\n\
\000100011101\n\
\111000010001\n\
\000001011001\n\
\001101111101\n\
\001000000001\n\
\010100010100\n\
\110010111011\n\
\010100100011\n\
\001010001010\n\
\111111011010\n\
\100101010101\n\
\000111000010\n\
\011010100000\n\
\010100111111\n\
\001011100010\n\
\101010001011\n\
\111011010101\n\
\100100100111\n\
\101110101010\n\
\111100101001\n\
\111010001000\n\
\000100001111\n\
\011010001000\n\
\011011100100\n\
\110011011111\n\
\110111100010\n\
\000100001100\n\
\001001101101\n\
\110111110011\n\
\101111001001\n\
\101010001001\n\
\011011000111\n\
\000110001001\n\
\111011001010\n\
\000110000100\n\
\000000111011\n\
\010110110110\n\
\101000000000\n\
\011100101111\n\
\111101111000\n\
\001110011100\n\
\111110011001\n\
\000010111000\n\
\010111001100\n\
\010010111111\n\
\110101100111\n\
\000110111010\n\
\000001111110\n\
\111000000000\n\
\001110011101\n\
\100000111100\n\
\100010100001\n\
\010011101100\n\
\010001101010\n\
\001001101011\n\
\110100010100\n\
\011000000110\n\
\011000011001\n\
\010001101111\n\
\000101001111\n\
\011101111101\n\
\100010010011\n\
\100101111000\n\
\001101110111\n\
\001000000000\n\
\100100101011\n\
\111000100011\n\
\101100011011\n\
\000000101101\n\
\001110010011\n\
\001010010000\n\
\011000001110\n\
\101011111001\n\
\110101100000\n\
\011011111010\n\
\001000011111\n\
\011000100000\n\
\000010101100\n\
\111100010011\n\
\101010000011\n\
\001100111111\n\
\000001111000\n\
\111101110101\n\
\001010100111\n\
\011100111111\n\
\111010111000\n\
\100101011111\n\
\011100001101\n\
\001011000011\n\
\110001110110\n\
\000011101000\n\
\111110010111\n\
\100010101101\n\
\110001011101\n\
\011010100010\n\
\001110000011\n\
\101001101111\n\
\100110100100\n\
\011010000111\n\
\001111010011\n\
\110111101111\n\
\110101100101\n\
\100011010000\n\
\011110000100\n\
\010110100010\n\
\011000111001\n\
\011111011010\n\
\011110110101\n\
\011011110111\n\
\001111100101\n\
\101110100110\n\
\101010100000\n\
\110100001100\n\
\111010110101\n\
\011010001111\n\
\000010111110\n\
\100110010111\n\
\011100011011\n\
\100101100011\n\
\010110111100\n\
\010011110111\n\
\111001011101\n\
\000011111100\n\
\000111111111\n\
\101000111010\n\
\010110110011\n\
\100110101111\n\
\100010111010\n\
\111001111111\n\
\000110110011\n\
\010010111101\n\
\110001100011\n\
\111100001000\n\
\010011000011\n\
\011011101011\n\
\001010101101\n\
\100011010011\n\
\001100101110\n\
\111000111111\n\
\101001011101\n\
\111011010011\n\
\110101011101\n\
\001011010111\n\
\011001010100\n\
\010011010100\n\
\011100100110\n\
\101111110001\n\
\110111111101\n\
\111110100110\n\
\110001100000\n\
\010110010011\n\
\011100110001\n\
\010111111011\n\
\001010101000\n\
\010100101111\n\
\111101000001\n\
\100000111010\n\
\001010100000\n\
\000001011011\n\
\100111011101\n\
\100011110010\n\
\011111010000\n\
\111001010110\n\
\110111111100\n\
\010110100011\n\
\101111111010\n\
\110111110000\n\
\111100000110\n\
\111101011110\n\
\100101111100\n\
\111110001101\n\
\111110011110\n\
\010101100010\n\
\110101101101\n\
\111100001101\n\
\000100011100\n\
\010011110011\n\
\000100010100\n\
\000110110010\n\
\001010111000\n\
\110111001111\n\
\110101000101\n\
\100011110000\n\
\101010010010\n\
\111110000010\n\
\111011001011\n\
\010110111010\n\
\011101010100\n\
\010010101010\n\
\010011110001\n\
\010000010110\n\
\101111000011\n\
\111111010100\n\
\001111001001\n\
\101001100111\n\
\000011011111\n\
\100111000001\n\
\010111010110\n\
\010100011100\n\
\010100011101\n\
\001111110011\n\
\101100010111\n\
\101110100010\n\
\111101110100\n\
\100100100010\n\
\111010011011\n\
\010110000100\n\
\100100010100\n\
\101000011010\n\
\101001111101\n\
\010000010001\n\
\110111101101\n\
\000001110110\n\
\010001110100\n\
\100100110001\n\
\011111010111\n\
\100000000110\n\
\100001110110\n\
\000001011110\n\
\011000001100\n\
\011001110110\n\
\010010101110\n\
\111100111000\n\
\001010010011\n\
\000001000001\n\
\100010111001\n\
\010001011111\n\
\001110000000\n\
\000010000000\n\
\001101110011\n\
\010100011001\n\
\000001001010\n\
\110001000110\n\
\100010100101\n\
\011100010001\n\
\101100000110\n\
\001001100000\n\
\110100000000\n\
\000010100010\n\
\101101100111\n\
\100001111011\n\
\011000011101\n\
\110100110010\n\
\001011001101\n\
\000111001110\n\
\011100100100\n\
\100010000100\n\
\001101100000\n\
\100010011111\n\
\010101010111\n\
\000101010011\n\
\100011100000\n\
\100001011101\n\
\110011111100\n\
\111010011001\n\
\001011110111\n\
\011010010100\n\
\000011001110\n\
\010110111001\n\
\111000001001\n\
\011100111010\n\
\011111000010\n\
\101100111010\n\
\010100111101\n\
\111001100100\n\
\101111010110\n\
\110111110100\n\
\111110101010\n\
\010011001110\n\
\110101101001\n\
\111101011000\n\
\001010001101\n\
\101111000010\n\
\111110000111\n\
\110000001100\n\
\011111001101\n\
\101001000101\n\
\010111001011\n\
\001010011110\n\
\111010100100\n\
\110000101000\n\
\011101000011\n\
\010010100110\n\
\111010000001\n\
\111101000111\n\
\001010100101\n\
\101011001100\n\
\110010111001\n\
\111000111100\n\
\100101011101\n\
\110010101001\n\
\010010111000\n\
\010101001110\n\
\110000101111\n\
\001101001101\n\
\001000101011\n\
\001110110111\n\
\101111111011\n\
\111001100110\n\
\100101110000\n\
\010000011011\n\
\001010000000\n\
\011001010110\n\
\010101101010\n\
\100100000011\n\
\110010101110\n\
\110011101000\n\
\001010001000\n\
\010000101100\n\
\100110011110\n\
\001010001111\n\
\111010001101\n\
\100100010000\n\
\100100111001\n\
\111111010110\n\
\100010011101\n\
\110100100100\n\
\000110011101\n\
\000101101001\n\
\001011001100\n\
\101000000001\n\
\111101100000\n\
\110110111000\n\
\111111000010\n\
\001010111011\n\
\011011111000\n\
\010111000101\n\
\011100110011\n\
\111111100010\n\
\000000100010\n\
\001000011110\n\
\010001011001\n\
\111100111101\n\
\001111010010\n\
\010110011011\n\
\111110001110\n\
\101111100100\n\
\011100011101\n\
\111001111001\n\
\101001000100\n\
\001011000110\n\
\100011010101\n\
\001101100010\n\
\110111101110\n\
\010101111000\n\
\111010000111\n\
\000110010111\n\
\001100111101\n\
\100000101101\n\
\000110101000\n\
\011000011100\n\
\000101001110\n\
\011010101100\n\
\100010010100\n\
\001001111001\n\
\101100100000\n\
\010011011000\n\
\100110101011\n\
\110111110110\n\
\100111001100\n\
\111010010011\n\
\000100011110\n\
\011100011000\n\
\010000011001\n\
\100110011010\n\
\010000001000\n\
\101010011010\n\
\111111011100\n\
\010110100111\n\
\000110001101\n\
\001011111000\n\
\011010111001\n\
\111011011101\n\
\110000010110\n\
\100110111001\n\
\110010000001\n\
\110001011110\n\
\111100001010\n\
\101110111100\n\
\110011110100\n\
\101111011101\n\
\000101010010\n\
\001111000110\n\
\110010011010\n\
\000010010001\n\
\001110110110\n\
\111101100101\n\
\000000111101\n\
\011110000110\n\
\111011001101\n\
\110111000110\n\
\101100110101\n\
\010001001001\n\
\001010110101\n\
\101100011010\n\
\100011001010\n\
\001011101110\n\
\101000010110\n\
\100100111011\n\
\001011101100\n\
\111110111010\n\
\110011111010\n\
\000001111101\n\
\010000110111\n\
\101101101100\n\
\000110000000\n\
\111110110100\n\
\101100000010\n\
\011000000001\n\
\001000001001\n\
\111101111011\n\
\010100001110\n\
\111100101000\n\
\011000010111\n\
\001000111101\n\
\100000100101\n\
\110111000101\n\
\001111100000\n\
\101110111001\n\
\000100101111\n\
\101011010100\n\
\101111111001\n\
\000010000101\n\
\001100001111\n\
\101001011001\n\
\001110111000\n\
\010011011101\n\
\110110111001\n\
\011111010010\n\
\100111101011\n\
\101110011000\n\
\010011100110\n\
\101111000000\n\
\101000110111\n\
\001100000111\n\
\110001101111\n\
\010011101010\n\
\100100000111\n\
\100011111101\n\
\010111010010\n\
\001010111010\n\
\011111010110\n\
\101101100101\n\
\011110001000\n\
\000100111111\n\
\001010111100\n\
\101101100000\n\
\010010000011\n\
\010011010111\n\
\111000011110\n\
\110111010101\n\
\100100101000\n\
\101001001000\n\
\001111110100\n\
\111011110101\n\
\110001000100\n\
\100111111001\n\
\101010110100\n\
\010111101001\n\
\011011010010\n\
\011111101010\n\
\111100111110\n\
\110011101101\n\
\110110101101\n\
\110010100101\n\
\111110101111\n\
\000011110011\n\
\001100001011\n\
\111110110111\n\
\011011101100\n\
\110111101011\n\
\010111010000\n\
\101000010111\n\
\010100010001\n\
\111101100100\n\
\011101101010\n\
\010000110110\n\
\111001110000\n\
\111111000100\n\
\011001100000\n\
\111100101111\n\
\011001001011\n\
\110100100011\n\
\011001111000\n\
\101010001100\n\
\100010001110\n\
\010111110110\n\
\001011011111\n\
\000001011100\n\
\010010001111\n\
\011101011000\n\
\001010011010\n\
\100010011110\n\
\010011000111\n\
\101011011111\n\
\100101000010\n\
\110111010111\n\
\110111101010\n\
\010100001011\n\
\001001010011\n\
\001011001111\n\
\011000001111\n\
\010100000111\n\
\100011001111\n\
\110100111100\n\
\101110010000\n\
\000010110010\n\
\100010100010\n\
\001010011000\n\
\010101100101\n\
\101100010001\n\
\011011001010\n\
\000101100000\n\
\000101111010\n\
\001001110010\n\
\100011100001\n\
\011110111011\n\
\100000010111\n\
\100001001101\n\
\111000100010\n\
\011000011000\n\
\100001100010\n\
\000000110011\n\
\000101010111\n\
\000101000100\n\
\110111000111\n\
\001100000000\n\
\110100010110\n\
\101001000011\n\
\110111100100\n\
\000111100011\n\
\101110001001\n\
\000010011101\n\
\000011010111\n\
\111101011001\n\
\111010001111\n\
\000100110100\n\
\000011100001\n\
\100100111000\n\
\100111101110\n\
\001110101011\n\
\000111110100\n\
\110000010111\n\
\110010011111\n\
\000001111111\n\
\101011110011\n\
\101100001101\n\
\110101111011\n\
\011111000111\n\
\011110011011\n\
\001000010110\n\
\110110110100\n\
\010101001011\n\
\000100111000\n\
\011101111111\n\
\000111101001\n\
\110101111010\n\
\110000111001\n\
\011000111101\n\
\001101001010\n\
\110100110100\n\
\100110010001\n\
\100000000100\n\
\001100111000\n\
\100000001110\n\
\101000011000\n\
\100010110110\n\
\101011001001\n\
\010010001010\n\
\111011101100\n\
\100010100110\n\
\101101000111\n\
\100110110101\n\
\111001010100\n\
\101000110000\n\
\000101011010\n\
\010001010110\n\
\010111011110\n\
\001101011110\n\
\000100000101\n\
\000001011101\n\
\010010100000\n\
\110110010001\n\
\100111000111\n\
\100010110000\n\
\010110100000\n\
\110111111111\n\
\011111011111\n\
\001011010010\n\
\001111100011\n\
\100010110100\n\
\011010001011\n\
\110011001010\n\
\101110111000\n\
\111001010011\n\
\010011010101\n\
\011100100101\n\
\101010110011\n\
\001001100101\n\
\110010100000\n\
\011000111110\n\
\011101001110\n\
\110110000100\n\
\001111101100\n\
\101100010100\n\
\110001100101\n\
\111111010001\n\
\011001100001\n\
\110100000101\n\
\000000010000\n\
\110101000111\n\
\100100110100\n\
\010010110100\n\
\010010000100\n\
\011000111111\n\
\011001101010\n\
\011111101110\n\
\110110010100\n\
\110001001110\n\
\111111100011\n\
\010001100001\n\
\001001111011\n\
\000101110010\n\
\011111110111\n\
\000011000110\n\
\110101000001\n\
\101110000011\n\
\000000111010\n\
\000101110011\n\
\000011100111\n\
\010010010011\n\
\110011011010\n\
\111100100100\n\
\110010011101\n\
\100001001000\n\
\101000110001\n\
\101001010011\n\
\111000111101\n\
\101011111000\n\
\110011110110\n\
\111110100011\n\
\010110100100\n\
\010101000011\n\
\001010101100\n\
\101010101001\n\
\111100100000\n\
\001111011111\n\
\110101010011\n\
\101111010000\n\
\110011110010\n\
\110110110001\n\
\011010011110\n\
\100010010101\n\
\101000101010\n\
\010111010001\n\
\000001111100\n\
\001000011001\n\
\011110110001\n\
\110100011000\n\
\110000111110\n\
\001001011100\n\
\110110110101\n\
\011011100110\n\
\011110010100\n\
\111011110110\n\
\111110111001\n\
\111010010100\n\
\001000001011\n\
\111101010011\n\
\100111101010\n\
\010100001100\n\
\111101111111\n\
\000111111000\n\
\001010011101\n\
\100000001011\n\
\010010001110\n\
\011011011101\n\
\100000000111\n\
\100101100100\n\
\001110000010\n\
\000000000000\n\
\101000110100\n\
\011010110001\n\
\100001101110\n\
\110001110011\n\
\101100010000\n\
\001111111111\n\
\111110110101\n\
\010111001101\n\
\011001000001\n\
\111110011100\n\
\100100011010\n\
\001000011000\n\
\011101101100\n\
\100110111111\n\
\111011011010\n\
\101001101000\n\
\010011110000\n\
\110010110011\n\
\111000000001\n\
\101101110110\n\
\111000001101\n\
\011111001001\n\
\010010110101\n\
\001100010101\n\
\111011110010\n\
\100011000101\n\
\111010000100\n\
\011001111101\n\
\111010011110\n\
\111011111011\n\
\110110011010\n\
\011111011011\n\
\010010001100\n\
\110100111110\n\
\010100111011\n\
\110110011101\n\
\110011010000\n\
\110001001010\n\
\000011000111\n\
\100011101101\n\
\101010100010\n\
\110111001010\n\
\100101101110\n\
\111100101010\n\
\110001011001\n\
\011010011001\n\
\000001111010\n\
\001010101010\n\
\010001110000\n\
\001001111100\n\
\011110000010\n\
\000100010001\n\
\110100000110\n\
\010110111011\n\
\001100111100\n\
\111100011000\n\
\011011011010\n\
\111001001010\n\
\110110100111\n\
\001011000000\n\
\000001110001\n\
\101100011100\n\
\100011000011\n\
\001100010001\n\
\000100111011\n\
\001001110011\n\
\100111111100\n\
\111010001001\n\
\101001000110\n\
\001101000100\n\
\110000000100\n\
\111010110011\n\
\000101110000\n\
\010000010101\n\
\000110111001\n\
\100100100000\n\
\110010001011\n\
\110111100011\n\
\101101101001\n\
\100001100100\n\
\000111010100\n\
\"