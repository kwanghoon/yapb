
data D1 = C1_1 | C1_2 | Cn

;

data D2 = [a b] . C2_1 | C2_2 a b | C2_3 a -client-> b

;

data D3 = {l} . C3_1 | C3_2 Int String | C3_3 Int -l-> String

;

data D4 = {l} . [a b] . C4_1 | C4_2 a b | C4_3 a -l-> D4 {l} [a b]





