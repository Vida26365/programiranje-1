PUSH 2
MOV A, 3

MOV B, 2

main:
    MOV C, A ;število ki se preverja
    DIV B
    MUL B ;ostankarček
    MOV D, A ; ostankarček
    MOV A, C ;število ki se preverja
    INC B
    CMP A, D ; preveri če je ostankarček enak številu
    JE ni_prastevilo
    CMP A, B
    JE dodaj_prastevilo
    JMP main
    

    
dodaj_prastevilo:
    PUSH A
    INC A
    MOV B, 2
    JMP main

ni_prastevilo:
    INC A
    MOV B, 2
    JMP main