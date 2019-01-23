from functools import lru_cache

# Nahrbtnik
# =========
#
# Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
# poljubne izdelke, katerih skupna masa ne presega k kilogramov. (Podelili so
# tri nagrade in sicer s parametrom k = 1, k = 2 in k = 5). Napišite funkcijo
# nahrbtnik(seznam_artiklov, k), ki poišče največjo ceno, ki jo lahko odnesemo
# iz trgovine. Naredite dve verziji: pri prvi lahko vzamemo samo en artikel iste
# vrste, pri drugi pa poljubno število artiklov iste vrste.

izdelki = [
	('jogurt', 0.39, 0.18),
	('mleko', 0.89, 1.03),
    ('kava', 2.19, 0.2),
    ('maslo', 1.49, 0.25),
    ('kvas', 0.22, 0.042),
    ('jajca', 2.39, 0.69),
    ('klobasa', 3.76, 0.50),
    ('čebula', 1.29, 2.0),
    ('kruh', 2.99, 1.0),
    ('Nutella', 4.99, 0.75),
    ('sok', 1.15, 2.0)
]




# Jajca
# =====
#
# Živimo v visoki stolpnici, ki ima n nadstropij. Imamo škatlo k jajc, ki so menda zelo trpežna,
# saj naj bi prenesla padce z višjih nadstropij stoplnice. Radi bi ugotovili, katero je najvišje
# nadstopje, pri katerem jajca še preživijo padec. Ker nimamo veliko časa, bi radi poiskali
# strategijo, pri kateri bomo minimizirali število metov.
#
# Razmislite:
#  * Kako moramo ravnati v primeru, ko imamo samo eno jajce?
#  * Kako lahko ravnamo v primeru, ko imamo na voljo zelo veliko jajc (več kot je število
#    nadstropij)?
#
# Napišite funkcij, ki bo izračunala maksimalno število metov (v najslabšem primeru), da ugotovimo
# številko kritičnega nadstropja, če imamo na voljo točko k jajc.



#  We are solving the problem of alternatingly colored towers. There are four
#  different types of building blocks, two of them blue and two red. The blue
#  blocks have heights 2 and 3 and the red ones 1 and 2.

#  Write the function [alternating_towers] for a given height calculates the
#  number of different towers of given height that we can build using alternatingly
#  colored blocks (red on blue, blue on red etc.). We may start with any color.

#  Hint: Use two mutually recursive auxilary functions using the keyword "and".
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#  # alternating_towers 10;;
#  - : int = 35

@ lru_cache(maxsize=None)
def stolpi(n, barva):
    if n == 0:
        return 1
    elif n < 0:
        return 0
    else:
        if barva == 'red':
            option1 = stolpi(n - 1, 'blue')
            option2 = stolpi(n - 2, 'red')
            return option1 + option2
        else:
            option3 = stolpi(n - 2, 'red')
            option4 = stolpi(n - 3, 'red')
            return option3 + option4


def alternajoci_stolpi(n):
    return stolpi(n, 'blue') + stolpi(n, 'red')

