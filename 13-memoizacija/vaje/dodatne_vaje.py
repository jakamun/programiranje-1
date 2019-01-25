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


def nahrbtnik_unique(seznam_artiklov, k):
    if len(seznam_artiklov) == 0:
        return 0
    else:
        for _, cena, teza in seznam_artiklov:
            if (k - teza) <= 0:
                return nahrbtnik_unique(seznam_artiklov[1:], k)
            else:
                moznost1 = nahrbtnik_unique(seznam_artiklov[1:], k)
                moznost2 = cena + nahrbtnik_unique(seznam_artiklov[1:], k - teza)
                return max(moznost1, moznost2)


# Jajca
# =====
###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################


def najdaljse_narascajoce_podzaporedje(sez):
    return None

###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
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
    return stolpi(n, 'red') + stolpi(n, 'blue')

# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):
    return None
