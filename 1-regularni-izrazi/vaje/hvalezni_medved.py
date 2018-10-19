###############################################################################
# Hvaležni medved
#
# Pri tej nalogi bomo napisali nekaj funkcij, ki nam bodo v pomoč pri analizi
# literarnih besedil, kot je na primer koroška narodna pripovedka *Hvaležni
# medved*.
###############################################################################

test_text = """Gori nekje v gorah, ne ve se več, ali je bilo pri Macigoju ali
Naravniku, je šivala gospodinja v senci pod drevesom in zibala otroka. Naenkrat
prilomasti - pa prej ni ničesar opazila - medved in ji moli taco, v kateri je
tičal velik, debel trn. Žena se je prestrašila, a medved le milo in pohlevno
godrnja. Zato se žena ojunači in mu izdere trn iz tace. Mrcina kosmata pa zvrne
zibel, jo pobaše in oddide. Čez nekaj časa pa ji zopet prinese zibel, a zvhano
napolnjeno s sladkimi hruškami . Postavil jo je na tla pred začudeno mater in
odracal nazaj v goščavo. "Poglej no", se je razveselila mati, "kakšen hvaležen
medved. Zvrhano zibelko sladkih hrušk mi je prinesel za en sam izdrt trn"."""

###############################################################################
# 1) Sestavite funkcijo [find_words], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in vsebujejo dan podniz.
#
# Namig: Pomagajte si z regex znakom za mejo [\b].
#
# >>> find_words(test_text, 'de')
# {'izdere', 'debel', 'oddide', 'začudeno'}
###############################################################################
def find_words(besedilo, niz):
    import re
    slovar = set()
    regularni =  r'\b\w*' + niz + r'\w*\b'
    seznam = re.findall(regularni, besedilo)
    for beseda in seznam:
        slovar.add(beseda)
    return slovar

ujemajoce_besede = find_words(test_text, 'de')

###############################################################################
# 2) Sestavite funkcijo [find_prefix], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in imajo dano predpono.
#
# >>> find_prefix(test_text, 'zi')
# {'zibala', 'zibel', 'zibelko'}
###############################################################################
def find_prefix(besedilo, predpona):
    import re
    slovar = set()
    regularni = r'(\b' + predpona + r'\w*\b)'
    seznam_ujemanj = re.findall(regularni, besedilo)
    for beseda in seznam_ujemanj:
        slovar.add(beseda)
    return slovar

besede_z_enako_predpono = find_prefix(test_text, 'zi')

###############################################################################
# 3) Sestavite funkcijo [find_suffix], ki vrne množico vseh besed, ki se
#    pojavijo v nizu in imajo dano pripono.
#
# >>> find_suffix(test_text, 'la')
# {'zibala', 'razveselila', 'prestrašila', 'šivala', 'opazila', 'tla'}
###############################################################################
def find_suffix(besedilo, pripona):
    import re
    slovar = set()
    regularni = r'\b\w*?' + pripona + r'\b'
    for beseda in re.findall(regularni, besedilo):
        slovar.add(beseda)
    return slovar

besede_z_enako_pripono = find_suffix(test_text, 'la')

###############################################################################
# 4) Sestavite funkcijo [double_letters], ki sprejme niz in vrne množico vseh
#    besed, ki vsebujejo podvojene črke.
#
# >>> double_letters('A volunteer is worth twenty pressed men.')
# {'volunteer', 'pressed'}
###############################################################################
def double_letters(niz):
    '''Funkcija mi vrne mnozico naborov v katerih je na prvem mestu beseda, na drugem pa črka ki se podvoji'''
    import re
    slovar = set()
    regularni = r'(\b\w*?(?P<dvojno>\w)(?P=dvojno)+\w*\b)'
    for beseda in re.findall(regularni, niz):
        slovar.add(beseda)
    return slovar

podvojene_crke = double_letters('A volunteer is worth twenty pressed men.')