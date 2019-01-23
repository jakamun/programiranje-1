##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################


def pivot(a, start, end):
    i = start
    j = end
    pivot = a[start]
    while i < j:
        if pivot >= a[i]:
            i += 1
        elif pivot < a[j]:
            j -= 1
        else:
            t = a[i]
            a[i] = a[j]
            a[j] = t
    indeks = i if a[i] < pivot else i - 1
    a[start] = a[indeks]
    a[indeks] = pivot
    return indeks

##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################


def quicksort_part(a, start, end):
    if start >= end:
        return
    else:
        pivot_i = pivot(a, start, end)
        quicksort_part(a, start, pivot_i - 1)
        quicksort_part(a, pivot_i + 1, end)
        return


def quicksort(a):
    quicksort_part(a, 0, len(a) - 1)
    return a


def hitro_uredi(a):
    def pomozna(i, j):
        if i >= j:
            return
        else:
            p = pivot(a, i, j)
            pomozna(i, p - 1)
            pomozna(p + 1, j)
    return pomozna(0, len(a) - 1)

##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################


def enostaven_nacin(a, k):
    urejen_seznam = quicksort(a)
    return urejen_seznam[k]


def kth_el_part(a, k, start, end):
    if start > end:
        return None
    else:
        pivot_i = pivot(a, start, end)
        if pivot_i == k:
            return a[pivot_i]
        elif pivot_i > k:
            return kth_el_part(a, k, start, pivot_i - 1)
        else:
            return kth_el_part(a, k, pivot_i + 1, end)


def kth_element(a, k):
    if k > len(a):
        return None
    else:
        return kth_el_part(a, k, 0, len(a)-1)


def kti_element(a, k):
    def pomozna(i, j):
        if i >= j:
            return a[k]
        else:
            p = pivot(a, i, j)
            if p == k:
                return a[p]
            elif p < k:
                return pomozna(p + 1, j)
            else:
                return pomozna(i, p - 1)
    return pomozna(0, len(a) - 1)
