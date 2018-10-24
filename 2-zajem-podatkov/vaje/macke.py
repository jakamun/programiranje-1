import requests
import re
import os
import csv

###############################################################################
# Najprej definirajmo nekaj pomožnih orodij za pridobivanje podatkov s spleta.
###############################################################################

# definiratje URL glavne strani bolhe za oglase z mačkami
cats_frontpage_url = 'http://www.bolha.com/zivali/male-zivali/macke/'
# mapa, v katero bomo shranili podatke
cat_directory = 'macke'
# ime datoteke v katero bomo shranili glavno stran
frontpage_filename = 'frontpage.html'
# ime CSV datoteke v katero bomo shranili podatke
csv_filename = 'macke.csv'


def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    try:
        # del kode, ki morda sproži napako
        r = requests.get(url)
    except requests.exceptions.ConnectionError:
        # koda, ki se izvede pri napaki
        # dovolj je če izpišemo opozorilo in prekinemo izvajanje funkcije
        print('Could not access page' + url)
    # nadaljujemo s kodo če ni prišlo do napake
    return r.text

# print('Dolzina strani je {}.'.format(len(download_url_to_string(cats_frontpage_url))))

def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

# Definirajte funkcijo, ki prenese glavno stran in jo shrani v datoteko.

def save_frontpage(url):
    '''Save "cats_frontpage_url" to the file
    "cat_directory"/"frontpage_filename"'''
    spletna_stran = download_url_to_string(url)
    save_string_to_file(spletna_stran, r'C:\\Users\HP\Documents\Programiranje\Programiranje1\programiranje-1\2-zajem-podatkov\vaje', 'frontpage_macke.html')

###############################################################################
# Po pridobitvi podatkov jih želimo obdelati.
###############################################################################


def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.'''
    mapa = directory + '\\' + filename
    with open(mapa, encoding='UTF-8') as datoteka:
        return datoteka.read()

# Definirajte funkcijo, ki sprejme niz, ki predstavlja vsebino spletne strani,
# in ga razdeli na dele, kjer vsak del predstavlja en oglas. To storite s
# pomočjo regularnih izrazov, ki označujejo začetek in konec posameznega
# oglasa. Funkcija naj vrne seznam nizov.


def page_to_ads(absolutna_pot_datoteke):
    '''Split "page" to a list of advertisement blocks.'''
    directory, frontpage = os.path.split(absolutna_pot_datoteke)
    spletna_stran = read_file_to_string(directory, frontpage)
    vzorec = re.compile(r'<div class="coloumn image">') 
    odrezi = re.compile(r'<div class="clear">&nbsp;</div>')
    seznam_oglasov = vzorec.split(spletna_stran)
    odrezano = seznam_oglasov[-1]
    odrezano = odrezi.split(odrezano)
    seznam_oglasov[-1] = odrezano[0]
    return seznam_oglasov[1:]

# page_to_ads(r'c:\\Users\HP\Documents\Programiranje\Programiranje1\programiranje-1\2-zajem-podatkov\vaje\frontpage_macke.html')

# Definirajte funkcijo, ki sprejme niz, ki predstavlja oglas, in izlušči
# podatke o imenu, ceni in opisu v oglasu.


def get_dict_from_ad_block(oglas):
    '''Build a dictionary containing the name, description and price
    of an ad block.'''
    vzorec = re.compile(
        r'<h3><a title="(?P<ime>.+?)" href=".*?'
        r'</a></h3>\n\n\s+(?P<opis>.+?)\s+<div class=".*?">'
        r'\s*<div class="price">(<span>|)(?P<cena>.*?)(</span>|)</div>.*?',
        re.DOTALL)
    ujemanje = vzorec.search(oglas)
    slovar = ujemanje.groupdict()
    return slovar

# get_dict_from_ad_block(page_to_ads(r'c:\\Users\HP\Documents\Programiranje\Programiranje1\programiranje-1\2-zajem-podatkov\vaje\frontpage_macke.html')[0])

# Definirajte funkcijo, ki sprejme ime in lokacijo datoteke, ki vsebuje
# besedilo spletne strani, in vrne seznam slovarjev, ki vsebujejo podatke o
# vseh oglasih strani.


def ads_from_file(pot_do_datoteke):
    '''Parse the ads in filename/directory into a dictionary list.'''
    seznam_oglasov = page_to_ads(pot_do_datoteke)
    seznam_slovarjev = []
    for oglas in seznam_oglasov:
        seznam_slovarjev.append(get_dict_from_ad_block(oglas))
    return seznam_slovarjev

###############################################################################
# Obdelane podatke želimo sedaj shraniti.
###############################################################################


def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None

# Definirajte funkcijo, ki sprejme neprazen seznam slovarjev, ki predstavljajo
# podatke iz oglasa mačke, in zapiše vse podatke v csv datoteko. Imena za
# stolpce [fieldnames] pridobite iz slovarjev.


def write_cat_ads_to_csv(pot, datoteka, frontpage):
    spletna_stran = os.path.join(pot, frontpage)
    seznam_podatkov = ads_from_file(spletna_stran)
    fieldnames = ['ime', 'opis', 'cena']
    write_csv(fieldnames, seznam_podatkov, pot, datoteka)
    return None
