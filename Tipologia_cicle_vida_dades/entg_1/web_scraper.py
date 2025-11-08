# -*- coding: utf-8 -*-

"""
Script para descargar automáticamente imágenes de matrículas de Platesmania,
filtradas por país.
"""

import time
from bs4 import BeautifulSoup
import os
import csv
from seleniumbase import Driver
from seleniumbase import BaseCase
import time 



# Rango de páginas a recorrer (cada página ~10 imágenes según el sitio)
st_pg = 1
end_pg = 10

main_dir = "./data/"

BASE = "https://platesmania.com"
# Cabeceras para simular un navegador y evitar bloqueos básicos

driver = Driver(
        browser="chrome",
        uc=True,
        headless2=False,
        incognito=True,
        agent="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.5615.138 Safari/537.36 AVG/112.0.21002.139",
        do_not_track=True,
        undetectable=True
    )

def click_manage_options_if_exists(driver):
    """
    Detecta y clica el 'Consentir' cookie si existe.
    """
    selector = "button.fc-button.fc-cta-consent.fc-primary-button"
    try:
        if driver.is_element_present(selector):
            print("Found 'Consentir' button, clicking it...")
            driver.click(selector)
            time.sleep(1)
        else:
            print("No cookie options button found.")
    except Exception as e:
        print(f"Could not handle cookie button: {e}")

def get_countrys():
    """
    Devuelve una lista con todos los paises que dispone la pagina
    """
    

    url = BASE + "/countries"
    driver.get(url)
    time.sleep(10)
    click_manage_options_if_exists(driver)
    html = driver.page_source
    soup = BeautifulSoup(html, 'html.parser')
    countrys = [a.find_all("a", href=True)for a in soup.find_all("span", class_="lead")]
    countrys = [x for x in countrys if x]
    countrys = [x[0]['href'] for x in countrys]
    # Hay muchos paises podremos solo dos para no saturar el serivdor
    countrys = ['es', 'fr']
    return countrys

def get_plate_links(country, page):
    """
    Devuelve los enlaces 'nomer...' de una página de galería para un país.
    Retorna lista vacía si no existe el país o no se encuentra la galería.
    """
    url = BASE + "/{}/gallery.php?&start={}".format(country, page)
    driver.get(url)
    time.sleep(5)
    click_manage_options_if_exists(driver)
    html = driver.page_source
    soup = BeautifulSoup(html, 'html.parser')

    # Heurística sencilla para comprobar que el país/galería es válida
    if not soup.find(string=lambda t: "Has buscado:" in t):
        print("No se encontró el país")
        return []

    # Acotar el HTML a la columna principal donde está la galería
    main_col = soup.find("div", class_="col-md-9")
    if not main_col:
        print("No se encontró la galería principal")
        return []
    
    items = main_col.find_all("div", class_="col-sm-6")

    results = []

    for item in items:
        panel = item.find("div", class_="panel")
        if not panel:
            continue

        # Pais
        country_tag = panel.find("h3", class_="panel-title")
        country_name = country_tag.get_text(strip=True) if country_tag else None


        # Imagen principal y link a la entrada
        main_a = panel.find("div", class_="panel-body").find("a", href=True)
        main_link = main_a["href"] if main_a else None
        main_img = main_a.find("img")["src"] if main_a and main_a.find("img") else None

        # Vehicle name
        vehicle_tag = panel.find("h4", class_="text-center")
        vehicle_name = vehicle_tag.get_text(strip=True) if vehicle_tag else None

        # Vehicle generation/details
        gen_tag = panel.find("small").find("a")
        generation = gen_tag.get_text(strip=True) if gen_tag else None
        generation_link = gen_tag["href"] if gen_tag else None

        # License plate image and number
        plate_img = panel.find("div", class_="col-xs-offset-3 col-xs-6 text-center").find("img")
        plate_number = plate_img["alt"] if plate_img and plate_img["alt"] else None
        plate_img_url = plate_img["src"] if plate_img else None

        # Description line (location + details)
        desc_tag = panel.find_all("small")
        description = desc_tag[1].get_text(" ", strip=True) if len(desc_tag) > 1 else None

        # User info
        user_tag = panel.find("i", class_="fa-user")
        user_a = user_tag.find_next("a") if user_tag else None
        username = user_a.get_text(strip=True) if user_a else None
        user_link = user_a["href"] if user_a else None

        # Date/time
        date_tag = panel.find("i", class_="fa-clock-o")
        upload_time = date_tag.find_next(string=True).strip() if date_tag else None

        # Likes and comments
        likes_tag = panel.find("i", class_="fa-heart")
        likes = likes_tag.find_next(string=True).strip() if likes_tag else "0"
        comments_tag = panel.find("i", class_="fa-comments-o")
        comments = comments_tag.find_next(string=True).strip() if comments_tag else "0"

        results.append({
            "country": country_name,
            "vehicle": vehicle_name,
            "generation": generation,
            "generation_link": generation_link,
            "main_link": main_link,
            "main_img": main_img,
            "plate_number": plate_number,
            "plate_img": plate_img_url,
            "description": description,
            "username": username,
            "user_link": user_link,
            "upload_time": upload_time,
            "likes": likes,
            "comments": comments
        })
        
    return results




try:
    # Carpeta de destino por país (p.ej. C:\...\data\es)
    countrys = get_countrys()
    for country in countrys:
        dl_folder = os.path.join(main_dir, country)
        os.makedirs(dl_folder, exist_ok=True)
    
        files = os.listdir(dl_folder)
    
        # En el sitio, la paginación suele empezar en 0; aquí se usa st_pg-1 por esa razón.
        for page in range(st_pg - 1, end_pg):
            time.sleep(2)  # pequeña pausa para no saturar el servidor
            data = get_plate_links(country, page)
            with open(dl_folder + "/mycsvfile.csv", "a", newline="", encoding="utf-8") as f:
                w = csv.DictWriter(f, data[0].keys())
                w.writeheader()
                w.writerows(data)

except Exception as e:
    print("Error general: {}".format(e))
