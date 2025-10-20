# -*- coding: utf-8 -*-

"""
Script para descargar automáticamente imágenes de matrículas de Platesmania,
filtradas por país.
"""

import time
import requests
from bs4 import BeautifulSoup
import os
import shutil
import csv

# Código de país en ISO-2 (ej. "es", "fr", "it")
country = "es"

# Rango de páginas a recorrer (cada página ~10 imágenes según el sitio)
st_pg = 1
end_pg = 2

main_dir = "./data/"

BASE = "https://platesmania.com"
# Cabeceras para simular un navegador y evitar bloqueos básicos
HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) ' 
                  'AppleWebKit/537.36 (KHTML, like Gecko) '
                  'Chrome/115.0.0.0 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8',
    'Accept-Language': 'en-US,en;q=0.9',
    'Referer': 'https://platesmania.com/',
    'Connection': 'keep-alive',
    'DNT': '1',  # Do Not Track
    'Upgrade-Insecure-Requests': '1',
}


def get_plate_links(country, page):
    """
    Devuelve los enlaces 'nomer...' de una página de galería para un país.
    Retorna lista vacía si no existe el país o no se encuentra la galería.
    """
    url = BASE + "/{}/gallery.php?&start={}".format(country, page)
    print("\n🔎 Página: {}".format(url))
    r = requests.get(url, headers=HEADERS)
    r.raise_for_status()
    soup = BeautifulSoup(r.text, 'html.parser')

    # Heurística sencilla para comprobar que el país/galería es válida
    if not soup.find(string=lambda t: "You have searched:" in t):
        print("⚠️ No se encontró el país")
        return []

    # Acotar el HTML a la columna principal donde está la galería
    main_col = soup.find("div", class_="col-md-9")
    if not main_col:
        print("⚠️ No se encontró la galería principal")
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
    # Carpeta de destino por país (p.ej. C:\...\ocr5_proto\es)
    dl_folder = os.path.join(main_dir, country)
    os.makedirs(dl_folder, exist_ok=True)

    # Cache de nombres ya presentes para evitar re-descargas
    files = os.listdir(dl_folder)

    # En el sitio, la paginación suele empezar en 0; aquí se usa st_pg-1 por esa razón.
    for page in range(st_pg - 1, end_pg):
        time.sleep(2)  # pequeña pausa para no saturar el servidor
        data = get_plate_links(country, page)
        with open(dl_folder + "mycsvfile.csv", "w", newline="", encoding="utf-8") as f:
            w = csv.DictWriter(f, data[0].keys())
            w.writeheader()
            w.writerows(data)

except Exception as e:
    print("❌ Error general: {}".format(e))
