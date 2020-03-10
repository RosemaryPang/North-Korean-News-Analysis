#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Jan 19 02:36:04 2019

@author: Rosemary Pang

North Korea Statement Scraping
"""
import os
import re
import random
from bs4 import BeautifulSoup
import requests
import time
from urllib.request import urlopen
from selenium import webdriver
from selenium.webdriver.common.by import By
from collections import Counter
from urllib.request import urlopen


path_to_chromedriver = '/Users/mpang/Downloads/chromedriver'
browser = webdriver.Chrome(executable_path = path_to_chromedriver)
browser.get('http://www.kcna.co.jp/item/2019/calendar-2019e.html')

elm = browser.find_element_by_link_text('2016')
browser.implicitly_wait(15)
elm.click()

os.chdir('/Users/mpang/Dropbox/NKStatement/2016')

counter = 0
text_cont = ''

for i in range(0,388):
    elems = browser.find_elements_by_xpath('//a[@href]')
    elems = [a.get_attribute('href') for a in elems]
    #print(elems)

    elems = [e for e in elems if not re.search('calendar',e)]

    for elem in elems:
        browser.get(elem)
    
        links = browser.find_elements_by_xpath("//a[@href]")
        links = [x for x in links if not re.search('calendar', str(x.get_attribute("href")))]
        links = [a.get_attribute('href') for a in links]
        
        for j in links:
            time.sleep(1)
            browser.get(j)
            html = browser.find_element_by_xpath(".//html")
            text_cont = html.text
            
            counter += 1
            fileName = "NK_Statement_" + "2016_" + str(counter) + ".txt"
            print(fileName)
            textFile = open(fileName, 'w')
            textFile.write(text_cont)
            textFile.close()
            print("File has been closed")

 
    
    
    



##### Try to work with only one news
res = requests.get('http://www.kcna.co.jp/item/2018/201812/news01/20181201-05ee.html')
res.content
soup = BeautifulSoup(res.content,'html.parser')
month = soup.find_all('b')[1].get_text()
date = soup.find_all('b')[2].get_text()
title = soup.find_all('b')[4].get_text()
content = soup.find_all('tr')[7].get_text()


