import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import random
import openpyxl
import seaborn as sns
import matplotlib.pyplot as plt

# url = "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=187310&target=after&page=1"
# html = requests.get(url).text
# print(type(html)) #string
# soup = BeautifulSoup(html, 'html.parser')
# print(soup)
# print(type(soup)) #bs4 beautifulsoup

# elements = soup.find_all('td', class_ = 'title')

# print(elements)
# print(type(elements))
# print('This is the total element')
# print(elements[0])
#
#
# print('this is the tag em')
# stars =elements[0].select_one('em').text
# print(stars)
#
# print('this is nl part')
# comment = elements[0].select_one('br').next_sibling.strip()
# print(comment)

# df = pd.DataFrame(columns= ['comment','star'])

# for i in range(len(elements)):
#     # print(i)
#     # print(elements[i])
#     stars =elements[i].select_one('em').text
#     # print(stars)
#     comment = elements[i].select_one('br').next_sibling.strip()
#     # print(comment)
#     df= df.append({'comment' : comment , 'star': stars}, ignore_index=True)
#
# print(df.head())
# print(df.shape)


df = pd.DataFrame(columns= ['comment','star'])

for k in range(1,100):
    try:
        print('This is the page we are looking at')
        print(k)
        url = "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=187310&target=after&page={}".format(k)
        html = requests.get(url).text
        soup = BeautifulSoup(html, 'html.parser')
        elements = soup.find_all('td', class_ = 'title')
        print('element count')
        print(len(elements))
        for i in range(len(elements)):
            stars = elements[i].select_one('em').text
            comment = elements[i].select_one('br').next_sibling.strip()
            df = df.append({'comment': comment, 'star': stars}, ignore_index=True)
        interval = round(random.uniform(0, 2), 2)
        print('This is the interval')
        print(interval)
        time.sleep(interval)
    except:
        print('This will not happen')

print(df.head())
print(df.shape)
print(df.tail())

df.to_csv('./naver_movie.csv', index=False)
df.to_excel('./naver_movie.xlsx')

df = pd.read_csv('./naver_movie.csv')
print(df.head())
print(df.shape)


sns.displot(df['star'], bins = 5)
plt.show()
