from selenium import webdriver
import time
import pandas as pd
from tqdm import tqdm
import urllib.request

def movie_Crawler():

    df= pd.DataFrame(columns=['title','image', 'genre'])
    total = 0


    driver = webdriver.Chrome('./chromedriver')
    #naver movie 1st page
    driver.get('https://movie.naver.com/movie/running/current.nhn')

    indexnum=0
    for i in tqdm(range(1,20)):

        #TITLE
        movie = driver.find_element_by_xpath('//*[@id="content"]/div[1]/div[1]/div[3]/ul/li['+str(i)+']/dl/dt/a')
        title = movie.text
        movie.click()

        #GENRE
        try:
            genre= driver.find_element_by_xpath('//*[@id="content"]/div[1]/div[2]/div[1]/dl/dd[1]/p/span[1]/a').text
        except:
            try:
                genre = driver.find_element_by_xpath('// *[ @ id = "content"] / div[1] / div[2] / div[1] / dl / dd[1] / p / span[1] / a[1]').text
            except:
                print('Genre not found')
                pass

        time.sleep(0.5)

        #POSTER
        #photo
        driver.find_element_by_xpath('//*[@id="movieEndTabMenu"]/li[3]/a/em').click()

        time.sleep(1)

        #poster
        poster = driver.find_element_by_class_name('photo_sub_tab')
        tag = poster.find_elements_by_tag_name('li') #list

        for i in range(len(tag)):
            if tag[i].get_attribute('imagetype') == 'POSTER':

                tag[i].click()
            else:
                pass

        time.sleep(1)
        count = int(driver.find_element_by_xpath('//*[@id="photoTypeGroup"]/li[2]/a/em').text)
        total += count

        while True:
            time.sleep(0.4)
            img = driver.find_element_by_xpath('//*[@id="photo_area"]/div/div[4]/div/div/div/img').get_attribute('src')
            # print(img)
            df = df.append({'title': title, 'image': img,'genre': genre}, ignore_index=True)

            urllib.request.urlretrieve(img, './img/' + str(title) + ' ' + str(indexnum) + '.jpg')
            indexnum += 1

            try:
                #arrow click
                driver.find_element_by_xpath('//*[@id="photo_area"]/div/div[4]/div/div/a[2]').click()

            except:
                break

        time.sleep(2)
        driver.back()
        driver.back()


    driver.close()
    print(total)


    return df




movie= movie_Crawler()
# print(movie.shape)
# print(movie.tail(10))
# movie.to_csv('./movie.csv', index=False)



data = pd.read_csv('./movie.csv')
print(data.shape)
print(data.head())



