#!/usr/bin/env python
# coding: utf-8

# In[2]:


from bs4 import BeautifulSoup
import requests
import pandas as pd

review_dict = {'username':[], 'date':[],'score':[],'review':[]}
while True: # While there are still reviews to collect
    video_game = input().lower() # Type in the name of a video game
    if (video_game == 'quit'): break # Stops collecting reviews if prompted to quit
    video_game = video_game.replace(' ', '-').replace(':','').lower() 
    
    print("What are the number of pages?")
    num_pages = int(input())
    
    for page in range(0,num_pages):
        URL = "https://www.metacritic.com/game/pc/"+ video_game + "/user-reviews?sort-by=most-helpful&num_items=100&page="+str(page)
        user_agent = {'User-agent': 'Mozilla/5.0'}
        response = requests.get(URL, headers = user_agent)
        soup = BeautifulSoup(response.text, 'html.parser')

        for review in soup.find_all('li', class_='review user_review'):
            if review.find('span', class_='blurb blurb_expanded'):
                review_dict['review'].append(
                    review.find('span', class_='blurb blurb_expanded').text)
                review_dict['username'].append(
                    review.find('div', class_='review_critic').find('a').text)
                review_dict['date'].append(
                    review.find('div', class_='date').text)
                review_dict['score'].append(
                    review.find('div', class_='review_grade').find('div').text)
            else:
                review_dict['review'].append(
                    review.find('div',class_='review_body').find('span').text) 
                review_dict['username'].append(
                    review.find('div', class_='review_critic').find('a').text)
                review_dict['date'].append(
                    review.find('div', class_='date').text)
                review_dict['score'].append(
                    review.find('div', class_='review_grade').find('div').text)
pd.DataFrame(review_dict).to_csv('output.csv')

