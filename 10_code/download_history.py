#####################################################################
##################################
# Section I - Pull OHLCV
##################################
#####################################################################

#####################################################################
# 1. Import library and setup API key
#####################################################################
import requests
import json
import csv
from datetime import datetime
import pandas as pd

apikey = "9f1062e7562a30112cc49d3789b3edcc6a1e782905f5160856a27930ca18585d"
# attach to end of URLstring
url_api_part = "&api_key=" + apikey

#####################################################################
# 2 Populate list of all coin names
#####################################################################
# 2.1 - List of all coin names
# URL to get a list of coins from cryptocompare API
URLcoinslist = "https://min-api.cryptocompare.com/data/all/coinlist"

# Get list of cryptos with their symbols
res1 = requests.get(URLcoinslist)
res1_json = res1.json()
data1 = res1_json["Data"]
symbol_array = []
cryptoDict = dict(data1)

# write to CSV
with open("coin_names.csv", mode="w", encoding="utf-8") as test_file:
    test_file_writer = csv.writer(
        test_file, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
    )
    for coin in cryptoDict.values():
        name = coin["Name"]
        symbol = coin["Symbol"]
        symbol_array.append(symbol)
        coin_name = coin["CoinName"]
        full_name = coin["FullName"]
        entry = [symbol, coin_name]
        test_file_writer.writerow(entry)
print("Done getting crypto names and symbols. See coin_names.csv for result")


# 2.2 - List of coin ids
# URL to get a list of coins from cryptocompare API
URLcoinslist = "https://min-api.cryptocompare.com/data/blockchain/list?" + url_api_part

# Get list of cryptos with their symbols
res1 = requests.get(URLcoinslist)
res1_json = res1.json()
data1 = res1_json["Data"]
cryptoDict = dict(data1)

# write to CSV
with open("coin_ids.csv", mode="w", encoding="utf-8") as test_file:
    test_file_writer = csv.writer(
        test_file, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
    )
    for coin in cryptoDict.values():
        id = coin["id"]
        symbol = coin["symbol"]
        partner_symbol = coin["partner_symbol"]
        rawts = coin["data_available_from"]
        data_available_from = datetime.utcfromtimestamp(rawts).strftime("%Y-%m-%d %H:%M:%S")
        entry = [id, symbol, partner_symbol, data_available_from]
        test_file_writer.writerow(entry)
print("Done getting crypto ids. See coin_ids.csv for result")

# add column names
coin_ids = pd.read_csv(
    "./00_source/coin_ids.csv",
    header=0,
    names=["id", "symbol", "partner_symbol", "data_available_from"],
)
coin_ids.to_csv("./00_source/coin_ids.csv", index=False)


#####################################################################
# 3. Populate historical price for each crypto in BTC
#####################################################################
# #Note: this part might take a while to run since we're populating data for 4k+ coins
# #counter variable for progress made

# # for now, we'll only get BTC
# symbol_array=['BTC']

# progress = 0
# num_cryptos = str(len(symbol_array))
# for symbol in symbol_array:
#    # get data for that currency
#    URL = 'https://min-api.cryptocompare.com/data/histoday?fsym=' + symbol + '&tsym=BTC&allData=true' + url_api_part
#    res = requests.get(URL)
#    res_json = res.json()
#    data = res_json['Data']
#    # write required fields into csv
#    with open('crypto_prices.csv', mode = 'a', encoding='utf-8') as test_file:
#        test_file_writer = csv.writer(test_file,
#                                      delimiter = ',',
#                                      quotechar = '"',
#                                      quoting=csv.QUOTE_MINIMAL)
#        for day in data:
#            rawts = day['time']
#            ts = datetime.utcfromtimestamp(rawts).strftime('%Y-%m-%d %H:%M:%S')
#            o = day['open']
#            h = day['high']
#            l = day['low']
#            c = day['close']
#            vfrom = day['volumefrom']
#            vto = day['volumeto']
#            entry = [ts, o, h, l, c, vfrom, vto, symbol]
#            test_file_writer.writerow(entry)
#    progress = progress + 1
#    print('Processed ' + str(symbol))
#    print(str(progress) + ' currencies out of ' +  num_cryptos + ' written to csv')
# print('Done getting price data for all coins. See crypto_prices.csv for result')

#####################################################################
# 4. Populate BTC prices in different fiat currencies
#####################################################################
# List of fiat currencies we want to query
# You can expand this list, but CryptoCompare does not have
# a comprehensive fiat list on their site
# fiatList = ['USD']

progress2 = 0
fiat = "USD"
timeStamp = int(
    datetime.timestamp(datetime.now())
)  # initiate time stamp to current time
for i in range(0, 10):
    # get data for bitcoin price in that fiat
    URL = (
        "https://min-api.cryptocompare.com/data/histohour?fsym=BTC&tsym="
        + fiat
        + "&limit=2000&toTs="
        + str(timeStamp)
    )
    res = requests.get(URL)
    res_json = res.json()
    data = res_json["Data"]
    # write required fields into csv
    with open("btc_prices_all.csv", mode="a") as test_file:
        test_file_writer = csv.writer(
            test_file, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        for hour in data:
            rawts = hour["time"]
            ts = datetime.utcfromtimestamp(rawts).strftime("%Y-%m-%d %H:%M:%S")
            o = hour["open"]
            h = hour["high"]
            l = hour["low"]
            c = hour["close"]
            vfrom = hour["volumefrom"]
            vto = hour["volumeto"]
            entry = [ts, o, h, l, c, vfrom, vto, fiat]
            test_file_writer.writerow(entry)
    progress2 = progress2 + 1
    timeStamp = data[0]["time"]
    print(data[0]["time"], timeStamp)
    # print('processed ' + str(fiat))
    # print(str(progress2) + ' currencies out of  17 written')
print("Done getting price data for btc. See btc_prices.csv for result")

# add column names
prices = pd.read_csv(
    "./00_source/btc_prices_all.csv",
    header=0,
    names=["time", "open", "high", "low", "close", "volumefrom", "volumeto", "fiat"],
)
prices.to_csv("./00_source/btc_prices_all.csv", index=False)


#####################################################################
##################################
# Section II - Pull Social Media Mentions
##################################
#####################################################################
progress3 = 0

coinId = 1182  # the coinId of BTC
timeStamp = int(
    datetime.timestamp(datetime.now())
)  # initiate time stamp to current time
for i in range(0, 10):
    # get data for bitcoin price in that fiat
    URL = (
        "https://min-api.cryptocompare.com/data/social/coin/histo/hour?coinId="
        + str(coinId)
        + "&limit=2000&toTs="
        + str(timeStamp)
        + url_api_part
    )
    res = requests.get(URL)
    res_json = res.json()
    data = res_json["Data"]
    # write required fields into csv
    with open("btc_social.csv", mode="a") as test_file:
        test_file_writer = csv.writer(
            test_file, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
        )
        for hour in data:
            rawts = hour["time"]
            ts = datetime.utcfromtimestamp(rawts).strftime("%Y-%m-%d %H:%M:%S")
            comments = hour["comments"]
            posts = hour["posts"]
            followers = hour["followers"]
            points = hour["points"]
            overview_page_views = hour["overview_page_views"]
            analysis_page_views = hour["analysis_page_views"]
            markets_page_views = hour["markets_page_views"]
            charts_page_views = hour["charts_page_views"]
            trades_page_views = hour["trades_page_views"]
            forum_page_views = hour["forum_page_views"]
            influence_page_views = hour["influence_page_views"]
            total_page_views = hour["total_page_views"]
            fb_likes = hour["fb_likes"]
            fb_talking_about = hour["fb_talking_about"]
            twitter_followers = hour["twitter_followers"]
            twitter_following = hour["twitter_following"]
            twitter_lists = hour["twitter_lists"]
            twitter_favourites = hour["twitter_favourites"]
            twitter_statuses = hour["twitter_statuses"]
            reddit_subscribers = hour["reddit_subscribers"]
            reddit_active_users = hour["reddit_active_users"]
            reddit_posts_per_hour = hour["reddit_posts_per_hour"]
            reddit_posts_per_day = hour["reddit_posts_per_day"]
            reddit_comments_per_hour = hour["reddit_comments_per_hour"]
            reddit_comments_per_day = hour["reddit_comments_per_day"]
            code_repo_stars = hour["code_repo_stars"]
            code_repo_forks = hour["code_repo_forks"]
            code_repo_subscribers = hour["code_repo_subscribers"]
            code_repo_open_pull_issues = hour["code_repo_open_pull_issues"]
            code_repo_closed_pull_issues = hour["code_repo_closed_pull_issues"]
            code_repo_open_issues = hour["code_repo_open_issues"]
            code_repo_closed_issues = hour["code_repo_closed_issues"]
            code_repo_contributors = hour["code_repo_contributors"]
            entry = [ts, comments,posts,followers,points,overview_page_views,analysis_page_views,markets_page_views,charts_page_views,trades_page_views,forum_page_views,influence_page_views,total_page_views,fb_likes,fb_talking_about,twitter_followers,twitter_following,twitter_lists,twitter_favourites,twitter_statuses,reddit_subscribers,reddit_active_users,reddit_posts_per_hour,reddit_posts_per_day,reddit_comments_per_hour,reddit_comments_per_day,code_repo_stars,code_repo_forks,code_repo_subscribers,code_repo_open_pull_issues,code_repo_closed_pull_issues,code_repo_open_issues,code_repo_closed_issues,code_repo_contributors]
            test_file_writer.writerow(entry)
    timeStamp = data[0]["time"]
    # print('processed ' + str(fiat))
    # print(str(progress2) + ' currencies out of  17 written')
print("Done getting price data for btc. See btc_social.csv for result")

# add column names
social = pd.read_csv(
    "./00_source/btc_social.csv",
    header=0,
    names=["time","comments","posts","followers","points","overview_page_views","analysis_page_views","markets_page_views","charts_page_views","trades_page_views","forum_page_views","influence_page_views","total_page_views","fb_likes","fb_talking_about","twitter_followers","twitter_following","twitter_lists","twitter_favourites","twitter_statuses","reddit_subscribers","reddit_active_users","reddit_posts_per_hour","reddit_posts_per_day","reddit_comments_per_hour","reddit_comments_per_day","code_repo_stars","code_repo_forks","code_repo_subscribers","code_repo_open_pull_issues","code_repo_closed_pull_issues","code_repo_open_issues","code_repo_closed_issues","code_repo_contributors"],
)
social.to_csv("./00_source/btc_social.csv", index=False)
