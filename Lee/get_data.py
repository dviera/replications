import urllib
from bs4 import BeautifulSoup
import re
import csv


url = "http://stats.footballpredictions.net/england/premier/1995-1996/results.html"
htmlfile = urllib.urlopen(url)
soup = BeautifulSoup(htmlfile)

#home team names
hometeam = soup.find_all("td", class_="hometeam")
ht = []
for element in hometeam:
	ht.append(element.a.get_text())

#away team names
awayteam = soup.find_all("td", class_="awayteam")
wt = []
for element in awayteam:
	wt.append(element.a.get_text())

#scores
scores = soup.find_all("td", class_="score")
sc = []
for element in scores:
	#sc.append(element.get_text())
	a = element.get_text()
	a = re.sub(r"\s+", "", a, flags=re.UNICODE)
	a = a.encode('ascii')
	sc.append(a.split('-'))


f = open("results.csv", "wt")

try: 
	writer = csv.writer(f, delimiter=',')
	writer.writerow(("hometeam", "awayteam", "homegoal", "awaygoal"))
	for i in range(len(ht)):
		writer.writerow((ht[i], wt[i], sc[i][0], sc[i][1]))
finally:
	f.close()

f = open("results2.csv", "wt")

try: 
	writer = csv.writer(f, delimiter=',')
	writer.writerow(("team", "opponent", "goal", "home"))
	for i in range(len(ht)):
		writer.writerow((ht[i], wt[i], sc[i][0], 1))
		writer.writerow((wt[i], ht[i], sc[i][1], 0))
finally:
	f.close()
