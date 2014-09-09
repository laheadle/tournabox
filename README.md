

src/lib/*.ml
src/tourneys/year/
 -tourney_name.ml

html/tourneys/year/
 -tourney_name.html
 -tourney_name.js

---

html/players/djok.html
html/tourneys/year/usopen.html
html/tourneys/year/djok.html

- Is a table per tourney useful?
- Is a complete player history useful?
- Or a complete queryable decision history?

levels: tourney [name, year], round[number], decision[entries, winner,loser], entry[seed], player[name, country, gender]

most important ops:

1 - group by: player (seed, name ascending) or round (descending)
2 - name filter
3 - country filter

columns

- seed [2]
- name [2]
- country [2]

- winner [1]
- loser [1]

- round

- gender?
- tournament?
- year?

- handling doubles? mixed doubles?

ui:

round > N
players[a,b] => contains "serena"
seed 
involves country
involves gender:

players.name contains "serena"

how about: serb vs russian?

players[a,b] => a.country = serbia and b.country = russia

top ten vs 2nd ten:

players[a,b] => a.seed < 11 and b.seed > 10 and b.seed < 21

why git as a database?
----------------------

- Free
- Transparent
- Easy to change
- See a history of changes
- Easy to fork, customize, merge
- Decouples front and backend, Encourages experimentation with other front-ends
- Easy to import into other backends (e.g. sql)

