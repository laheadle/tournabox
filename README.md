

levels: tourney [name, date], round[number], decision[date, entries, winner,loser], entry[seed], player[name, country, gender]


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

