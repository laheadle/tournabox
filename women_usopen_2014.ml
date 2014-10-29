
let entries =
"WILLIAMS, Serena USA [1]
TOWNSEND, Taylor USA (W)
SCHIAVONE, Francesca ITA
KING, Vania USA
LEPCHENKO, Varvara USA
VAN UYTVANCK, Alison BEL
BARTHEL, Mona GER
ZHANG, Shuai CHN [32]
STOSUR, Samantha AUS [24]
DAVIS, Lauren USA
PARMENTIER, Pauline FRA
KANEPI, Kaia EST
VANDEWEGHE, Coco USA
VEKIC, Donna CRO
TOMLJANOVIC, Ajla AUS
SUAREZ NAVARRO, Carla ESP [15]
PENNETTA, Flavia ITA [11]
GOERGES, Julia GER
ROGERS, Shelby USA
ZANEVSKA, Maryna UKR (Q)
GIBBS, Nicole USA (W)
GARCIA, Caroline FRA
PEREIRA, Teliana BRA
PAVLYUCHENKOVA, Anastasia RUS [23]
DELLACQUA, Casey AUS [29]
MAYR-ACHLEITNER, Patricia AUT
KANIA, Paula POL (Q)
WANG, Qiang CHN (Q)
PLISKOVA, Karolina CZE
MEUSBURGER, Yvonne AUT
RISKE, Alison USA
IVANOVIC, Ana SRB [8]
KVITOVA, Petra CZE [3]
MLADENOVIC, Kristina FRA
KOUKALOVA, Klara CZE
CETKOVSKA, Petra CZE
KRUNIC, Aleksandra SRB (Q)
PITER, Katarzyna POL
GAJDOSOVA, Jarmila AUS (W)
KEYS, Madison USA [27]
KUZNETSOVA, Svetlana RUS [20]
ERAKOVIC, Marina NZL
VESNINA, Elena RUS
CHAN, Yung-Jan TPE (Q)
SCHEEPERS, Chanelle RSA
MCHALE, Christina USA
DOI, Misaki JPN
AZARENKA, Victoria BLR [16]
CIBULKOVA, Dominika SVK [12]
BELLIS, Catherine USA (W)
DIYAS, Zarina KAZ
TSURENKO, Lesia UKR (Q)
SVITOLINA, Elina UKR
HERCOG, Polona SLO
MIN, Grace USA (W)
MAKAROVA, Ekaterina RUS [17]
ZAHLAVOVA STRYCOVA, Barbora CZE [30]
BARTY, Ashleigh AUS (Q)
SHVEDOVA, Yaroslava KAZ
NICULESCU, Monica ROU
CIRSTEA, Sorana ROU
WATSON, Heather GBR
GOVORTSOVA, Olga BLR
BOUCHARD, Eugenie CAN [7]
KERBER, Angelique GER [6]
PERVAK, Ksenia RUS (Q)
DUAN, Ying-Ying CHN (Q)
KUDRYAVTSEVA, Alla RUS (Q)
BENCIC, Belinda SUI
WICKMAYER, Yanina BEL
WOZNIAK, Aleksandra CAN
NARA, Kurumi JPN [31]
STEPHENS, Sloane USA [21]
BECK, Annika GER
RAZZANO, Virginie FRA
LARSSON, Johanna SWE
KNAPP, Karin ITA
PIRONKOVA, Tsvetana BUL
JOVANOVSKI, Bojana SRB
JANKOVIC, Jelena SRB [9]
SAFAROVA, Lucie CZE [14]
BABOS, Timea HUN
ZHENG, Saisai CHN (Q)
VOEGELE, Stefanie SUI
OPRANDI, Romina SUI
HANTUCHOVA, Daniela SVK
HESSE, Amandine FRA (W)
CORNET, Alize FRA [22]
VINCI, Roberta ITA [28]
ORMAECHEA, Paula ARG
BEGU, Irina-Camelia ROU
SOLER-ESPINOSA, Silvia ESP
PENG, Shuai CHN
ZHENG, Jie CHN
FICHMAN, Sharon CAN
RADWANSKA, Agnieszka POL [4]
SHARAPOVA, Maria RUS [5]
KIRILENKO, Maria RUS
PLISKOVA, Kristyna CZE
DULGHERU, Alexandra ROU
GLUSHKO, Julia ISR
BRENGLE, Madison USA (W)
ABANDA, Francoise CAN (Q)
LISICKI, Sabine GER [26]
PETKOVIC, Andrea GER [18]
JABEUR, Ons TUN (Q)
SMITKOVA, Tereza CZE
PUIG, Monica PUR
SCHMIEDLOVA, Anna SVK
SASNOVICH, Aliaksandra BLR (Q)
RYBARIKOVA, Magdalena SVK
WOZNIACKI, Caroline DEN [10]
ERRANI, Sara ITA [13]
FLIPKENS, Kirsten BEL
RODIONOVA, Anastasia AUS (Q)
GIORGI, Camila ITA
BERTENS, Kiki NED
BACSINSZKY, Timea SUI
DATE-KRUMM, Kimiko JPN
WILLIAMS, Venus USA [19]
MUGURUZA, Garbine ESP [25]
LUCIC-BARONI, Mirjana CRO (Q)
KONTA, Johanna GBR
PEER, Shahar ISR
CEPELOVA, Jana SVK
TORRO-FLOR, Maria-Teresa ESP
COLLINS, Danielle Rose USA (W)
HALEP, Simona ROU [2]"


let outcomes =
"WILLIAMS, s
KING
LEPCHENKO
BARTHEL
STOSUR
KANEPI
VANDEWEGHE
SUAREZ NAVARRO
PENNETTA
ROGERS
GIBBS
PAVLYUCHENKOVA
DELLACQUA
WANG
PLISKOVA, ka
IVANOVIC
KVITOVA
CETKOVSKA
KRUNIC
KEYS
ERAKOVIC
VESNINA
MCHALE
AZARENKA
BELLIS
DIYAS
HERCOG
MAKAROVA
ZAHLAVOVA STRYCOVA
NICULESCU
CIRSTEA
BOUCHARD
KERBER
KUDRYAVTSEVA
BENCIC
NARA
STEPHENS
LARSSON
PIRONKOVA
JANKOVIC
SAFAROVA
ZHENG, s
HANTUCHOVA
CORNET
VINCI
BEGU
PENG
RADWANSKA
SHARAPOVA
DULGHERU
BRENGLE
LISICKI
PETKOVIC
PUIG
SASNOVICH
WOZNIACKI
ERRANI
RODIONOVA
BACSINSZKY
WILLIAMS, v
LUCIC-BARONI
PEER
CEPELOVA
HALEP
WILLIAMS, s
LEPCHENKO
KANEPI
SUAREZ NAVARRO
PENNETTA
GIBBS
DELLACQUA
PLISKOVA, ka
KVITOVA
KRUNIC
VESNINA
AZARENKA
DIYAS
MAKAROVA
ZAHLAVOVA STRYCOVA
BOUCHARD
KERBER
BENCIC
LARSSON
JANKOVIC
SAFAROVA
CORNET
VINCI
PENG
SHARAPOVA
LISICKI
PETKOVIC
WOZNIACKI
ERRANI
WILLIAMS, v
LUCIC-BARONI
HALEP
WILLIAMS, s
KANEPI
PENNETTA
DELLACQUA
KRUNIC
AZARENKA
MAKAROVA
BOUCHARD
BENCIC
JANKOVIC
SAFAROVA
PENG
SHARAPOVA
WOZNIACKI
ERRANI
LUCIC-BARONI
WILLIAMS, s
PENNETTA
AZARENKA
MAKAROVA
BENCIC
PENG
WOZNIACKI
ERRANI
PENG
WOZNIACKI
williams, s
makarova
williams, s
wozniacki"
(*williams, s*)
;;

Tourney.play entries outcomes

