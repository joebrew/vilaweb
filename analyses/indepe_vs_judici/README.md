La gran distracció: com afecta el judici al procés a l'atencio a la independència de Catalunya
================

    [1] TRUE

Introducció
===========

És clar que l'empresonament d'una grant part del lideratge polític català és important. El que no és tan clar és fins quin punt l'empresonament afecta les aspiracions polítiques dels líders catalans.

D'una banda, es podria hipotetitzar que la preso podria tenir un efecte *persuasiu* pel sobiranisme, demostrant que l'Estat espanyol té tendències autoritaries o que la Justícia espanyola confon, per raons polítiques, la desobediència civil amb una insurrecció armada. Però de l'altre banda, es podria hipotetitzar que la preso tindria un efecte *disuasiu* pel sobiranisme: que el fet de afrontar-se a possibles penes de preso fa que els líders no perseguexin els seus objectius polítics amb tanta fervor com abans, o tot simplement que el patiment humà dels presos fa que els sobiranistes canviin de prioritats.

Aquesta article tractarà aquest tema: quina és la relació entre l'atenció a la independència i l'atenció als presos.

Metodologia
===========

Faig un anàlisi de 422512 piulets de 40 polítics sobiranistes (no necessariament independentistes). Calculo la freqüencia de l'aparació de certs termes ("independència", "presos", "república", "judici", etc.).

Resultats
=========

El gràfic següent mostra el percentatge de piulets mensuals que cadascú dels polítics seleccionats esmentant paraules relacionades amb la independència ("independent", "independència", etc.) i els presos ("presos", "judici", "juicio") des del principi de 2017 fins el final del mes passat (maig 2019).

![](figures/unnamed-chunk-4-1.png)

Si agreguem tots aquests polítics en un sol gràfic, es veu així:

![](figures/unnamed-chunk-5-1.png)

Que passa? Les dues linies segueixen un patró classicament inversa: una puja i l'altre baixa, una baixa i l'altre puja. Què sugereix? Que com més atenció facin els polítics sobiranistes al judici, menys cas fan als seus objectius polítics.

Interpretació
=============

Entre polítics sobiranistes, l'atenció que fan als presos sembla estar inversament correlacionada amb l'atenció a la independència de Catalunya. Es podria interpretar aquesta troballa com a evidència de la efectivitat de l'estratègia de l'unionisme. És a dir, el fet d'haver empresonament el llideratge polític català ha frenat (o al menys desaccelerat) l'independentisme. Per raons humanes, els liders independentistes potser estàn més preocupats pel que passa en una sala a Madrid que al Parlament de Catalunya. O sigui, haver judicialitzat la qüestió de la independència de Catalunya sembla haver funcionat: els polítics independentistes, pendents del futur dels seus companys i de les seves families, ja no parlen tant del tema de la República.

Però hi ha una altre interpretació. La judicialització de la qüestió de la sobirania de Catalunya no és un fre a l'independentisme, sinó un "temps mort" a la política (a "time-out" from politics). És una simple pausa. Funciona en el sentit de que ha "decapitat" el llideratge, i ha captivat l'atenció de tothom durant més d'un any. Però el judici s'acaba. I l'anticipació del fi del judici ja es veu en les dades: en els últims mesos, el percentatge de piulets relacionats amb la independència torna a pujar.

L'empresonament va ser efectiu al seu moment, però l'efecte comença a afeblir-se. Els polítics catalans comencen a parlar, una altre vegada, del seu objectiu polític: una república independent.

Reflexió personal
=================

Independenment de la sentència a Madrid, d'aqui uns mesos Espanya i Catalunya es trobaràn en exactament la mateixa situació que en Octubre 2017. La única diferència serà que Espanya ja haurà "played the card up its sleeve". O sigui, o els presos estaràn en llibertat, o el cas ja no a Madrid, sinó Estrasburg. En qualsevol dels 2 casos anteriors, fer servir la Juridactura una altre vegada per a fins polítics ja no serà possible. Espanya haurà de reconeixer que té un problema polític, i haurà d'intentar resoldre el problema fent política.

Technical details
=================

-   Data scraped from twtiter during the period on June 12, 2019 using the python twint library.
-   Data processed, aggregated, and visualized using R.
-   The number of retweets and likes are as of date of data retrieval
-   All code for this analysis at <https://github.com/joebrew/vilaweb/tree/master/analyses/>
