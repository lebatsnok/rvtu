## RVTU funktsioonid

Paigaldamine: `remotes::install_github("lebatsnok/rvtu")`

[RVTU andmetabelite metainfo kasutamine ja haldamine](#metainfo)
[Vaimse tervisega seotud heaolu indeksi (VTHOI) arvutamine](#vthoi)

### <a name="metainfo"></a>RVTU andmetabelite metainfo kasutamine ja haldamine

#### Võtmes sisalduva metainfo kasutamine

RVTU  andmetabeleid säilitatakse R-is andmetabeline (data.frame), millele on lisatud metainfo tunnuste kohta. Kui andmetabeli nimi on `x`, siis `key(x)` annab tulemuseks 'võtme': metainfo tabeli (sh tunnuste tüübid, selgitused, faktortunnuste väärtused jne). Võtmes on iga tabeli `x` tunnuse kohta 1 rida ja kuitahes palju veerge; veerus nimega `NAME` peab olema tunnuse nimi. Seda metainfot kasutavad järgmised funktsioonid:

* `grab(x, EXPR)` -- vali andmetabelist x kasutades tunnuste metainfot
* `key(x, expr)`  -- andmetabeli x võti (või osa sellest)
* `%has%`         -- x %has% y - kas x-is esineb alamstring y, suur-ja väiketähtedel vahet tegemata (nt "Linnavalitsu %has% "linn" --> TRUE)
* `%Has%`         -- x %Has% y - sama, mis eelmine, kuid tehakse vahet suur- ja väiketähtedel nt "Linnavalitsus" %has% "Linn" --> TRUE

Näited:

`grab(x, NAME %has% "eek")`  -- kõik tunnused andmetabelist x, mille nimes on "eek" kas suur- või väiketähtedega

`grab(x, SRC %in% "EEK")`    -- tunnused andmetabelist x, mille puhul võtme tulba `SRC` väärtus on "EEK"

`key(x, SRC %in% "EEK")`     -- andmetabeli x võti, arvestades ainult neid tunnuseid, mille puhul võtme tulba `SRC` väärtus on "EEK"

#### Võtme asendamine ja täiendamine

* `key<-`             -- andmetabeli võtme asendamine (eeldatakse, et võtmes on täpselt 1 rida iga andmetabeli tunnuse kohta)
* `fkey<-`            -- andmetabeli võtme asendamine (võti võib vajada korrastamist)
* `add2key`           -- võtmele 1 või mitme rea lisamine
* `regularize_key`    -- andmetabeli võtme korrastamine (tulemuseks on võti, kus on täpselt 1 rida iga andmetabeli tunnuse kohta)

#### Faktortunnuste tasemed

* `paste_levels`      -- 
* `split_levels`      --
* `use_labels`        -- kasuta koodide asemel 'silte'
* `factorize`         -- teisenda tunnus 'faktoris' R-i mõttes
* `factorize_all`     -- teisenda kõik tunnused andmetabelis x faktoriteks

#### Skooride arvutamine
* `compute_scores`    -- liida samasse skaalasse kuuluvad väited / küsimused


#### Muu

* `include_all` 
* `regularize_mk`     -- maakonnatunnuse teisendamine
* `write_xlsx`        -- salvesta andmetabel koos võtmega exceli tablelina
* `label_for_export`  -- lisa tunnusele faktorite väärtused (nt SPSS-i failina salvestamiseks)
* `Load`              -- loe `.rda` failist (sarnane funktsiooniga `base::load`, kuid töökeskkonda ei muudeta, vaid tagastatakse väärtus listina, mille komponentideks on kõik failis salvestatud objektod)

### <a name="vthoi"></a>Vaimse tervisega seotud heaolu indeksi (VTHOI) arvutamine

Funktsioon `vthoi` vaimse tervisega seotud heaolu indeksi arvutamiseks. 

Kasutamine:

Eelduseks on andmetabel, kus on 8 tunnust täpselt õiges järjekorras (tunnuste nimed ei ole tähtsad, kuid vastusevariantide kodeering on oluline, selle kohta vt RVTU koondaruande Lisa 1, alapeatükk 1.6)

|  |                                                 |       |
|--|-------------------------------------------------|-------|
| 1|„Tunnen end enamasti energilise ja elujõulisena” |(B52)  |
| 2|„Tunnen end enamasti tähelepaneliku ja ärksana”  |(B53)  |
| 3|„Vaatan tulevikku lootuse ja entusiasmiga”       |(B54)  |
| 4|Rahulolu eluga tervikuna                         |(B1)   |
| 5|Rahulolu majandusliku olukorraga                 |(B2)   |
| 6|Rahulolu peresuhetega                            |(B3)   |
| 7|Rahulolu sõprussuhetega                          |(B4)   |
| 8|„Kuidas Te hindate oma tervist käesoleval ajal?” |(C4)   |
|  |                                                 |       |

Eeldame, et selline andmetabel kannab nime `X`. Sel juhul `vthoi(X)` annab tulemuseks faktorskoorid, mis vastavad täpselt RVTU aruandes kasutatutele. 
