## RVTU funktsioonid

Praegu on siin funktsioon `hoi` vaimse tervisega seotud heaolu indeksi arvutamiseks.

Kasutamine:

1. `remotes::install_github("lebatsnok/rvtu")`
2. Nüüd tuleks leida andmetabel, kus on 8 tunnust täpselt õiges järjekorras (tunnuste nimed ei ole tähtsad, kuid vastusevariantide kodeering on oluline, selle kohta vt RVTU koondaruande Lisa 1, p 1.6)

1 „Tunnen end enamasti energilise ja elujõulisena” (B52) 
2 „Tunnen end enamasti tähelepaneliku ja ärksana” (B53) 
3 „Vaatan tulevikku lootuse ja entusiasmiga” (B54) 
4 Rahulolu eluga tervikuna (B1) 
5 Rahulolu majandusliku olukorraga (B2) 
6 Rahulolu peresuhetega (B3) 
7 Rahulolu sõprussuhetega (B4) 
8 „Kuidas Te hindate oma tervist käesoleval ajal?” (C4) 

3. Oletame, et selline andmetabel on leitud ja kannab nime `X`. Sel juhul `hoi(x)` annab tulemuseks faktorskoorid, mis vastavad täpselt RVTU aruandes kasutatutele. 