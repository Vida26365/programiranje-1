# Python
sez = [1, 2, 3, 4]

sez2 = [0] + sez
# V pomnilniku se ustvari popolnoma nov seznam v O(n) času

ni_nov_sez = sez.reverse()
# Seznam sez se spremeni na mestu. Ne moremo več dostopati do seznama kot je bil prej. ni_nov_sez ima vrednost None