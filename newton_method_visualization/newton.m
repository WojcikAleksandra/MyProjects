function [iterations] = newton(z_0, n, a, e)
% Ta funkcja oblicza liczbe iteracji potrzebna do znalezienia
% miejsca zerowego wielomianu w za pomoca metody newtona przyjmujac za punkt poczatkowy z_0
% argument n to stopien wielomianu w,
% a to wektor wspolczynnikow,
% e to blad uzywany przy warunku stopu
iterations = 0;
if nargin == 3 
    e = 10^(-15); % domyslna wartosc bledu e
end
sub = 10; % sub to roznica miedzy kolejnymi przyblizeniami, inicjujemy ja liczba wieksza od e
z = z_0;
while abs(sub) >= e % szukamy kolejnych przyblizen, dopoki roznica miedzy nimi nie bedzie mniejsza niz e
    iterations = iterations + 1;
    if iterations == 40 % jezeli po 40 iteracjach nie bylo warunku stopu, to wychodzimy z petli i zwracamy 40, wtedy na wizualizacji te punkty beda najciemniejsze
        break
    end       
    if wielomian_pochodna(z, n, a) == 0 % jezeli w pewnym kroku otrzymamy styczna rownolegla, to ustawiamy liczbe iteracji na bardzo mala, zeby na wizualizacji dany punkt odroznial sie od reszty 
        iterations = -30;
        break
    end
    z_prev = z;
    z = z_prev - (wielomian_wartosc(z_prev, n, a)/(wielomian_pochodna(z_prev, n,a))); % obliczamy kolejne przyblizenia
    sub = z - z_prev;
end
end