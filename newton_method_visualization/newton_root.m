function [root] = newton_root(z_0, n, a, e)
% Ta funkcja oblicza miejsce zerowe wielomianu w za pomoca metody Newtona,
% przyjmujac za punkt poczatkowy z_0.
% argument n to stopien wielomianu w,
% a to wektor wspolczynnikow, natomiast e to blad uzywany przy warunku stopu
iterations = 0;
if nargin == 3 
    e = 10^(-15); % domyslna wartosc bledu e
end
sub = 10; % sub to roznica miedzy kolejnymi przyblizeniami, inicjujemy ja liczba wieksza od e
z = z_0;
root = z;
while abs(sub)>= e % szukamy kolejnych przyblizen dopoki roznica miedzy nimi nie będzie mniejsza niz e
    iterations = iterations + 1;
    if iterations == 200 | wielomian_pochodna(z, n, a) == 0  % jezeli po 200 iteracjach nie bylo warunku stopu lub otrzymalismy styczna rownolegla, to wychodzimy z petli i zwracamy NaN
        root = NaN;
        break
    end
    z_prev = z;
    z = z_prev - (wielomian_wartosc(z_prev, n, a)/(wielomian_pochodna(z_prev, n,a))); % obliczamy kolejne przyblizenia
    sub = z - z_prev;
    root = z;
end
root = round(root, 5);
end