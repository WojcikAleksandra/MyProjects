function [A] = image_newton(a, b, c, d, m, n, n_poly, a_poly)
% Ta funkcja zwraca macierz (n+1)x(m+1), na podstawie ktorej tworzona jest
% wizualizacja szybkosci zbieznosci metody Newtona.
% [a,b] to zakres wartosci x (czesc rzeczywista) dla kolejnych liczb zespolonych
% [c,d] to natomiast zakres y (czesc urojona)
% n_poly to stopien wielomianu w
% a_poly to wektor wspolczynnikow wielomianu
A = zeros(n+1, m+1); % tworzymy macierz A zapelniona zerami
h_1 = (b-a)/n; % ustalamy odleglosc miedzy kolejnymi czesciami rzeczywistymi liczb zepolonych
h_2 = (d-c)/m; % ustalamy odleglosc miedzy kolejnymi czesciami urojonymi liczb zepolonych
for k = 1:n+1
    for p = 1:m+1
        x = a + (k-1)*h_1;
        y = c + (p-1)*h_2;
        z = x + y*j;
        A(k,p) = newton(z,n_poly, a_poly); % w polu macierzy (k,p) zapisujemy liczbe iteracji metody newtona dla z zadeklarowanego wiersz wyzej
    end
end
end