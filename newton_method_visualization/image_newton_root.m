function [A] = image_newton_root(a, b, c, d, m, n, n_poly, a_poly)
% Ta funkcja zwraca macierz (n+1)x(m+1) wypelniona wartosciami znalezionych
% miejsc zerowych wielomianu w.
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
        A(k,p) = newton_root(z,n_poly, a_poly); % w polu macierzy (k,p) zapisujemy miejsce zerowe znalezione za pomocą metody Newtona dla z zadeklarowanego wiersz wyżej
    end
end
end