%% SKRYPT TESTUJACY %%

% CIEKAWE PRZYKLADY WYWOLAN

%% Wykres 1.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 6;
% a_poly = rand(1, 7); % takie wartosci otrzymalysmy:
a_poly = [0.0357    0.8491    0.9340    0.6787    0.7577    0.7431    0.3922];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 2.
% Inne wspolczynniki niz na wykresie 1.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 6;
% a_poly = rand(1, 7); % inne wspolczynniki:
a_poly = [0.8235    0.6948    0.3171    0.9502    0.0344    0.4387    0.3816];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 3.
% Inny stopien wielomianu niz na wykresie 2.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 5;
a_poly = [0.8235    0.6948    0.3171    0.9502    0.0344    0.4387];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 4.
% Inny stopien wielomianu niz na wykresie 2.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 4;
a_poly = [0.8235    0.6948    0.3171    0.9502    0.0344];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 5.
% Inny stopien wielomianu niz na wykresie 2.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 3;
a_poly = [0.8235    0.6948    0.3171    0.9502];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 6.
% Inny stopien wielomianu niz na wykresie 2.
colormap(flipud(hot));
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 2;
a_poly = [0.8235    0.6948    0.3171];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 7.
% Mniejszy zakres wartosci rzeczywistych i urojonych punktow poczatkowych
% (niz na wykresie 2.)
colormap(flipud(hot));
a = -5;
b = 5;
c = -5;
d = 5;
m = 400;
n = 400;
n_poly = 6;
a_poly = [0.8235    0.6948    0.3171    0.9502    0.0344    0.4387    0.3816];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)

%% Wykres 8.
% Wiekszy zakres wartosci rzeczywistych i urojonych punktow poczatkowych
% (niz na wykresie 2.)
colormap(flipud(hot));
a = -500;
b = 500;
c = -500;
d = 500;
m = 400;
n = 400;
n_poly = 6;
a_poly = [0.8235    0.6948    0.3171    0.9502    0.0344    0.4387    0.3816];
A1 = image_newton(a, b, c, d, m, n, n_poly, a_poly);
imagesc(A1)


% ANALIZA BLEDOW NUMERYCZNYCH
% (dla wartosci miejsc zerowych wielomianu znalezionych przy pomocy metody 
% Newtona)

%% Macierz bledow bezwzglednych

% Ustawiamy parametry
colormap(summer);
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 4;
a_poly = [2 3 5 1 -7];
roots = [-0.98830 -0.20469 0.30747 0.95694]; % otrzymane miejsca zerowe 
% w Wolfram Alpha

% Generujemy macierz z miejscami zerowymi wielomianu otrzymanymi za pomoca
% metody Newtona dla kazdego z punktow poczatkowych.
A2 = image_newton_root(a, b, c, d, m, n, n_poly, a_poly);

% Generujemy macierz bledow bezwzglednych
A_absolute = zeros(n+1, m+1);
for k = 1:n+1
    for p = 1:m+1
        A_absolute(k,p) = min(abs(roots - A2(k,p)));
    end
end

imagesc(A_absolute)

%% Macierz bledow wzglednych

colormap(summer);
a = -50;
b = 50;
c = -50;
d = 50;
m = 400;
n = 400;
n_poly = 4;
a_poly = [2 3 5 1 -7];
roots = [-0.98830 -0.20469 0.30747 0.95694]; % otrzymane miejsca zerowe 
% w Wolfram Alpha

A2 = image_newton_root(a, b, c, d, m, n, n_poly, a_poly);

% Generujemy macierz bledow wzglednych
A_relative = zeros(n+1, m+1);
for k = 1:n+1
    for p = 1:m+1
        min_dist = 100;
        for r = 1:length(roots) % szukamy miejsca zerowego wyznaczonego 
            % analitycznie, znajdujacego sie najblizej miejsca zerowego 
            % z metody Newtona dla danego punktu macierzy
            if abs(roots(r) - A2(k,p)) < min_dist
                min_dist = abs(roots(r) - A2(k,p));
                root = roots(r);
            end
        end
        A_relative(k,p) = abs(root - A2(k,p)/root);
    end
end

imagesc(A_relative)