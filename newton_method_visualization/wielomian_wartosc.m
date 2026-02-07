function [value] = wielomian_wartosc(z, n, a)
    % Ta funkcja oblicza wartosc wielomianu w stopnia n w punkcie z (liczba
    % zespolona)
    % a - wektor wspolczynnikow a_k (k = 0:n)
    if n == 0
        value = 2*a(1);
        return
    end    
    t = 1; % T_0(z) pierwsza wartosc wielomianu Czebyszewa T
    u = 1; % U_0(z) pierwsza wartosc wielomianu Czebyszewa U
    value = a(1)*(t + u); % wartosc wielomianu w_0
    t_prev = t;
    u_prev = u;
    t = z; % T_1(z) druga wartosc wielomianu Czebyszewa T
    u = 2*z ; % U_1(z) druga wartosc wielomianu Czebyszewa U
    value = value + a(2)*(t + u); % wartosc wielomianu w_1
    for i = 3:n+1 % w tej petli obliczamy wartosc wielomianu w_n dodajac kolejne skladniki
        t_pprev = t_prev;
        t_prev = t;
        t = 2*z*t_prev - t_pprev;
        u_pprev = u_prev;
        u_prev = u;
        u = 2*z*u_prev - u_pprev;
        value = value + a(i)*(t + u);
    end
end