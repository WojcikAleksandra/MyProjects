function [diff_value] = wielomian_pochodna(z, n, a)
% Ta funkcja oblicza pochodna wielomianu w stopnia n w punkcie z (liczba
% zespolona)
% a - wektor wspolczynnikow a_k (k = 0:n)
if n == 0
    diff_value = 0;
    return
end    
h=2^(-12) + 2^(-12)*j; % bardzo mala liczba, z ktorej korzystamy przy obliczaniu pochodnej
t = 1; % T_0(z) pierwsza wartosc wielomianu Czebyszewa T
u = 1; % U_0(z) pierwsza wartosc wielomianu Czebyszewa U
t_prev = t;
u_prev = u;
t_diff_prev = 0; % pochodna od t_prev
u_diff_prev = 0; % pochodna od u_prev
t = z; % T_1(z) druga wartosc wielomianu Czebyszewa T
u = 2*z ; % U_1(z) druga wartosc wielomianu Czebyszewa U
t_diff = ((z + h) - z)/h; % pochodna T_1 w punkcie z
u_diff = (2*(z + h) - 2*z)/h; % pochodna U_1 w punkcie z
diff_value = a(1)*(t_diff_prev + u_diff_prev) + a(2)*(t_diff + u_diff); % pochodna w_1 w punkcie z
    for i = 3:n+1 % w petli obliczamy wartosc pochodnej wielomianu w_n w punkcie z dodajac kolejne pochodne skladnikow
        t_pprev = t_prev;
        t_prev = t;
        t = 2*z*t_prev - t_pprev;
        t_diff_pprev = t_diff_prev;
        t_diff_prev = t_diff;
        t_diff = 2*t_prev + 2*z*t_diff_prev - t_diff_pprev;
        u_pprev = u_prev;
        u_prev = u;
        u = 2*z*u_prev - u_pprev;
        u_diff_pprev = u_diff_prev;
        u_diff_prev = u_diff;
        u_diff = 2*u_prev + 2*z*u_diff_prev - u_diff_pprev;
        diff_value = diff_value + a(i)*(t_diff + u_diff);
    end
end