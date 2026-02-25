function [T_order, T] = static_g1_tt(y, x, params, T_order, T)
if T_order >= 1
    return
end
[T_order, T] = sticky_price_check.sparse.static_resid_tt(y, x, params, T_order, T);
T_order = 1;
if size(T, 1) < 11
    T = [T; NaN(11 - size(T, 1), 1)];
end
T(9) = (-(params(25)/params(13)*T(3)/T(8)));
T(10) = (-(1/T(6)));
T(11) = (-(T(4)/T(6)));
end
