function [T_order, T] = static_g1_tt(y, x, params, T_order, T)
if T_order >= 1
    return
end
[T_order, T] = tank_cw_medium_displace_B.sparse.static_resid_tt(y, x, params, T_order, T);
T_order = 1;
if size(T, 1) < 13
    T = [T; NaN(13 - size(T, 1), 1)];
end
T(11) = (-(params(28)/params(15)*T(3)/T(8)));
T(12) = (-(1/T(6)));
T(13) = (-(T(4)/T(6)));
end
