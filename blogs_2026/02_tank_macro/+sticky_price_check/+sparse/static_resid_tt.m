function [T_order, T] = static_resid_tt(y, x, params, T_order, T)
if T_order >= 0
    return
end
T_order = 0;
if size(T, 1) < 8
    T = [T; NaN(8 - size(T, 1), 1)];
end
T(1) = 1/params(1);
T(2) = ((params(4)+T(1)-1)/((params(6)-1)/params(6)*(1-params(29))))^(1/(1-params(29)-1));
T(3) = (params(6)-1)/params(6)*(1-(1-params(29)))*(params(25)/(params(25)*T(2)))^(-(1-params(29)));
T(4) = (params(4)+T(1)-1)*params(25)*T(2);
T(5) = params(25)*((T(2))^(1-params(29))-(T(3)+T(4)/params(25)));
T(6) = params(25)^(1-(1-params(29)))*(params(25)*T(2))^(1-params(29))-T(5);
T(7) = 1-params(27)-params(4)*params(25)*T(2)/T(6);
T(8) = T(6)*T(7);
end
