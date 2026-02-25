function [T_order, T] = static_resid_tt(y, x, params, T_order, T)
if T_order >= 0
    return
end
T_order = 0;
if size(T, 1) < 10
    T = [T; NaN(10 - size(T, 1), 1)];
end
T(1) = 1/params(1);
T(2) = ((params(4)+T(1)-1)/((params(6)-1)/params(6)*(1-params(31))))^(1/(1-params(31)-1));
T(3) = (params(6)-1)/params(6)*(1-(1-params(31)))*(params(27)/(params(27)*T(2)))^(-(1-params(31)));
T(4) = (params(4)+T(1)-1)*params(27)*T(2);
T(5) = params(27)*((T(2))^(1-params(31))-(T(3)+T(4)/params(27)));
T(6) = params(27)^(1-(1-params(31)))*(params(27)*T(2))^(1-params(31))-T(5);
T(7) = 1-params(29)-params(4)*params(27)*T(2)/T(6);
T(8) = T(6)*T(7);
T(9) = (params(6)-1)/((params(6)-1)*params(34)/((1-params(34))*(1-params(1)*params(34))));
T(10) = (params(7)-1)/(params(35)*(params(7)-1)/((1-params(35))*(1-params(1)*params(35))));
end
