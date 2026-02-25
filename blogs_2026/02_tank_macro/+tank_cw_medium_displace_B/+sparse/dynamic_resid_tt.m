function [T_order, T] = dynamic_resid_tt(y, x, params, steady_state, T_order, T)
if T_order >= 0
    return
end
T_order = 0;
if size(T, 1) < 10
    T = [T; NaN(10 - size(T, 1), 1)];
end
T(1) = 1/params(1);
T(2) = ((params(4)+T(1)-1)/((params(6)-1)/params(6)*(1-params(32))))^(1/(1-params(32)-1));
T(3) = (params(6)-1)/params(6)*(1-(1-params(32)))*(params(28)/(params(28)*T(2)))^(-(1-params(32)));
T(4) = (params(4)+T(1)-1)*params(28)*T(2);
T(5) = params(28)*((T(2))^(1-params(32))-(T(3)+T(4)/params(28)));
T(6) = params(28)^(1-(1-params(32)))*(params(28)*T(2))^(1-params(32))-T(5);
T(7) = 1-params(30)-params(4)*params(28)*T(2)/T(6);
T(8) = T(6)*T(7);
T(9) = (params(6)-1)/((params(6)-1)*params(35)/((1-params(35))*(1-params(1)*params(35))));
T(10) = (params(7)-1)/(params(36)*(params(7)-1)/((1-params(36))*(1-params(1)*params(36))));
end
