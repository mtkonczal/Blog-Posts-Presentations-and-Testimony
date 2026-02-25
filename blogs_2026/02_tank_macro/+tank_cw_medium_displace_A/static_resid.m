function residual = static_resid(T, y, x, params, T_flag)
% function residual = static_resid(T, y, x, params, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%                                              to evaluate the model
%   T_flag    boolean                 boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   residual
%

if T_flag
    T = tank_cw_medium_displace_A.static_resid_tt(T, y, x, params);
end
residual = zeros(57, 1);
    residual(1) = (y(1)) - (y(44)-params(2)*y(4));
    residual(2) = (y(1)) - (y(1)+y(10));
residual(3) = y(8);
residual(4) = y(7);
    residual(5) = (y(2)) - (y(44)-params(2)*y(5));
    residual(6) = (y(3)) - (params(3)*y(6));
    residual(7) = (y(14)) - (y(3)-y(2));
    residual(8) = (y(2)) - (y(10)+y(2)-y(26)*params(20)/T(8));
    residual(9) = (y(5)+y(26)/T(8)) - (params(27)/params(14)*T(3)*(y(6)+y(15))/T(8)-y(24)*(T(6)*params(29)+T(1)*T(6)*params(30)*4-T(6)*params(30)*4)/T(8)+params(16)/params(14)*y(29)/T(8)+T(1)*y(26)/T(8));
    residual(10) = (y(17)) - ((1-(1-params(31)))*(y(28)+y(9))+(1-params(31))*(y(43)+y(35)));
    residual(11) = (y(16)) - (y(17)*(1+T(5)/T(6)));
    residual(12) = (y(18)) - (y(17)-y(9));
    residual(13) = (y(15)) - (y(18)+y(19));
    residual(14) = (y(36)) - (y(17)-y(43)-y(35));
    residual(15) = (y(37)) - (y(19)+y(36));
    residual(16) = (y(16)) - (params(27)*T(3)*(y(15)+y(9))/T(6)+T(4)*(y(35)+y(43)+y(37))/T(6)+y(29)/T(6));
    residual(17) = (y(29)) - (y(30));
    residual(18) = (y(31)) - (y(15)+y(9));
    residual(19) = (y(32)) - (y(15)+y(9)-y(16));
    residual(20) = (y(42)) - (y(35)+y(43)+y(37)-y(16));
    residual(21) = (y(37)) - (1/params(24)*y(43));
    residual(22) = (y(40)) - (params(4)*(y(39)+y(45))+y(40)*(1-params(4)));
    residual(23) = (y(11)-y(12)) - (params(1)*(1-params(4))*y(41)+y(37)*(1-params(1)*(1-params(4)))-y(41));
    residual(24) = (y(39)*(1+params(1))) - (1/(2*params(5))*(y(45)+y(41))+y(39)+params(1)*y(39));
    residual(25) = (y(38)) - (y(39));
    residual(26) = (y(35)) - (y(40));
    residual(27) = (y(16)) - (params(29)*y(21)+T(7)*y(20)+params(4)*params(27)*T(2)/T(6)*y(38)+y(43)*T(4)/T(6));
    residual(28) = (y(10)) - (y(11)-y(12));
    residual(29) = (y(20)) - (params(14)*y(5)+(1-params(14))*y(4));
    residual(30) = (y(9)) - (y(6));
    residual(31) = (y(12)) - (params(1)*y(12)+T(9)*(y(19)+y(33)));
    residual(32) = (y(13)) - (params(1)*y(13)+T(10)*(y(14)-y(15)+y(34)));
residual(33) = y(13);
    residual(34) = (y(11)) - (y(11)*params(10)+(1-params(10))*(y(12)*params(11)+y(16)*params(12))+x(2));
    residual(35) = (y(28)) - (y(28)*params(8)+x(1));
    residual(36) = (y(21)) - (y(21)*params(9)+x(3));
    residual(37) = (y(27)) - (T(1)*(y(10)+y(27))+T(6)*params(29)*y(21)/(T(6)*params(30)*4)-(T(6)*params(29)+T(1)*T(6)*params(30)*4-T(6)*params(30)*4)*y(22)/(T(6)*params(30)*4));
    residual(38) = (y(27)) - (y(25)+params(14)*y(26)/(T(6)*params(30)*4));
    residual(39) = (y(22)) - (y(22)*params(17)+y(27)*params(18)+y(21)*params(19));
    residual(40) = (y(33)) - (y(33)*params(22)+x(4));
    residual(41) = (y(34)) - (y(34)*params(22)+x(5));
    residual(42) = (y(44)) - (y(44)*params(25)+x(6));
    residual(43) = (y(45)) - (y(45)*params(26)+x(7));
    residual(44) = (y(23)) - (y(22));
    residual(45) = (y(24)) - (y(22));
    residual(46) = (y(46)) - (y(29)/T(6));
    residual(47) = (y(48)) - (y(27)/T(6));
    residual(48) = (y(50)) - (y(26)/T(6));
    residual(49) = (y(51)) - (y(25)/T(6));
    residual(50) = (y(47)) - (y(21)/T(6));
    residual(51) = (y(49)) - (y(22)/T(6));
    residual(52) = (y(52)) - (y(24)/T(6));
    residual(53) = (y(53)) - (y(23)/T(6));
    residual(54) = (y(54)) - ((1-params(14))*y(4));
    residual(55) = (y(55)) - (params(14)*y(5));
    residual(56) = (y(57)) - ((1-params(14))*y(51));
    residual(57) = (y(56)) - (params(14)*y(50));

end
