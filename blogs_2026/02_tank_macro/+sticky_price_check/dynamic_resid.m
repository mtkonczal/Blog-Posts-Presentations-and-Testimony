function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
% function residual = dynamic_resid(T, y, x, params, steady_state, it_, T_flag)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T             [#temp variables by 1]     double   vector of temporary terms to be filled by function
%   y             [#dynamic variables by 1]  double   vector of endogenous variables in the order stored
%                                                     in M_.lead_lag_incidence; see the Manual
%   x             [nperiods by M_.exo_nbr]   double   matrix of exogenous variables (in declaration order)
%                                                     for all simulation periods
%   steady_state  [M_.endo_nbr by 1]         double   vector of steady state values
%   params        [M_.param_nbr by 1]        double   vector of parameter values in declaration order
%   it_           scalar                     double   time period for exogenous variables for which
%                                                     to evaluate the model
%   T_flag        boolean                    boolean  flag saying whether or not to calculate temporary terms
%
% Output:
%   residual
%

if T_flag
    T = sticky_price_check.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(57, 1);
    residual(1) = (y(16)) - (y(59)-params(2)*y(19));
    residual(2) = (y(16)) - (y(25)+y(73));
residual(3) = y(23);
residual(4) = y(22);
    residual(5) = (y(17)) - (y(59)-params(2)*y(20));
    residual(6) = (y(18)) - (params(3)*y(21));
    residual(7) = (y(29)) - (y(18)-y(17));
    residual(8) = (y(17)) - (y(25)+y(74)-y(41)*params(18)/T(8));
    residual(9) = (y(20)+y(41)*1/T(8)) - (params(25)/params(13)*(y(21)+y(30))*T(3)/T(8)-y(39)*(params(27)*T(6)+T(1)*params(28)*4*T(6)-params(28)*4*T(6))/T(8)+params(14)/params(13)*y(44)/T(8)+T(1)*y(6)/T(8));
    residual(10) = (y(32)) - ((y(43)+y(24))*(1-(1-params(29)))+(1-params(29))*(y(58)+y(11)));
    residual(11) = (y(31)) - (y(32)*(1+T(5)/T(6)));
    residual(12) = (y(33)) - (y(32)-y(24));
    residual(13) = (y(30)) - (y(33)+y(34));
    residual(14) = (y(51)) - (y(32)-y(58)-y(11));
    residual(15) = (y(52)) - (y(34)+y(51));
    residual(16) = (y(31)) - (params(25)*(y(30)+y(24))*T(3)/T(6)+(y(11)+y(58)+y(52))*T(4)/T(6)+y(44)/T(6));
    residual(17) = (y(44)) - (y(45));
    residual(18) = (y(46)) - (y(30)+y(24));
    residual(19) = (y(47)) - (y(30)+y(24)-y(31));
    residual(20) = (y(57)) - (y(11)+y(58)+y(52)-y(31));
    residual(21) = (y(52)) - (1/params(22)*y(58));
    residual(22) = (y(55)) - (params(4)*(y(54)+y(60))+(1-params(4))*y(13));
    residual(23) = (y(26)-y(75)) - (params(1)*(1-params(4))*y(79)+(1-params(1)*(1-params(4)))*y(77)-y(56));
    residual(24) = (y(54)*(1+params(1))) - (1/(2*params(5))*(y(60)+y(56))+y(12)+params(1)*y(78));
    residual(25) = (y(53)) - (y(54));
    residual(26) = (y(50)) - (y(55));
    residual(27) = (y(31)) - (params(27)*y(36)+y(35)*T(7)+y(53)*params(4)*params(25)*T(2)/T(6)+y(58)*T(4)/T(6));
    residual(28) = (y(25)) - (y(26)-y(75));
    residual(29) = (y(35)) - (params(13)*y(20)+(1-params(13))*y(19));
    residual(30) = (y(24)) - (y(21));
    residual(31) = (y(27)) - (params(1)*y(75)+params(30)*(y(34)+y(48)));
    residual(32) = (y(28)) - (params(1)*y(76)+params(31)*(y(29)-y(30)+y(49)));
    residual(33) = (y(28)) - (y(30)-y(3));
    residual(34) = (y(26)) - (params(10)*y(2)+(1-params(10))*(y(27)*params(11)+y(31)*params(12))+x(it_, 2));
    residual(35) = (y(43)) - (params(8)*y(8)+x(it_, 1));
    residual(36) = (y(36)) - (params(9)*y(4)+x(it_, 3));
    residual(37) = (y(42)) - (T(1)*(y(7)+y(1))+y(36)*params(27)*T(6)/(params(28)*4*T(6))-y(37)*(params(27)*T(6)+T(1)*params(28)*4*T(6)-params(28)*4*T(6))/(params(28)*4*T(6)));
    residual(38) = (y(42)) - (y(40)+params(13)*y(41)/(params(28)*4*T(6)));
    residual(39) = (y(37)) - (params(15)*y(5)+y(7)*params(16)+y(36)*params(17));
    residual(40) = (y(48)) - (params(20)*y(9)+x(it_, 4));
    residual(41) = (y(49)) - (params(21)*y(10)+x(it_, 5));
    residual(42) = (y(59)) - (params(23)*y(14)+x(it_, 6));
    residual(43) = (y(60)) - (params(24)*y(15)+x(it_, 7));
    residual(44) = (y(38)) - (y(37));
    residual(45) = (y(39)) - (y(37));
    residual(46) = (y(61)) - (y(44)/T(6));
    residual(47) = (y(63)) - (y(42)/T(6));
    residual(48) = (y(65)) - (y(41)/T(6));
    residual(49) = (y(66)) - (y(40)/T(6));
    residual(50) = (y(62)) - (y(36)/T(6));
    residual(51) = (y(64)) - (y(37)/T(6));
    residual(52) = (y(67)) - (y(39)/T(6));
    residual(53) = (y(68)) - (y(38)/T(6));
    residual(54) = (y(69)) - ((1-params(13))*y(19));
    residual(55) = (y(70)) - (params(13)*y(20));
    residual(56) = (y(72)) - ((1-params(13))*y(66));
    residual(57) = (y(71)) - (params(13)*y(65));

end
