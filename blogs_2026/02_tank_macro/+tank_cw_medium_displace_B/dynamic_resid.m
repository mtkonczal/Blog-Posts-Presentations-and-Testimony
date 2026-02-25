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
    T = tank_cw_medium_displace_B.dynamic_resid_tt(T, y, x, params, steady_state, it_);
end
residual = zeros(60, 1);
    residual(1) = (y(17)) - (y(63)-params(2)*y(20));
    residual(2) = (y(17)) - (y(26)+y(77));
residual(3) = y(24);
residual(4) = y(23);
    residual(5) = (y(18)) - (y(63)-params(2)*y(21));
    residual(6) = (y(19)) - (params(3)*y(22));
    residual(7) = (y(30)) - (y(19)-y(18));
    residual(8) = (y(18)) - (y(26)+y(78)-y(42)*params(21)/T(8));
    residual(9) = (y(21)+y(42)/T(8)) - (params(28)/params(15)*(y(22)+y(31))*T(3)/T(8)-y(40)*(params(30)*T(6)+T(1)*params(31)*4*T(6)-params(31)*4*T(6))/T(8)+params(17)/params(15)*y(48)/T(8)+T(1)*y(6)/T(8));
    residual(10) = (y(46)) - (params(38)*(y(31)-y(56)));
    residual(11) = (y(47)) - (params(39)*y(45)+y(46)*(1-params(39)));
    residual(12) = (y(33)) - (y(47)*params(40)+(y(44)+y(25))*(1-(1-params(32)))+(1-params(32))*(y(62)+y(12)));
    residual(13) = (y(32)) - (y(33)*(1+T(5)/T(6)));
    residual(14) = (y(34)) - (y(33)-y(47)*params(41)-y(25));
    residual(15) = (y(31)) - (y(34)+y(35));
    residual(16) = (y(55)) - (y(33)+y(47)*params(42)-y(62)-y(12));
    residual(17) = (y(56)) - (y(35)+y(55));
    residual(18) = (y(32)) - (params(28)*(y(31)+y(25))*T(3)/T(6)+(y(12)+y(56)+y(62))*T(4)/T(6)+y(48)/T(6));
    residual(19) = (y(48)) - (y(49));
    residual(20) = (y(50)) - (y(31)+y(25));
    residual(21) = (y(51)) - (y(31)+y(25)-y(32));
    residual(22) = (y(61)) - (y(12)+y(56)+y(62)-y(32));
    residual(23) = (y(56)) - (1/params(25)*y(62));
    residual(24) = (y(59)) - (params(4)*(y(58)+y(64))+(1-params(4))*y(14));
    residual(25) = (y(27)-y(79)) - (params(1)*(1-params(4))*y(83)+(1-params(1)*(1-params(4)))*y(81)-y(60));
    residual(26) = (y(58)*(1+params(1))) - (1/(2*params(5))*(y(64)+y(60))+y(13)+params(1)*y(82));
    residual(27) = (y(57)) - (y(58));
    residual(28) = (y(54)) - (y(59));
    residual(29) = (y(32)) - (params(30)*y(37)+y(36)*T(7)+y(57)*params(4)*params(28)*T(2)/T(6)+y(62)*T(4)/T(6));
    residual(30) = (y(26)) - (y(27)-y(79));
    residual(31) = (y(36)) - (params(15)*y(21)+(1-params(15))*y(20));
    residual(32) = (y(25)) - (y(22));
    residual(33) = (y(28)) - (params(1)*y(79)+(y(35)+y(52))*T(9));
    residual(34) = (y(29)) - (params(1)*y(80)+(y(30)-y(31)+y(53))*T(10));
    residual(35) = (y(29)) - (y(31)-y(3));
    residual(36) = (y(27)) - (params(11)*y(2)+(1-params(11))*(y(28)*params(12)+y(32)*params(13))+x(it_, 3));
    residual(37) = (y(44)) - (params(8)*y(8)+(1-params(37))*x(it_, 2)+x(it_, 1));
    residual(38) = (y(45)) - (params(9)*y(9)+params(37)*x(it_, 2));
    residual(39) = (y(37)) - (params(10)*y(4)+x(it_, 4));
    residual(40) = (y(43)) - (T(1)*(y(7)+y(1))+y(37)*params(30)*T(6)/(params(31)*4*T(6))-y(38)*(params(30)*T(6)+T(1)*params(31)*4*T(6)-params(31)*4*T(6))/(params(31)*4*T(6)));
    residual(41) = (y(43)) - (y(41)+params(15)*y(42)/(params(31)*4*T(6)));
    residual(42) = (y(38)) - (params(18)*y(5)+y(7)*params(19)+y(37)*params(20));
    residual(43) = (y(52)) - (params(23)*y(10)+x(it_, 5));
    residual(44) = (y(53)) - (params(24)*y(11)+x(it_, 6));
    residual(45) = (y(63)) - (params(26)*y(15)+x(it_, 7));
    residual(46) = (y(64)) - (params(27)*y(16)+x(it_, 8));
    residual(47) = (y(39)) - (y(38));
    residual(48) = (y(40)) - (y(38));
    residual(49) = (y(65)) - (y(48)/T(6));
    residual(50) = (y(67)) - (y(43)/T(6));
    residual(51) = (y(69)) - (y(42)/T(6));
    residual(52) = (y(70)) - (y(41)/T(6));
    residual(53) = (y(66)) - (y(37)/T(6));
    residual(54) = (y(68)) - (y(38)/T(6));
    residual(55) = (y(71)) - (y(40)/T(6));
    residual(56) = (y(72)) - (y(39)/T(6));
    residual(57) = (y(73)) - ((1-params(15))*y(20));
    residual(58) = (y(74)) - (params(15)*y(21));
    residual(59) = (y(76)) - ((1-params(15))*y(70));
    residual(60) = (y(75)) - (params(15)*y(69));

end
