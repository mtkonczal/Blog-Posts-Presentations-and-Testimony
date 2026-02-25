function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
% function g1 = dynamic_g1(T, y, x, params, steady_state, it_, T_flag)
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
%   g1
%

if T_flag
    T = tank_cw_medium_displace_A.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(57, 86);
g1(1,16)=1;
g1(1,19)=params(2);
g1(1,59)=(-1);
g1(2,16)=1;
g1(2,73)=(-1);
g1(2,25)=(-1);
g1(3,23)=1;
g1(4,22)=1;
g1(5,17)=1;
g1(5,20)=params(2);
g1(5,59)=(-1);
g1(6,18)=1;
g1(6,21)=(-params(3));
g1(7,17)=1;
g1(7,18)=(-1);
g1(7,29)=1;
g1(8,17)=1;
g1(8,74)=(-1);
g1(8,25)=(-1);
g1(8,41)=params(20)/T(8);
g1(9,20)=1;
g1(9,21)=T(11);
g1(9,30)=T(11);
g1(9,39)=(params(29)*T(6)+T(1)*params(30)*4*T(6)-params(30)*4*T(6))/T(8);
g1(9,6)=(-(T(1)/T(8)));
g1(9,41)=1/T(8);
g1(9,44)=(-(params(16)/params(14)/T(8)));
g1(10,24)=(-(1-(1-params(31))));
g1(10,32)=1;
g1(10,43)=(-(1-(1-params(31))));
g1(10,11)=(-(1-params(31)));
g1(10,58)=(-(1-params(31)));
g1(11,31)=1;
g1(11,32)=(-(1+T(5)/T(6)));
g1(12,24)=1;
g1(12,32)=(-1);
g1(12,33)=1;
g1(13,30)=1;
g1(13,33)=(-1);
g1(13,34)=(-1);
g1(14,32)=(-1);
g1(14,11)=1;
g1(14,51)=1;
g1(14,58)=1;
g1(15,34)=(-1);
g1(15,51)=(-1);
g1(15,52)=1;
g1(16,24)=(-(params(27)*T(3)/T(6)));
g1(16,30)=(-(params(27)*T(3)/T(6)));
g1(16,31)=1;
g1(16,44)=T(12);
g1(16,11)=T(13);
g1(16,52)=T(13);
g1(16,58)=T(13);
g1(17,44)=1;
g1(17,45)=(-1);
g1(18,24)=(-1);
g1(18,30)=(-1);
g1(18,46)=1;
g1(19,24)=(-1);
g1(19,30)=(-1);
g1(19,31)=1;
g1(19,47)=1;
g1(20,31)=1;
g1(20,11)=(-1);
g1(20,52)=(-1);
g1(20,57)=1;
g1(20,58)=(-1);
g1(21,52)=1;
g1(21,58)=(-(1/params(24)));
g1(22,54)=(-params(4));
g1(22,13)=(-(1-params(4)));
g1(22,55)=1;
g1(22,60)=(-params(4));
g1(23,26)=1;
g1(23,75)=(-1);
g1(23,77)=(-(1-params(1)*(1-params(4))));
g1(23,56)=1;
g1(23,79)=(-(params(1)*(1-params(4))));
g1(24,12)=(-1);
g1(24,54)=1+params(1);
g1(24,78)=(-params(1));
g1(24,56)=(-(1/(2*params(5))));
g1(24,60)=(-(1/(2*params(5))));
g1(25,53)=1;
g1(25,54)=(-1);
g1(26,50)=1;
g1(26,55)=(-1);
g1(27,31)=1;
g1(27,35)=(-T(7));
g1(27,36)=(-params(29));
g1(27,53)=(-(params(4)*params(27)*T(2)/T(6)));
g1(27,58)=T(13);
g1(28,25)=1;
g1(28,26)=(-1);
g1(28,75)=1;
g1(29,19)=(-(1-params(14)));
g1(29,20)=(-params(14));
g1(29,35)=1;
g1(30,21)=(-1);
g1(30,24)=1;
g1(31,27)=1;
g1(31,75)=(-params(1));
g1(31,34)=(-T(9));
g1(31,48)=(-T(9));
g1(32,28)=1;
g1(32,76)=(-params(1));
g1(32,29)=(-T(10));
g1(32,30)=T(10);
g1(32,49)=(-T(10));
g1(33,28)=1;
g1(33,3)=1;
g1(33,30)=(-1);
g1(34,2)=(-params(10));
g1(34,26)=1;
g1(34,27)=(-((1-params(10))*params(11)));
g1(34,31)=(-((1-params(10))*params(12)));
g1(34,81)=(-1);
g1(35,8)=(-params(8));
g1(35,43)=1;
g1(35,80)=(-1);
g1(36,4)=(-params(9));
g1(36,36)=1;
g1(36,82)=(-1);
g1(37,1)=(-T(1));
g1(37,36)=(-(params(29)*T(6)/(params(30)*4*T(6))));
g1(37,37)=(params(29)*T(6)+T(1)*params(30)*4*T(6)-params(30)*4*T(6))/(params(30)*4*T(6));
g1(37,7)=(-T(1));
g1(37,42)=1;
g1(38,40)=(-1);
g1(38,41)=(-(params(14)/(params(30)*4*T(6))));
g1(38,42)=1;
g1(39,36)=(-params(19));
g1(39,5)=(-params(17));
g1(39,37)=1;
g1(39,7)=(-params(18));
g1(40,9)=(-params(22));
g1(40,48)=1;
g1(40,83)=(-1);
g1(41,10)=(-params(22));
g1(41,49)=1;
g1(41,84)=(-1);
g1(42,14)=(-params(25));
g1(42,59)=1;
g1(42,85)=(-1);
g1(43,15)=(-params(26));
g1(43,60)=1;
g1(43,86)=(-1);
g1(44,37)=(-1);
g1(44,38)=1;
g1(45,37)=(-1);
g1(45,39)=1;
g1(46,44)=T(12);
g1(46,61)=1;
g1(47,42)=T(12);
g1(47,63)=1;
g1(48,41)=T(12);
g1(48,65)=1;
g1(49,40)=T(12);
g1(49,66)=1;
g1(50,36)=T(12);
g1(50,62)=1;
g1(51,37)=T(12);
g1(51,64)=1;
g1(52,39)=T(12);
g1(52,67)=1;
g1(53,38)=T(12);
g1(53,68)=1;
g1(54,19)=(-(1-params(14)));
g1(54,69)=1;
g1(55,20)=(-params(14));
g1(55,70)=1;
g1(56,66)=(-(1-params(14)));
g1(56,72)=1;
g1(57,65)=(-params(14));
g1(57,71)=1;

end
