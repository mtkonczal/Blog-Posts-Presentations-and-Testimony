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
    T = tank_cw_medium_displace_B.dynamic_g1_tt(T, y, x, params, steady_state, it_);
end
g1 = zeros(60, 91);
g1(1,17)=1;
g1(1,20)=params(2);
g1(1,63)=(-1);
g1(2,17)=1;
g1(2,77)=(-1);
g1(2,26)=(-1);
g1(3,24)=1;
g1(4,23)=1;
g1(5,18)=1;
g1(5,21)=params(2);
g1(5,63)=(-1);
g1(6,19)=1;
g1(6,22)=(-params(3));
g1(7,18)=1;
g1(7,19)=(-1);
g1(7,30)=1;
g1(8,18)=1;
g1(8,78)=(-1);
g1(8,26)=(-1);
g1(8,42)=params(21)/T(8);
g1(9,21)=1;
g1(9,22)=T(11);
g1(9,31)=T(11);
g1(9,40)=(params(30)*T(6)+T(1)*params(31)*4*T(6)-params(31)*4*T(6))/T(8);
g1(9,6)=(-(T(1)/T(8)));
g1(9,42)=1/T(8);
g1(9,48)=(-(params(17)/params(15)/T(8)));
g1(10,31)=(-params(38));
g1(10,46)=1;
g1(10,56)=params(38);
g1(11,45)=(-params(39));
g1(11,46)=(-(1-params(39)));
g1(11,47)=1;
g1(12,25)=(-(1-(1-params(32))));
g1(12,33)=1;
g1(12,44)=(-(1-(1-params(32))));
g1(12,47)=(-params(40));
g1(12,12)=(-(1-params(32)));
g1(12,62)=(-(1-params(32)));
g1(13,32)=1;
g1(13,33)=(-(1+T(5)/T(6)));
g1(14,25)=1;
g1(14,33)=(-1);
g1(14,34)=1;
g1(14,47)=params(41);
g1(15,31)=1;
g1(15,34)=(-1);
g1(15,35)=(-1);
g1(16,33)=(-1);
g1(16,47)=(-params(42));
g1(16,12)=1;
g1(16,55)=1;
g1(16,62)=1;
g1(17,35)=(-1);
g1(17,55)=(-1);
g1(17,56)=1;
g1(18,25)=(-(params(28)*T(3)/T(6)));
g1(18,31)=(-(params(28)*T(3)/T(6)));
g1(18,32)=1;
g1(18,48)=T(12);
g1(18,12)=T(13);
g1(18,56)=T(13);
g1(18,62)=T(13);
g1(19,48)=1;
g1(19,49)=(-1);
g1(20,25)=(-1);
g1(20,31)=(-1);
g1(20,50)=1;
g1(21,25)=(-1);
g1(21,31)=(-1);
g1(21,32)=1;
g1(21,51)=1;
g1(22,32)=1;
g1(22,12)=(-1);
g1(22,56)=(-1);
g1(22,61)=1;
g1(22,62)=(-1);
g1(23,56)=1;
g1(23,62)=(-(1/params(25)));
g1(24,58)=(-params(4));
g1(24,14)=(-(1-params(4)));
g1(24,59)=1;
g1(24,64)=(-params(4));
g1(25,27)=1;
g1(25,79)=(-1);
g1(25,81)=(-(1-params(1)*(1-params(4))));
g1(25,60)=1;
g1(25,83)=(-(params(1)*(1-params(4))));
g1(26,13)=(-1);
g1(26,58)=1+params(1);
g1(26,82)=(-params(1));
g1(26,60)=(-(1/(2*params(5))));
g1(26,64)=(-(1/(2*params(5))));
g1(27,57)=1;
g1(27,58)=(-1);
g1(28,54)=1;
g1(28,59)=(-1);
g1(29,32)=1;
g1(29,36)=(-T(7));
g1(29,37)=(-params(30));
g1(29,57)=(-(params(4)*params(28)*T(2)/T(6)));
g1(29,62)=T(13);
g1(30,26)=1;
g1(30,27)=(-1);
g1(30,79)=1;
g1(31,20)=(-(1-params(15)));
g1(31,21)=(-params(15));
g1(31,36)=1;
g1(32,22)=(-1);
g1(32,25)=1;
g1(33,28)=1;
g1(33,79)=(-params(1));
g1(33,35)=(-T(9));
g1(33,52)=(-T(9));
g1(34,29)=1;
g1(34,80)=(-params(1));
g1(34,30)=(-T(10));
g1(34,31)=T(10);
g1(34,53)=(-T(10));
g1(35,29)=1;
g1(35,3)=1;
g1(35,31)=(-1);
g1(36,2)=(-params(11));
g1(36,27)=1;
g1(36,28)=(-((1-params(11))*params(12)));
g1(36,32)=(-((1-params(11))*params(13)));
g1(36,86)=(-1);
g1(37,8)=(-params(8));
g1(37,44)=1;
g1(37,84)=(-1);
g1(37,85)=(-(1-params(37)));
g1(38,9)=(-params(9));
g1(38,45)=1;
g1(38,85)=(-params(37));
g1(39,4)=(-params(10));
g1(39,37)=1;
g1(39,87)=(-1);
g1(40,1)=(-T(1));
g1(40,37)=(-(params(30)*T(6)/(params(31)*4*T(6))));
g1(40,38)=(params(30)*T(6)+T(1)*params(31)*4*T(6)-params(31)*4*T(6))/(params(31)*4*T(6));
g1(40,7)=(-T(1));
g1(40,43)=1;
g1(41,41)=(-1);
g1(41,42)=(-(params(15)/(params(31)*4*T(6))));
g1(41,43)=1;
g1(42,37)=(-params(20));
g1(42,5)=(-params(18));
g1(42,38)=1;
g1(42,7)=(-params(19));
g1(43,10)=(-params(23));
g1(43,52)=1;
g1(43,88)=(-1);
g1(44,11)=(-params(24));
g1(44,53)=1;
g1(44,89)=(-1);
g1(45,15)=(-params(26));
g1(45,63)=1;
g1(45,90)=(-1);
g1(46,16)=(-params(27));
g1(46,64)=1;
g1(46,91)=(-1);
g1(47,38)=(-1);
g1(47,39)=1;
g1(48,38)=(-1);
g1(48,40)=1;
g1(49,48)=T(12);
g1(49,65)=1;
g1(50,43)=T(12);
g1(50,67)=1;
g1(51,42)=T(12);
g1(51,69)=1;
g1(52,41)=T(12);
g1(52,70)=1;
g1(53,37)=T(12);
g1(53,66)=1;
g1(54,38)=T(12);
g1(54,68)=1;
g1(55,40)=T(12);
g1(55,71)=1;
g1(56,39)=T(12);
g1(56,72)=1;
g1(57,20)=(-(1-params(15)));
g1(57,73)=1;
g1(58,21)=(-params(15));
g1(58,74)=1;
g1(59,70)=(-(1-params(15)));
g1(59,76)=1;
g1(60,69)=(-params(15));
g1(60,75)=1;

end
