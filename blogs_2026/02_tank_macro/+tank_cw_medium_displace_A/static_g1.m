function g1 = static_g1(T, y, x, params, T_flag)
% function g1 = static_g1(T, y, x, params, T_flag)
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
%   g1
%

if T_flag
    T = tank_cw_medium_displace_A.static_g1_tt(T, y, x, params);
end
g1 = zeros(57, 57);
g1(1,1)=1;
g1(1,4)=params(2);
g1(1,44)=(-1);
g1(2,10)=(-1);
g1(3,8)=1;
g1(4,7)=1;
g1(5,2)=1;
g1(5,5)=params(2);
g1(5,44)=(-1);
g1(6,3)=1;
g1(6,6)=(-params(3));
g1(7,2)=1;
g1(7,3)=(-1);
g1(7,14)=1;
g1(8,10)=(-1);
g1(8,26)=params(20)/T(8);
g1(9,5)=1;
g1(9,6)=T(11);
g1(9,15)=T(11);
g1(9,24)=(T(6)*params(29)+T(1)*T(6)*params(30)*4-T(6)*params(30)*4)/T(8);
g1(9,26)=1/T(8)-T(1)/T(8);
g1(9,29)=(-(params(16)/params(14)/T(8)));
g1(10,9)=(-(1-(1-params(31))));
g1(10,17)=1;
g1(10,28)=(-(1-(1-params(31))));
g1(10,35)=(-(1-params(31)));
g1(10,43)=(-(1-params(31)));
g1(11,16)=1;
g1(11,17)=(-(1+T(5)/T(6)));
g1(12,9)=1;
g1(12,17)=(-1);
g1(12,18)=1;
g1(13,15)=1;
g1(13,18)=(-1);
g1(13,19)=(-1);
g1(14,17)=(-1);
g1(14,35)=1;
g1(14,36)=1;
g1(14,43)=1;
g1(15,19)=(-1);
g1(15,36)=(-1);
g1(15,37)=1;
g1(16,9)=(-(params(27)*T(3)/T(6)));
g1(16,15)=(-(params(27)*T(3)/T(6)));
g1(16,16)=1;
g1(16,29)=T(12);
g1(16,35)=T(13);
g1(16,37)=T(13);
g1(16,43)=T(13);
g1(17,29)=1;
g1(17,30)=(-1);
g1(18,9)=(-1);
g1(18,15)=(-1);
g1(18,31)=1;
g1(19,9)=(-1);
g1(19,15)=(-1);
g1(19,16)=1;
g1(19,32)=1;
g1(20,16)=1;
g1(20,35)=(-1);
g1(20,37)=(-1);
g1(20,42)=1;
g1(20,43)=(-1);
g1(21,37)=1;
g1(21,43)=(-(1/params(24)));
g1(22,39)=(-params(4));
g1(22,40)=1-(1-params(4));
g1(22,45)=(-params(4));
g1(23,11)=1;
g1(23,12)=(-1);
g1(23,37)=(-(1-params(1)*(1-params(4))));
g1(23,41)=(-(params(1)*(1-params(4))-1));
g1(24,41)=(-(1/(2*params(5))));
g1(24,45)=(-(1/(2*params(5))));
g1(25,38)=1;
g1(25,39)=(-1);
g1(26,35)=1;
g1(26,40)=(-1);
g1(27,16)=1;
g1(27,20)=(-T(7));
g1(27,21)=(-params(29));
g1(27,38)=(-(params(4)*params(27)*T(2)/T(6)));
g1(27,43)=T(13);
g1(28,10)=1;
g1(28,11)=(-1);
g1(28,12)=1;
g1(29,4)=(-(1-params(14)));
g1(29,5)=(-params(14));
g1(29,20)=1;
g1(30,6)=(-1);
g1(30,9)=1;
g1(31,12)=1-params(1);
g1(31,19)=(-T(9));
g1(31,33)=(-T(9));
g1(32,13)=1-params(1);
g1(32,14)=(-T(10));
g1(32,15)=T(10);
g1(32,34)=(-T(10));
g1(33,13)=1;
g1(34,11)=1-params(10);
g1(34,12)=(-((1-params(10))*params(11)));
g1(34,16)=(-((1-params(10))*params(12)));
g1(35,28)=1-params(8);
g1(36,21)=1-params(9);
g1(37,10)=(-T(1));
g1(37,21)=(-(T(6)*params(29)/(T(6)*params(30)*4)));
g1(37,22)=(T(6)*params(29)+T(1)*T(6)*params(30)*4-T(6)*params(30)*4)/(T(6)*params(30)*4);
g1(37,27)=1-T(1);
g1(38,25)=(-1);
g1(38,26)=(-(params(14)/(T(6)*params(30)*4)));
g1(38,27)=1;
g1(39,21)=(-params(19));
g1(39,22)=1-params(17);
g1(39,27)=(-params(18));
g1(40,33)=1-params(22);
g1(41,34)=1-params(22);
g1(42,44)=1-params(25);
g1(43,45)=1-params(26);
g1(44,22)=(-1);
g1(44,23)=1;
g1(45,22)=(-1);
g1(45,24)=1;
g1(46,29)=T(12);
g1(46,46)=1;
g1(47,27)=T(12);
g1(47,48)=1;
g1(48,26)=T(12);
g1(48,50)=1;
g1(49,25)=T(12);
g1(49,51)=1;
g1(50,21)=T(12);
g1(50,47)=1;
g1(51,22)=T(12);
g1(51,49)=1;
g1(52,24)=T(12);
g1(52,52)=1;
g1(53,23)=T(12);
g1(53,53)=1;
g1(54,4)=(-(1-params(14)));
g1(54,54)=1;
g1(55,5)=(-params(14));
g1(55,55)=1;
g1(56,51)=(-(1-params(14)));
g1(56,57)=1;
g1(57,50)=(-params(14));
g1(57,56)=1;

end
