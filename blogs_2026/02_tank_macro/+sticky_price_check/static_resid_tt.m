function T = static_resid_tt(T, y, x, params)
% function T = static_resid_tt(T, y, x, params)
%
% File created by Dynare Preprocessor from .mod file
%
% Inputs:
%   T         [#temp variables by 1]  double   vector of temporary terms to be filled by function
%   y         [M_.endo_nbr by 1]      double   vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1]       double   vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1]     double   vector of parameter values in declaration order
%
% Output:
%   T         [#temp variables by 1]  double   vector of temporary terms
%

assert(length(T) >= 8);

T(1) = 1/params(1);
T(2) = ((params(4)+T(1)-1)/((params(6)-1)/params(6)*(1-params(29))))^(1/(1-params(29)-1));
T(3) = (params(6)-1)/params(6)*(1-(1-params(29)))*(params(25)/(params(25)*T(2)))^(-(1-params(29)));
T(4) = (params(4)+T(1)-1)*params(25)*T(2);
T(5) = params(25)*((T(2))^(1-params(29))-(T(3)+T(4)/params(25)));
T(6) = params(25)^(1-(1-params(29)))*(params(25)*T(2))^(1-params(29))-T(5);
T(7) = 1-params(27)-params(4)*params(25)*T(2)/T(6);
T(8) = T(6)*T(7);

end
