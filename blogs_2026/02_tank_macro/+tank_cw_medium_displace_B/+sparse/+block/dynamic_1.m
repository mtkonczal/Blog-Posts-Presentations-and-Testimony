function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(68)=0;
  y(67)=0;
  y(88)=params(8)*y(28)+(1-params(37))*x(2)+x(1);
  y(89)=params(9)*y(29)+params(37)*x(2);
  y(81)=params(10)*y(21)+x(4);
  y(96)=params(23)*y(36)+x(5);
  y(97)=params(24)*y(37)+x(6);
  y(107)=params(26)*y(47)+x(7);
  y(108)=params(27)*y(48)+x(8);
  T(1)=((params(4)+1/params(1)-1)/((params(6)-1)/params(6)*(1-params(32))))^(1/(1-params(32)-1));
  T(2)=(params(6)-1)/params(6)*(1-(1-params(32)))*(params(28)/(params(28)*T(1)))^(-(1-params(32)));
  T(3)=params(28)*((T(1))^(1-params(32))-(T(2)+(params(4)+1/params(1)-1)*params(28)*T(1)/params(28)));
  T(4)=params(28)^(1-(1-params(32)))*(params(28)*T(1))^(1-params(32))-T(3);
  y(110)=y(81)/T(4);
end
