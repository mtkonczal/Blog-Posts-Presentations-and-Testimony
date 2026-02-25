function [y, T] = dynamic_1(y, x, params, steady_state, sparse_rowval, sparse_colval, sparse_colptr, T)
  y(65)=0;
  y(64)=0;
  y(85)=params(8)*y(28)+x(1);
  y(78)=params(9)*y(21)+x(3);
  y(90)=params(20)*y(33)+x(4);
  y(91)=params(21)*y(34)+x(5);
  y(101)=params(23)*y(44)+x(6);
  y(102)=params(24)*y(45)+x(7);
  T(1)=((params(4)+1/params(1)-1)/((params(6)-1)/params(6)*(1-params(29))))^(1/(1-params(29)-1));
  T(2)=(params(6)-1)/params(6)*(1-(1-params(29)))*(params(25)/(params(25)*T(1)))^(-(1-params(29)));
  T(3)=params(25)*((T(1))^(1-params(29))-(T(2)+(params(4)+1/params(1)-1)*params(25)*T(1)/params(25)));
  T(4)=params(25)^(1-(1-params(29)))*(params(25)*T(1))^(1-params(29))-T(3);
  y(104)=y(78)/T(4);
end
