function [y, T] = static_8(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T)
  T(1)=((params(4)+1/params(1)-1)/((params(6)-1)/params(6)*(1-params(29))))^(1/(1-params(29)-1));
  T(2)=(params(6)-1)/params(6)*(1-(1-params(29)))*(params(25)/(params(25)*T(1)))^(-(1-params(29)));
  T(3)=params(25)*((T(1))^(1-params(29))-(T(2)+(params(4)+1/params(1)-1)*params(25)*T(1)/params(25)));
  T(4)=params(25)^(1-(1-params(29)))*(params(25)*T(1))^(1-params(29))-T(3);
  y(47)=y(21)/T(4);
end
