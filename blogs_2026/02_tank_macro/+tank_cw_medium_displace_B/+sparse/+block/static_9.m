function [y, T] = static_9(y, x, params, sparse_rowval, sparse_colval, sparse_colptr, T)
  T(1)=((params(4)+1/params(1)-1)/((params(6)-1)/params(6)*(1-params(32))))^(1/(1-params(32)-1));
  T(2)=(params(6)-1)/params(6)*(1-(1-params(32)))*(params(28)/(params(28)*T(1)))^(-(1-params(32)));
  T(3)=params(28)*((T(1))^(1-params(32))-(T(2)+(params(4)+1/params(1)-1)*params(28)*T(1)/params(28)));
  T(4)=params(28)^(1-(1-params(32)))*(params(28)*T(1))^(1-params(32))-T(3);
  y(50)=y(21)/T(4);
end
