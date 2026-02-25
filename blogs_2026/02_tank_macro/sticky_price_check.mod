%% TANK-CW Medium Scale: Sticky Price/Wage Check
%% Two scenarios driven by kappa and kappaw (Phillips curve slopes):
%%
%%   Scenario 1 — Normal stickiness:
%%     s_prices_duration = 3.5 quarters  ->  kappa  ≈ 0.117
%%     s_wages_duration  = 3.5 quarters  ->  kappaw ≈ 0.117
%%     Productivity shock raises MPL -> wages rise over time -> workers benefit.
%%
%%   Scenario 2 — Near-rigid prices and wages:
%%     s_prices_duration = 1000 quarters ->  kappa  ≈ 0.000011
%%     s_wages_duration  = 1000 quarters ->  kappaw ≈ 0.000011
%%     Wages essentially frozen. Productivity gains cannot flow to workers
%%     through nominal wage adjustment -> all gains go to profits.
%%
%% Shock: near-permanent labor-augmenting technology shock (rhoZ = 0.999).
%% Model: TANK-CW, model_version = 1 (lambda ≈ 0.80 workers, hand-to-mouth).
%%
%% kappa and kappaw are declared as explicit parameters so that
%% set_param_value() can switch scenarios between stoch_simul calls
%% in compare_sticky_price.m without re-running the preprocessor.

%% -----------------------------------------------------------------------
%% ENDOGENOUS VARIABLES
%% -----------------------------------------------------------------------
var
    UCS     $u^{\prime}(c^C)$   (long_name='Marginal Utility of Consumption, Capitalists')
    UCH     $u^{\prime}(c^W)$   (long_name='Marginal Utility of Consumption, Workers')
    UHH     $u^{\prime}(n^W)$   (long_name='Marginal Utility of Leisure, Workers')
    CS      $c^C$               (long_name='Consumption, Capitalists')
    CH      $c^W$               (long_name='Consumption, Workers')
    HH      $n^W$               (long_name='Hours, Workers')
    HS      $n^C$               (long_name='Hours, Capitalists')
    UHS     $u^{\prime}(n^C)$   (long_name='Marginal Utility of Leisure, Capitalists')
    H       $n$                 (long_name='Aggregate Hours')
    R       $r$                 (long_name='Real Interest Rate')
    Rn      $R$                 (long_name='Nominal Interest Rate')
    PIE     $\Pi$               (long_name='Inflation')
    PIEW    $\Pi^w$             (long_name='Wage Inflation')
    MRS     $mrs$               (long_name='Marginal Rate of Substitution')
    W       $w$                 (long_name='Real Wage')
    Y       $y$                 (long_name='Real Output')
    YW      ${y^m}$             (long_name='Real Wholesale Output')
    MPL     $mpl$               (long_name='Marginal Product of Labor')
    MC      $mc$                (long_name='Real Marginal Costs')
    C       $c$                 (long_name='Consumption')
    G       $g$                 (long_name='Government Spending')
    tax     $t$                 (long_name='Lump Sum Taxes')
    taxS    $t^U$               (long_name='Lump Sum Taxes, Capitalists')
    taxH    $t^W$               (long_name='Lump Sum Taxes, Workers')
    BS      $b^{C}$             (long_name='Government Bonds, Capitalists')
    BH      $b^{W}$             (long_name='Government Bonds, Workers')
    B       $b$                 (long_name='Government Debt')
    Z       $z$                 (long_name='Labor Augmenting Technology')
    profits $d$                 (long_name='Profits - Aggregate')
    profitsS $d^C$              (long_name='Profits - Capitalists')
    LI      $li$                (long_name='Labor Income')
    LS      $ls$                (long_name='Labor Share')
    MS      ${ms}$              (long_name='Price Mark-up Shock')
    WMS     ${wms}$             (long_name='Wage Mark-up Shock')
    K       ${k}$               (long_name='Capital Stock')
    MPK     ${mpk}$             (long_name='Marginal Product of Capital')
    RK      ${r^K}$             (long_name='Rental Rate of Capital')
    I       ${i}$               (long_name='Investment')
    IS      ${i^S}$             (long_name='Investment, Capitalists')
    KS      ${k^S}$             (long_name='Capital Stock, Capitalists')
    Q       ${q}$               (long_name='Tobin Q')
    KSh     ${ks}$              (long_name='Capital Share')
    U       ${u}$               (long_name='Variable Capital Utilization')
    Pr      ${Pr}$              (long_name='Preference Shock')
    ZI      ${ZI}$              (long_name='MEI Shock')
    profitsY GY BY taxY BHY BSY taxHY taxSY CSl CHl BHYl BSYl
    ;

%% -----------------------------------------------------------------------
%% EXOGENOUS VARIABLES
%% -----------------------------------------------------------------------
varexo
    epsZ    ${\epsilon^{Z}}$    (long_name='Technology Shock')
    epsM    ${\epsilon^{M}}$    (long_name='Monetary Policy Shock')
    epsG    ${\epsilon^{G}}$    (long_name='Government Spending Shock')
    epsMS   ${\epsilon^{MS}}$   (long_name='Price Mark-up Shock')
    epsWMS  ${\epsilon^{WMS}}$  (long_name='Wage Mark-up Shock')
    epsPr   ${\epsilon^{Pr}}$   (long_name='Preference Shock')
    epsZI   ${\epsilon^{I}}$    (long_name='MEI Shock')
    ;

%% -----------------------------------------------------------------------
%% PARAMETERS
%% -----------------------------------------------------------------------
parameters
    betta       ${\beta}$           (long_name='Discount Factor')
    sigma_c     ${\sigma}$          (long_name='CRRA / IES')
    varrho      ${\varphi}$         (long_name='Inverse Frisch Elasticity')
    delta       ${\delta}$          (long_name='Capital Depreciation')
    phiX        ${\iota}$           (long_name='Investment Adjustment Costs')
    zzeta       ${\eta}$            (long_name='Elasticity of Substitution, Goods')
    zzeta_w     ${\eta^w}$          (long_name='Elasticity of Substitution, Labor')
    rhoZ        ${\rho^z}$          (long_name='AR(1) Technology Shock')
    rhoG        ${\rho^g}$          (long_name='AR(1) Government Spending')
    rho_r       ${\phi^r}$          (long_name='Taylor Rule Smoothing')
    theta_pie   ${\phi^\pi}$        (long_name='Taylor Rule Inflation Coeff')
    theta_y     ${\phi^y}$          (long_name='Taylor Rule Output Coeff')
    lambda      ${\lambda}$         (long_name='Share of Worker Agents')
    tauD        ${\tau^d}$          (long_name='Tax on Profits')
    rho_tauT    ${\phi^{\tau t}}$   (long_name='Tax Inertia')
    phi_tauT_B  ${\phi^{\tau B}}$   (long_name='Fiscal Rule Coeff on Debt')
    phi_tauT_G  ${\phi^{\tau G}}$   (long_name='Fiscal Rule Coeff on Output')
    psiH        ${\psi^W}$          (long_name='Portfolio Adjustment Cost, Workers')
    bH          ${\bar{b^W}}$       (long_name='Workers Bond Benchmark')
    rhoMS       ${\rho^{MS}}$       (long_name='AR(1) Price Mark-up')
    rhoWMS      ${\rho^{WMS}}$      (long_name='AR(1) Wage Mark-up')
    util        ${\upsilon}$        (long_name='Capital Utilization')
    rhoPr       ${\rho^{Pr}}$       (long_name='AR(1) Preference Shock')
    rhoZI       ${\rho^I}$          (long_name='AR(1) MEI Shock')
    Hss         ${\bar H}$          (long_name='Steady State Hours')
    PIEss       ${\bar\Pi}$         (long_name='Steady State Inflation')
    gy          (long_name='G/Y Ratio in SS')
    BYss        (long_name='B/Y Ratio in SS')
    LSss        ${\bar{ls}}$        (long_name='Steady State Labor Share')
    %% Phillips curve slopes — declared as parameters so set_param_value
    %% can switch scenarios without re-running the preprocessor.
    kappa       ${\kappa}$          (long_name='Phillips Curve Slope (prices)')
    kappaw      ${\kappa^w}$        (long_name='Phillips Curve Slope (wages)')
    s_prices_duration s_wages_duration calvo calvo_w
    ;

%% -----------------------------------------------------------------------
%% PARAMETER VALUES  (Scenario 1 — Normal Stickiness as default)
%% -----------------------------------------------------------------------
betta       = 0.99;
delta       = 0.025;
rhoZ        = 0.999;    %% Near-permanent shock — same as displace_A/B
sigma_c     = 1;
varrho      = 1;
phiX        = 2;
LSss        = 0.67;
util        = 0.495;
lambda      = 0.7967;   %% TANK-CW model_version = 1

theta_pie   = 1.5;
rho_r       = 0.7;
theta_y     = 0;

rhoG        = 0.9;
rho_tauT    = 0;
phi_tauT_B  = 0.33;
phi_tauT_G  = 0.1;

rhoMS       = 0.75;
rhoWMS      = 0.75;
rhoPr       = 0.75;
rhoZI       = 0.75;

gy          = 0.2;
bH          = 0;
psiH        = 0.0742;
BYss        = 0.57;
tauD        = 0;

PIEss       = 1;
Hss         = 0.33;
zzeta       = 6;
zzeta_w     = 6;

%% Scenario 1 stickiness (default).
%% compare_sticky_price.m will override kappa and kappaw for Scenario 2.
s_prices_duration = 3.5;
s_wages_duration  = 3.5;
calvo   = 1 - 1/s_prices_duration;
calvo_w = 1 - 1/s_wages_duration;

%% kappa = (1-calvo)*(1-beta*calvo)/calvo
%% This is the Calvo/Rotemberg equivalence result; does not depend on zzeta.
kappa   = (1 - calvo)  * (1 - betta*calvo)   / calvo;
kappaw  = (1 - calvo_w)* (1 - betta*calvo_w) / calvo_w;

%% -----------------------------------------------------------------------
%% MODEL BLOCK
%% -----------------------------------------------------------------------
model(linear);
#eta    = lambda;
#Rss    = 1/betta;
#RKss   = Rss - 1 + delta;
#gamma1 = RKss;
#gamma2 = gamma1 * (1/util);
#HHss   = Hss/lambda;
#MCss   = (zzeta-1)/zzeta;
#alp    = 1 - LSss;
#Kss    = (RKss/(MCss*alp))^(1/(alp-1)) * Hss;
#YWss   = Hss^(1-alp) * Kss^alp;
#Wss    = MCss*(1-alp)*(Hss/Kss)^(-alp);
#F      = Hss*((Kss/Hss)^alp - (Wss + RKss*Kss/Hss));
#Yss    = YWss - F;
#FY     = F/Yss;
#profitsss = Yss - Wss*Hss - RKss*Kss;
#Bss    = BYss*4*Yss;
#BSss   = Bss/(1-lambda);
#iy     = delta*Kss/Yss;
#cy     = 1 - gy - iy;
#Css    = cy*Yss;
#Gss    = gy*Yss;
#CHss   = Css;
#taxss  = 1/betta*Bss + Gss - Bss;
#taxHss = eta/lambda * taxss;
#taxSss = (1-eta)/(1-lambda) * taxss;

%% --- Capitalist household ---
[name='Capitalist MU of Consumption']
UCS = Pr - sigma_c*CS;
[name='Capitalist Euler Equation']
UCS = R + UCS(+1);
[name='Capitalist MU of Leisure (inactive)']
UHS = 0;
[name='Capitalist Labor Supply (inactive)']
HS = 0;

%% --- Worker household ---
[name='Worker MU of Consumption']
UCH = Pr - sigma_c*CH;
[name='Worker MU of Leisure']
UHH = varrho*HH;
[name='Worker MRS']
MRS = UHH - UCH;
[name='Worker Euler Equation (with PAC)']
UCH = R + UCH(+1) - (psiH/CHss)*BH;
[name='Worker Budget Constraint']
CH + BH*(1/CHss) = (W+HH)*Wss*HHss/CHss - taxHss/CHss*taxH + tauD/lambda*profits/CHss + BH(-1)*Rss/CHss;

%% --- Firms ---
[name='Cobb-Douglas Production Function (Labor Augmenting)']
YW = (1-alp)*(Z+H) + alp*(U+K(-1));
[name='Retail Output']
Y = YW*(1+FY);
[name='Marginal Product of Labor']
MPL = YW - H;
[name='Real Wage']
W = MC + MPL;
[name='Marginal Product of Capital']
MPK = YW - U - K(-1);
[name='Capital Rental Rate']
RK = MC + MPK;
[name='Income Identity']
Y = (W+H)*Wss*Hss/Yss + (RK+U+K(-1))*(RKss*Kss)/Yss + profits/Yss;
[name='Profits to Capitalists']
profits = profitsS;
[name='Labor Income']
LI = W + H;
[name='Labor Share']
LS = W + H - Y;
[name='Capital Share']
KSh = RK + U + K(-1) - Y;

%% --- Capital ---
[name='Variable Capital Utilization']
RK = gamma2/gamma1 * U;
[name='Capital Law of Motion']
KS = delta*(IS+ZI) + (1-delta)*KS(-1);
[name='Capital Arbitrage / Tobin Q']
Rn - PIE(+1) = betta*(1-delta)*Q(+1) + (1-betta*(1-delta))*RK(+1) - Q;
[name='Investment Euler Equation']
(1+1/Rss)*IS = (1/Rss)*IS(+1) + IS(-1) + 1/(2*phiX)*( Q + ZI );
[name='Investment Aggregation']
I = IS;
[name='Capital Aggregation']
K = KS;

%% --- Aggregation and market clearing ---
[name='Resource Constraint']
Y = C*cy + G*gy + iy*I + gamma1*Kss/Yss*U;
[name='Fisher Equation']
R = Rn - PIE(+1);
[name='Consumption Aggregation']
C = lambda*CH + (1-lambda)*CS;
[name='Labor Aggregation']
H = HH;

%% --- Nominal dynamics ---
%% kappa and kappaw are explicit model parameters (not #-variables)
%% so that compare_sticky_price.m can switch scenarios via set_param_value.
[name='Phillips Curve']
PIE = betta*PIE(+1) + kappa*(MC + MS);
[name='Wage Phillips Curve']
PIEW = betta*PIEW(+1) + kappaw*(MRS - W + WMS);
[name='Wage Inflation Definition']
PIEW = W - W(-1);
[name='Taylor Rule']
Rn = rho_r*Rn(-1) + (1-rho_r)*(theta_pie*PIE + theta_y*Y) + epsM;

%% --- Shocks ---
[name='Labor Augmenting Technology']
Z = rhoZ*Z(-1) + epsZ;
[name='Government Spending Shock']
G = rhoG*G(-1) + epsG;
[name='Government Budget Constraint']
B = (B(-1)+R(-1))*Rss + G*Gss/Bss - tax*taxss/Bss;
[name='Bond Market Clearing']
B = BS + lambda*BH/Bss;
[name='Fiscal Rule']
tax = rho_tauT*tax(-1) + phi_tauT_B*B(-1) + phi_tauT_G*G;
[name='Price Mark-up Shock']
MS = rhoMS*MS(-1) + epsMS;
[name='Wage Mark-up Shock']
WMS = rhoWMS*WMS(-1) + epsWMS;
[name='Preference Shock']
Pr = rhoPr*Pr(-1) + epsPr;
[name='MEI Shock']
ZI = rhoZI*ZI(-1) + epsZI;
[name='Capitalist Tax Rule']
taxS = (1-eta)/(1-lambda)*tax;
[name='Worker Tax Rule']
taxH = eta/lambda*tax;

%% Accounting identities
profitsY = profits/Yss;
BY   = B/Yss;
BHY  = BH/Yss;
BSY  = BS/Yss;
GY   = G/Yss;
taxY = tax/Yss;
taxHY= taxH/Yss;
taxSY= taxS/Yss;
CSl  = (1-lambda)*CS;
CHl  = lambda*CH;
BSYl = (1-lambda)*BSY;
BHYl = lambda*BHY;
end;

%% -----------------------------------------------------------------------
%% SHOCKS: Only the technology shock is active for this comparison.
%% -----------------------------------------------------------------------
shocks;
var epsZ;   stderr 1;
var epsM;   stderr 0;
var epsG;   stderr 0;
var epsMS;  stderr 0;
var epsWMS; stderr 0;
var epsPr;  stderr 0;
var epsZI;  stderr 0;
end;

steady;
check;
resid;

stoch_simul(order=1, irf=20) Y C CS CH W H LS PIE Rn R profits I Z CSl CHl;

save('sticky_normal_results.mat', 'oo_', 'M_', 'options_');
