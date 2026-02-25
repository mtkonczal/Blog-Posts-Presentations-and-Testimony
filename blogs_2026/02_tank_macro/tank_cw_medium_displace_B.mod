%% TANK-CW Medium Scale: Task Displacement - AI Replaces Labor Tasks
%% Third set: Task Displacement Comparison
%%
%% Scenario B (Displacement): Acemoglu-Restrepo style task allocation.
%%   We explicitly model:
%%     I_tilde : unconstrained cost-minimizing automation threshold (from W/RK)
%%     I       : technology-feasible automation frontier (AI_disp)
%%     I_star  : equilibrium automated task share
%%
%%   Linearized around a technology-constrained regime using:
%%     I_star = omega_I*I + (1-omega_I)*I_tilde
%%   with omega_I close to 1.
%%
%%   phi_disp controls how much of a common AI shock loads into automation (I)
%%   vs labor-augmenting productivity (Z):
%%     phi_disp=0 -> pure labor-augmenting AI
%%     phi_disp=1 -> pure automation/displacement AI
%%
%% Compare with Scenario A (tank_cw_medium_displace_A.mod):
%%   Z shock -> wages rise with productivity, labor share stable.

%Pre-processor variable to choose which type of model do you want to run
@#define model_version = 1

%Fiscal policy
@#define gov_spending = 1

%Sticky wages
@#define sticky_wages = 1

                    @#if gov_spending == 1
@#define debt_gdp_steadystate = 1
                    @#else
                    @#define debt_gdp_steadystate = 0
                    @#endif

%Profits in ss
@#define free_entry = 0

%%DO NOT CHANGE THE FOLLOWING%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@#if model_version == 0
@#define U_ls = 1
@#define PAC = 0
@#else
            @#if (model_version == 1|| model_version ==2)
                @#define U_ls = 0
                @#define PAC = 1
             @#else
                            @#if (model_version == 3|| model_version ==4)
                            @#define U_ls = 1
                            @#define PAC = 1
@#endif
@#endif
@#endif
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%DECLARATION OF ENDOGENOUS VARIABLES%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
var
    UCS           $u^{\prime}(c^C)$                    (long_name='Marginal Utility of Consumption, Capitalists')
    UCH           $u^{\prime}(c^W)$                    (long_name='Marginal Utility of Consumption, Workers')
    UHH           $u^{\prime}(n^W)$                    (long_name='Marginal Utility of Leisure, Workers')
    CS            $c^C$                        (long_name='Consumption, Capitalists')
    CH            $c^W$                        (long_name='Consumption, Workers')
    HH            $n^W$                        (long_name='Hours, Workers')
    HS            $n^C$                        (long_name='Hours, Capitalists')
    UHS           $u^{\prime}(n^C)$                    (long_name='Marginal Utility of Leisure, Capitalists')
    H             $n$                          (long_name='Aggregate Hours')
    R             $r$                          (long_name='Real Interest Rate')
    Rn            $R$                          (long_name='Nominal Interest Rate')
    PIE           $\Pi$                        (long_name='Inflation')
    PIEW          $\Pi^w$                      (long_name='Wage Inflation')
    MRS           $mrs$                        (long_name='Marginal Rate of Substitution')
    W             $w$                          (long_name='Real Wage')
    Y             $y$                          (long_name='Real Output')
    YW            ${y^m}$                      (long_name='Real Wholesale Output')
    MPL           $mpl$                        (long_name='Human Marginal Product of Labor')
    MC            $mc$                         (long_name='Real Marginal Costs')
    C             $c$                          (long_name='Consumption')
    G             $g$                          (long_name='Government Spending')
    tax           $t$                          (long_name='Lump Sum Taxes')
    taxS          $t^U$                        (long_name='Lump Sum Taxes, Capitalists')
    taxH          $t^W$                        (long_name='Lump Sum Taxes, Workers')
    BS            $b^{C}$                      (long_name='Government bonds, Capitalists')
    BH            $b^{W}$                      (long_name='Government bonds, Workers')
    B             $b$                          (long_name='Government debt')
    Z             $z$                          (long_name='Labor Augmenting Technology (inactive)')
    AI_disp       ${AI}$                       (long_name='Automation Technology Frontier (I)')
    I_tilde       ${\\tilde{I}}$               (long_name='Cost-minimizing Automation Threshold')
    I_star        ${I^*}$                      (long_name='Equilibrium Automated Task Share')
    profits       $d$                          (long_name='Profits - aggregate')
    profitsS      $d^C$                        (long_name='Profits - Capitalists')
    LI            $li$                         (long_name='Labor Income')
    LS            $ls$                         (long_name='Labor Share')
    MS             ${ms}$                      (long_name='Price Mark-up shock')
    WMS            ${wms}$                     (long_name='Wage Mark-up shock')
    K              ${k}$                       (long_name='Capital Stock')
    MPK           ${mpk}$                      (long_name='Marginal Product of Capital')
    RK            ${r^K}$                      (long_name='Rental Rate of Capital')
    I             ${i}$                        (long_name='Investment')
    IS            ${i^S}$                      (long_name='Investment, Capitalists')
    KS            ${k^S}$                      (long_name='Capital Stock, Capitalists')
    Q             ${q}$                        (long_name='Tobin s Q')
    KSh           ${ks}$                       (long_name='Capital Share')
    U             ${u}$                        (long_name='Variable Capital Utilization')
    Pr            ${Pr}$                       (long_name='Preference Shock')
    ZI             ${ZI}$                      (long_name='MEI Shock')
%% Variables in deviations from Yss
    profitsY GY BY taxY BHY BSY taxHY taxSY CSl CHl  BHYl BSYl
    ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%DECLARATION OF EXOGENOUS VARIABLES%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
varexo
    epsZ           ${\epsilon^{Z}}$       (long_name='Technology shock (inactive)')
    epsAI          ${\epsilon^{AI}}$      (long_name='AI Task Displacement shock')
    epsM           ${\epsilon^{M}}$       (long_name='Monetary Policy shock')
    epsG           ${\epsilon^{G}}$       (long_name='Government Spending shock')
    epsMS          ${\epsilon^{MS}}$      (long_name='Price Mark-up shock')
    epsWMS         ${\epsilon^{WMS}}$     (long_name='Wage Mark-up shock')
    epsPr          ${\epsilon^{Pr}}$      (long_name='Preference shock')
    epsZI          ${\epsilon^{I}}$       (long_name='MEI shock')
   ;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%DECLARATION OF PARAMETERS%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parameters
   betta          ${\beta}$               (long_name='Discount Factor')
   sigma_c        ${\sigma}$              (long_name='Intertemporal elasticity of substitution')
   varrho         ${\varphi}$             (long_name='Inverse of Frish elasticity of Labor Supply')
   delta          ${\delta}$              (long_name='Capital depreciation')
   phiX           ${\iota}$              (long_name='Investment adjustment costs')
   zzeta          ${\eta}$            (long_name='Elasticity of substitutions between intermediate goods')
   zzeta_w        ${\eta^w}$            (long_name='Elasticity of substitutions between labor bundles')
   rhoZ           ${\rho^{z}}$            (long_name='AR(1) Technology shock')
   rhoAI          ${\rho^{AI}}$           (long_name='AR(1) AI Displacement shock')
   rhoG           ${\rho^{g}}$            (long_name='AR(1) Government Spending shock')
   rho_r          ${\phi^{r}}$            (long_name='Interest rate smoothing')
   theta_pie      ${\phi^{\pi}}$          (long_name='Taylor rule coeff of inflation')
   theta_y        ${\phi^{y}}$            (long_name='Taylor rule coeff of output')
   nuH            ${a^W}$                 (long_name='Weight on Hours in Utility, Workers')
   lambda         ${\lambda}$             (long_name='Share of Workers Agents')
   nuS            ${a^U}$                 (long_name='Weight on Hours in Utility, Capitalists')
   tauD           ${\tau^d}$              (long_name='Tax on Profits')
   rho_tauT       ${\phi^{\tau t}}$       (long_name='Tax Inertia')
   phi_tauT_B     ${\phi^{\tau B}}$       (long_name='Fiscal Rule Coefficient on Debt')
   phi_tauT_G     ${\phi^{\tau G}}$       (long_name='Fiscal Rule Coefficient on Output')
   psiH            ${\psi^W}$             (long_name='Fixed Costs for H bonds')
   bH            ${\bar{b^W}}$            (long_name='workers bond holdings benchmark')
   rhoMS          ${\rho^{MS}}$           (long_name='AR(1) Price Mark-up shock')
   rhoWMS         ${\rho^{WMS}}$          (long_name='AR(1) Wage Mark-up shock')
   util           ${\upsilon}$            (long_name='Variable capital utilization')
   rhoPr          ${\rho^{Pr}}$           (long_name='AR(1) Preference shock')
   rhoZI          ${\rho^{I}}$            (long_name='AR(1) MEI shock')
   Hss            ${\bar H}$              (long_name='Steady State Hours')
   PIEss          ${\bar \Pi}$            (long_name='Steady State Inflation')
   gy             ${\frac{\bar{G}}{\bar{Y}}}$ (long_name='G/Y ratio in SS')
   BYss           ${\frac{\bar{B}}{\bar{Y}}}$ (long_name='B/Y ratio in SS')
   LSss           ${\bar{ls}}$            (long_name='Steady State Labor Share')

s_prices_duration s_wages_duration calvo calvo_w
   phi_disp       ${\phi^{disp}}$         (long_name='Share of AI shock loading into automation (0=augmenting,1=automation)')
   chi_Iwr        ${\chi^{I}_{wr}}$       (long_name='Elasticity of unconstrained automation threshold to W-RK')
   omega_I        ${\omega^{I}}$          (long_name='Weight on technology frontier in I* (vs cost-min threshold)')
   chi_YI         ${\chi^{Y}_{I}}$        (long_name='Output effect of higher equilibrium automation share')
   chi_MPLI       ${\chi^{MPL}_{I}}$      (long_name='Displacement effect of I* on human MPL')
   chi_MPKI       ${\chi^{MPK}_{I}}$      (long_name='Complementarity effect of I* on MPK')
   ;

%%%%%%%%%%%%%%%%%%%%%%
%%PARAMETERS VALUES %%
%%%%%%%%%%%%%%%%%%%%%%
betta      = 0.99;
delta      = 0.0250;
rhoZ       = 0.999;
rhoAI      = 0.999;   %% Near-permanent automation frontier shock
phi_disp   = 1;       %% 0: all AI is labor augmenting; 1: all AI is automation
chi_Iwr    = 0.40;
omega_I    = 0.90;
chi_YI     = 0.67;
chi_MPLI   = 0.67;
chi_MPKI   = 0.33;
sigma_c    = 1;
varrho     = 1;
phiX       = 2;
LSss       = 0.67;
util       = 0.495;

@#if (model_version == 1||model_version == 4)
lambda      = 0.7967;
@#endif

theta_pie  = 1.5;
rho_r      = 0.7;
theta_y    = 0;

rhoG       = 0.9;
rho_tauT   = 0;
phi_tauT_B = 0.33;
phi_tauT_G = 0.1;

rhoMS  = 0.75;
rhoWMS = 0.75;
rhoPr  = 0.75;
rhoZI  = 0.75;

@#if gov_spending == 1
gy = 0.2;
@#else
gy = 0;
@#endif

bH   = 0;
psiH = 0.0742;

@#if debt_gdp_steadystate==1
BYss = 0.57;
@#else
BYss = 0;
@#endif

@#if sticky_wages > 0
zzeta_w = 6;
s_wages_duration = 3.5;
@#else
zzeta_w = 1000;
s_wages_duration = 1.00001;
@#endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%STEADY STATE RELATIONSHIPS%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PIEss = 1;
Hss   = 0.33;

zzeta = 6;
s_prices_duration = 3.5;

tauD    = 0;
calvo   = 1-1/s_prices_duration;
calvo_w = 1-1/s_wages_duration;

model(linear);
#eta        = lambda;
#Rss        = 1/betta;
#RKss       = (Rss-1+delta);
#gamma1     = RKss;
#gamma2     = gamma1*(1/(util));
#HHss       = Hss/lambda;
#MCss       = (zzeta-1)/zzeta;
#alp        = 1-LSss;

#Kss        = (RKss/(MCss*alp))^(1/(alp-1))*Hss;
#YWss       = (Hss)^(1-alp)*Kss^alp;
#Wss        = MCss*(1-alp)*(Hss/Kss)^(-alp);
#F          = Hss*((Kss/Hss)^alp-(Wss+RKss*Kss/Hss));
#Yss        = YWss-F;
#FY         = F/Yss;
#MRSss      = Wss*(1-1/zzeta_w);
#profitsss  = Yss-Wss*Hss-RKss*Kss;
#Bss        = BYss*4*Yss;
#BSss       = Bss/(1-lambda);
#iy         = delta*Kss/Yss;
#cy         = 1-gy-iy;
#Css        = cy*Yss;
#Gss        = gy*Yss;
#CHss       = Css;
#taxss      = 1/betta*Bss+Gss-Bss;
#taxHss     = eta/lambda*taxss;
#taxSss     = (1-eta)/(1-lambda)*taxss;
#xi         = calvo*(zzeta-1)/((1-calvo)*(1-betta*calvo));
#xiw        = calvo_w*(zzeta_w-1)/((1-calvo_w)*(1-betta*calvo_w));
#kappa      = (zzeta-1)/xi;
#kappaw     = (zzeta_w-1)/xiw;

%% Households (identical to Scenario A)
[name='Marginal Utility of Consumption, Capitalists']
UCS = Pr-sigma_c*(CS);
[name='Euler Equation, Capitalists']
UCS = R+UCS(+1);

[name='Marginal Utility of Leisure, Capitalists']
UHS = 0;
[name='Labor Supply, Capitalists']
HS = 0;

[name='Marginal Utility of Consumption, Workers']
UCH = Pr-sigma_c*(CH);
[name='Marginal Utility of Leisure, Workers']
UHH = varrho*HH;
[name='Labor Supply, Workers']
MRS = UHH-UCH;

[name='Euler Equation, Workers']
UCH = R+UCH(+1)-(psiH/CHss)*BH;

[name='Consumption, Workers']
CH+BH*1/(CHss) = (W+HH)*Wss*HHss/CHss-taxHss/CHss*taxH+tauD/lambda*profits/CHss+BH(-1)*Rss/CHss;

%% Firms - Acemoglu-Restrepo style task allocation %%

[name='Unconstrained Automation Threshold (I_tilde)']
%% Cost-minimizing threshold from relative factor prices (W/RK).
I_tilde = chi_Iwr*(W-RK);

[name='Equilibrium Automated Task Share (I_star)']
%% Linearized analog of I* = min{I, I_tilde}.
%% omega_I near 1 approximates technology-constrained allocation (I*=I).
I_star = omega_I*AI_disp+(1-omega_I)*I_tilde;

[name='Task-based Production with Endogenous Allocation']
%% Output rises as more tasks are automated (I_star increases).
YW = (1-alp)*(Z+H)+alp*(U+K(-1))+chi_YI*I_star;

[name='Real Output']
Y = YW*(1+FY);

[name='Human Marginal Product of Labor']
%% Displacement effect: higher I_star compresses the human task set.
MPL = YW-chi_MPLI*I_star-H;

[name='Real Wage']
%% Same formula as baseline, but MPL is now human MPL only.
W = MC+MPL;

[name='Marginal Product of Capital']
%% Reallocation toward automated tasks raises marginal return to capital.
MPK = YW+chi_MPKI*I_star-U-K(-1);

[name='Rental Rate']
RK = MC+MPK;

[name='Profits']
%% Income identity: AI output flows to profits (capitalists capture all gains).
Y = (W+H)*Wss*Hss/Yss+(RK+U+K(-1))*(RKss*Kss)/Yss+profits/Yss;

[name='Profits - Capitalists']
profits = profitsS;

[name='Labor Income']
LI = W+H;

[name='Labor Share']
%% LS = W + H - Y. Y rises (AI output) but W+H unchanged -> LS falls.
LS = W+H-Y;

[name='Capital Share']
KSh = RK+U+K(-1)-Y;

[name='Variable Capital Utilization']
RK = gamma2/gamma1*U;

[name='Capital Law of Motion']
KS = delta*(IS+ZI)+(1-delta)*KS(-1);

[name='Arbitrage condition for capital demand']
Rn-PIE(+1) = betta*(1-delta)*Q(+1)+(1-betta*(1-delta))*(RK(+1))-Q;

[name='Investment Equation']
(1+1/(Rss))*IS = 1/(Rss)*IS(+1)+IS(-1)+1/(2*phiX*(1)^2)*(Q+ZI);

[name='Aggregation: Investment']
I = IS;

[name='Aggregation: Capital']
K = KS;

[name='Resource Constraint']
Y = C*cy+G*gy+iy*I+gamma1*Kss/Yss*U;

[name='Fisher Equation']
R = Rn-PIE(+1);

[name='Aggregation: Consumption']
C = lambda*CH+(1-lambda)*CS;

[name='Aggregation: Labor']
H = HH;

%%Inflation Dynamics%%
[name='Linear Phillips Curve']
PIE = betta*PIE(+1)+kappa*(MC+MS);

[name='Linear Wage Phillips Curve']
PIEW = betta*PIEW(+1)+kappaw*(MRS-W+WMS);

[name='Wage Inflation']
PIEW = W-W(-1);

[name='Taylor Rule']
Rn = rho_r*Rn(-1)+(1-rho_r)*(theta_pie*(PIE)+theta_y*Y)+epsM;

[name='Labor Augmenting Technology']
%% Common AI shock split into augmenting (1-phi_disp) and automation (phi_disp).
Z = rhoZ*Z(-1)+(1-phi_disp)*epsAI+epsZ;

[name='Automation Frontier Shock (I)']
AI_disp = rhoAI*AI_disp(-1)+phi_disp*epsAI;

[name='Government Spending Shock']
G = rhoG*G(-1)+epsG;

[name='Government Budget Constraint']
B = (B(-1)+R(-1))*Rss+G*Gss/Bss-tax*taxss/Bss;

[name='Aggregation: Gov. Bonds']
B = BS+lambda*BH/Bss;

[name='Tax Rule']
tax = rho_tauT*tax(-1)+phi_tauT_B*B(-1)+phi_tauT_G*G;

[name='Price Mark-up Shock']
MS = rhoMS*MS(-1)+epsMS;

[name='Wage Mark-up Shock']
WMS = rhoWMS*WMS(-1)+epsWMS;

[name='Preference Shock']
Pr = rhoPr*Pr(-1)+epsPr;

[name='MEI Shock']
ZI = rhoZI*ZI(-1)+epsZI;

[name='Tax Rule, Capitalists']
taxS = (1-eta)/(1-lambda)*tax;

[name='Tax Rule, Workers']
taxH = eta/lambda*tax;

%% Accounting variables
profitsY = profits/Yss;
BY   = B/Yss;
BHY  = BH/Yss;
BSY  = BS/Yss;
GY   = G/Yss;
taxY = tax/Yss;
taxHY = taxH/Yss;
taxSY = taxS/Yss;
CSl  = (1-lambda)*CS;
CHl  = lambda*CH;
BSYl = (1-lambda)*BSY;
BHYl = lambda*BHY;

end;

%% ONLY AI DISPLACEMENT SHOCK
shocks;
var epsAI; stderr 1;
end;

steady;
check;
resid;

stoch_simul(order=1, irf=20) Y C CS CH W H LS PIE Rn R profits I AI_disp I_tilde I_star CSl CHl;

save('displace_B_results.mat', 'oo_', 'M_', 'options_');
