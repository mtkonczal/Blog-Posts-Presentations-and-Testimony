%% compare_sticky_price.m
%% Generates fig_sticky_comparison.png
%%
%% Compares two scenarios of a near-permanent labor-augmenting AI/productivity
%% shock in the TANK-CW model. PRICES are held at normal stickiness in both
%% scenarios (kappa ≈ 0.117) to maintain nominal stability and keep the
%% Taylor rule anchor intact. Only WAGE stickiness varies:
%%
%%   Scenario 1 (blue)  — Normal wage stickiness:
%%     s_wages_duration  = 3.5   ->  kappaw ≈ 0.117
%%     Wages adjust within a few quarters; workers capture productivity gains.
%%
%%   Scenario 2 (red) — Near-frozen wages:
%%     s_wages_duration  = 100   ->  kappaw ≈ 0.000201
%%     Average wage spell is 100 quarters. Wages essentially cannot adjust
%%     over any horizon shown; productivity gains flow entirely to profits.
%%
%% Why not freeze prices too?
%%   Setting kappa → 0 simultaneously removes the nominal anchor used by the
%%   Taylor rule (theta_pi = 1.5). With no price adjustment, the inflation
%%   signal disappears and the model becomes near-indeterminate, producing
%%   explosive IRFs. Keeping prices at normal stickiness while freezing wages
%%   isolates the wage channel cleanly.
%%
%% kappa and kappaw are explicit model parameters so set_param_value()
%% can switch scenarios between stoch_simul calls without recompiling.

clear; close all;

%% -------------------------------------------------------------------------
%% Run Dynare once (compiles model with default normal-stickiness values)
%% -------------------------------------------------------------------------
dynare sticky_price_check noclearall nolog

var_list_ = {'Y','W','H','LS','PIE','Rn','profits','CSl','CHl'};

%% -------------------------------------------------------------------------
%% Common: prices always at normal stickiness
%% -------------------------------------------------------------------------
betta = M_.params(strcmp(M_.param_names, 'betta'));

calvo_prices  = 1 - 1/3.5;
kappa_normal  = (1 - calvo_prices) * (1 - betta*calvo_prices) / calvo_prices;

%% -------------------------------------------------------------------------
%% Scenario 1: Normal wages (s_wages = 3.5 quarters)
%% -------------------------------------------------------------------------
calvo_wages_normal = 1 - 1/3.5;
kappaw_normal = (1 - calvo_wages_normal) * (1 - betta*calvo_wages_normal) / calvo_wages_normal;

set_param_value('kappa',  kappa_normal);
set_param_value('kappaw', kappaw_normal);
[~, oo_normal, ~, ~] = stoch_simul(M_, options_, oo_, var_list_);

%% -------------------------------------------------------------------------
%% Scenario 2: Near-frozen wages (s_wages = 100 quarters); prices unchanged
%% -------------------------------------------------------------------------
calvo_wages_rigid = 1 - 1/100;
kappaw_rigid = (1 - calvo_wages_rigid) * (1 - betta*calvo_wages_rigid) / calvo_wages_rigid;

set_param_value('kappa',  kappa_normal);   % prices: unchanged
set_param_value('kappaw', kappaw_rigid);   % wages: near-frozen
[~, oo_rigid, ~, ~] = stoch_simul(M_, options_, oo_, var_list_);

%% Restore defaults
set_param_value('kappa',  kappa_normal);
set_param_value('kappaw', kappaw_normal);

fprintf('\nPhillips curve slopes:\n');
fprintf('  Normal wages  kappa = %.5f  kappaw = %.5f  (3.5q prices, 3.5q wages)\n',  kappa_normal, kappaw_normal);
fprintf('  Frozen wages  kappa = %.5f  kappaw = %.6f  (3.5q prices, 100q wages)\n', kappa_normal, kappaw_rigid);

%% -------------------------------------------------------------------------
%% Extract IRFs
%% -------------------------------------------------------------------------
T   = 20;
q   = 1:T;
sk  = '_epsZ';

get_irf = @(oo_s, v) oo_s.irfs.([v sk]);

%% -------------------------------------------------------------------------
%% Impact-quarter table
%% -------------------------------------------------------------------------
vars_tbl = {'Y','W','LS','H','PIE','Rn','profits','CHl','CSl'};
fprintf('\nImpact responses (Quarter 1, model units x 100):\n');
fprintf('%-10s  %10s  %10s\n', 'Variable', 'Normal', 'FrozenWage');
fprintf('%s\n', repmat('-',1,34));
for j = 1:length(vars_tbl)
    v  = vars_tbl{j};
    n1 = get_irf(oo_normal, v);
    r1 = get_irf(oo_rigid,  v);
    fprintf('%-10s  %10.3f  %10.3f\n', v, n1(1)*100, r1(1)*100);
end

%% -------------------------------------------------------------------------
%% Colors
%% -------------------------------------------------------------------------
col = [0.20 0.44 0.69;   % blue  = normal wages
       0.84 0.19 0.15];  % red   = frozen wages
lw  = 2;

panels = {
    'Rn',      'Fed Funds Rate (Nominal)';
    'Y',       'Output';
    'W',       'Real Wage';
    'LS',      'Labor Share';
    'H',       'Hours Worked';
    'CHl',     'Worker Consumption (\lambda-weighted)';
    'CSl',     'Capitalist Consumption ((1-\lambda)-weighted)';
    'profits', 'Profits';
};
np = size(panels, 1);

%% -------------------------------------------------------------------------
%% Plot
%% -------------------------------------------------------------------------
fig = figure('Position', [100 100 900 1100]);

for k = 1:np
    vname = panels{k,1};
    ttl   = panels{k,2};

    ax = subplot(4, 2, k);
    hold(ax, 'on');

    irf_n = get_irf(oo_normal, vname);
    irf_r = get_irf(oo_rigid,  vname);

    plot(ax, q, irf_n*100, 'Color', col(1,:), 'LineWidth', lw);
    plot(ax, q, irf_r*100, 'Color', col(2,:), 'LineWidth', lw);

    yline(ax, 0, 'k:', 'LineWidth', 0.8);
    xlim(ax, [1 T]);
    title(ax, ttl, 'FontWeight', 'bold', 'FontSize', 9);
    ylabel(ax, '% dev. from SS', 'FontSize', 7);
    xlabel(ax, 'Quarters', 'FontSize', 7);
    set(ax, 'FontSize', 8, 'Box', 'off', 'TickDir', 'out');

    if k == 2
        legend(ax, ...
            {sprintf('Normal wages (\\kappaw\\approx%.3f)', kappaw_normal), ...
             sprintf('Frozen wages (\\kappaw\\approx%.4f)', kappaw_rigid)}, ...
            'Location', 'best', 'FontSize', 7, 'Box', 'off');
    end
end

sgtitle({'Medium-Scale TANK-CW: Productivity Shock, Varying Wage Stickiness', ...
    'Blue = Wages adjust (3.5q spell)  |  Red = Wages frozen (100q spell)  |  Prices normal in both'}, ...
    'FontSize', 11, 'FontWeight', 'bold');

%% -------------------------------------------------------------------------
%% Save
%% -------------------------------------------------------------------------
exportgraphics(fig, 'fig_sticky_comparison.png', 'Resolution', 150);
fprintf('\nSaved: fig_sticky_comparison.png\n');
