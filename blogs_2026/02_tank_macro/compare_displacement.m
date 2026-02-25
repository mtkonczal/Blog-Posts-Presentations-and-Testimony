%% compare_displacement.m
%% Generates fig_displacement_comparison.png
%%
%% Runs both displacement .mod files, extracts IRFs, and plots a comparison.
%% Blue  = Scenario A: Labor-augmenting AI (Z shock) -> wages rise, shared gains
%% Red   = Scenario B: AR-style task allocation AI (epsAI, phi_disp=1)
%%         with explicit I_tilde, I, and I_star equations.
%%
%% Run from the directory containing the .mod files.

clear; close all;

%% -------------------------------------------------------------------------
%% Run models
%% -------------------------------------------------------------------------
dynare tank_cw_medium_displace_A noclearall nolog
oo_A = oo_;

dynare tank_cw_medium_displace_B noclearall nolog
oo_B = oo_;

%% -------------------------------------------------------------------------
%% Extract IRFs
%% -------------------------------------------------------------------------
T = 20;
q = 1:T;

% Scenario A: shock is epsZ
get_A = @(v) oo_A.irfs.([v '_epsZ']);

% Scenario B: shock is epsAI
get_B = @(v) oo_B.irfs.([v '_epsAI']);

Y_A      = get_A('Y');       Y_B      = get_B('Y');
W_A      = get_A('W');       W_B      = get_B('W');
H_A      = get_A('H');       H_B      = get_B('H');
LS_A     = get_A('LS');      LS_B     = get_B('LS');
CHl_A    = get_A('CHl');     CHl_B    = get_B('CHl');
CSl_A    = get_A('CSl');     CSl_B    = get_B('CSl');
profits_A = get_A('profits'); profits_B = get_B('profits');
PIE_A    = get_A('PIE');     PIE_B    = get_B('PIE');
I_A      = get_A('I');       I_B      = get_B('I');
Rn_A     = get_A('Rn');      Rn_B     = get_B('Rn');

%% -------------------------------------------------------------------------
%% Plot
%% -------------------------------------------------------------------------
blue = [0.20 0.44 0.69];
red  = [0.84 0.19 0.15];
lw   = 2;

fig = figure('Position', [100 100 900 1100]);

panels = {
    Rn_A,      Rn_B,      'Fed Funds Rate (Nominal)';
    Y_A,       Y_B,       'Output';
    W_A,       W_B,       'Real Wage';
    LS_A,      LS_B,      'Labor Share';
    H_A,       H_B,       'Hours Worked';
    CHl_A,     CHl_B,     'Worker Consumption (\lambda-weighted)';
    CSl_A,     CSl_B,     'Capitalist Consumption ((1-\lambda)-weighted)';
    profits_A, profits_B, 'Profits';
};

nrows = 4; ncols = 2;

for k = 1:8
    subplot(nrows, ncols, k);

    irf_A = panels{k,1};
    irf_B = panels{k,2};
    name  = panels{k,3};

    plot(q, irf_A*100, 'Color', blue, 'LineWidth', lw); hold on;
    plot(q, irf_B*100, 'Color', red,  'LineWidth', lw);
    yline(0, 'k:', 'LineWidth', 0.8);
    xlim([1 T]);
    title(name, 'FontWeight', 'bold', 'FontSize', 9);
    ylabel('% dev. from SS', 'FontSize', 7);
    xlabel('Quarters', 'FontSize', 7);
    set(gca, 'FontSize', 8, 'Box', 'off', 'TickDir', 'out');

    if k == 2
        legend({'Labor-augmenting AI (Z shock)', 'Task-allocation AI (I^*)'}, ...
            'Location', 'best', 'FontSize', 7, 'Box', 'off');
    end
end

sgtitle({
    'Medium-Scale TANK-CW: Two Types of AI Shock', ...
    'Blue = wages rise with productivity  |  Red = wages flat, labor share collapses'
}, 'FontSize', 11, 'FontWeight', 'bold');

%% -------------------------------------------------------------------------
%% Save
%% -------------------------------------------------------------------------
exportgraphics(fig, 'fig_displacement_comparison.png', 'Resolution', 150);
fprintf('\nSaved: fig_displacement_comparison.png\n');
