%% compare_phi_disp.m
%% Generates fig_phi_disp_comparison.png
%%
%% Runs tank_cw_medium_displace_B with phi_disp = 0, 0.5, 1
%% to show how automation intensity shapes outcomes in the
%% AR-style task-allocation block (I_tilde, I, I_star).
%%
%% phi_disp=0   -> AI is pure labor-augmenting (wages rise, like Z shock)
%% phi_disp=0.5 -> partial displacement (wages rise some, labor share falls some)
%% phi_disp=1   -> full displacement (wages flat, all gains to profits)

clear; close all;

%% -------------------------------------------------------------------------
%% Compile model once, then re-solve for each phi_disp value
%% -------------------------------------------------------------------------
dynare tank_cw_medium_displace_B noclearall nolog

phi_vals  = [0, 0.5, 1];
n_phi     = length(phi_vals);
oo_store  = cell(n_phi, 1);

var_list_ = {'Y','W','H','LS','PIE','Rn','profits','CSl','CHl'};

for i = 1:n_phi
    set_param_value('phi_disp', phi_vals(i));
    [~, oo_i, ~, ~] = stoch_simul(M_, options_, oo_, var_list_);
    oo_store{i} = oo_i;
end

%% Restore phi_disp to its original value
set_param_value('phi_disp', 1);

%% -------------------------------------------------------------------------
%% Extract IRFs
%% -------------------------------------------------------------------------
T  = 20;
q  = 1:T;
sk = '_epsAI';   % shock suffix

get_irf = @(oo_s, v) oo_s.irfs.([v sk]);

%% -------------------------------------------------------------------------
%% Plot
%% -------------------------------------------------------------------------
% Three-colour ramp: blue -> amber -> red
col = [0.20 0.44 0.69;   % phi=0   blue
       0.93 0.60 0.00;   % phi=0.5 amber
       0.84 0.19 0.15];  % phi=1   red
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

fig = figure('Position', [100 100 900 1100]);

for k = 1:np
    vname = panels{k,1};
    ttl   = panels{k,2};

    ax = subplot(4, 2, k);
    hold(ax, 'on');

    for i = 1:n_phi
        irf = get_irf(oo_store{i}, vname);
        plot(ax, q, irf*100, 'Color', col(i,:), 'LineWidth', lw);
    end

    yline(ax, 0, 'k:', 'LineWidth', 0.8);
    xlim(ax, [1 T]);
    title(ax, ttl, 'FontWeight', 'bold', 'FontSize', 9);
    ylabel(ax, '% dev. from SS', 'FontSize', 7);
    xlabel(ax, 'Quarters', 'FontSize', 7);
    set(ax, 'FontSize', 8, 'Box', 'off', 'TickDir', 'out');

    if k == 2
        legend(ax, ...
            {'\phi_{disp}=0  (labor-augmenting)', ...
             '\phi_{disp}=0.5  (partial displacement)', ...
             '\phi_{disp}=1  (full displacement)'}, ...
            'Location', 'best', 'FontSize', 7, 'Box', 'off');
    end
end

sgtitle({'Medium-Scale TANK-CW: AI Task Displacement Intensity', ...
    'Blue \phi=0 (all AI \rightarrow labor productivity)  |  Amber \phi=0.5 (split)  |  Red \phi=1 (all AI \rightarrow automation)'}, ...
    'FontSize', 11, 'FontWeight', 'bold');

%% -------------------------------------------------------------------------
%% Save
%% -------------------------------------------------------------------------
exportgraphics(fig, 'fig_phi_disp_comparison.png', 'Resolution', 150);
fprintf('\nSaved: fig_phi_disp_comparison.png\n');
