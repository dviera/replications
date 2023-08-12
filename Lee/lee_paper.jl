using CSV
using GLM
using StatsModels
using DataFrames
using Distributions
using Random
using Plots

data = CSV.read("results2.csv", DataFrame)

m1 = glm(@formula(goal ~ team + opponent + home), data, Poisson(), contrasts=Dict(:opponent => EffectsCoding(), :team => EffectsCoding()))

m = length(coef(m1))

intercept = coef(m1)[1]
home_adv = coef(m1)[m]
att_effect = coef(m1)[2:20]
att_arsenal = -sum(att_effect)
deff_effect = coef(m1)[21:m-1]
deff_arsenal = -sum(deff_effect)

tb2 = DataFrame(Team=unique(data.team),
    Offensive_parameter=[[att_arsenal]; att_effect],
    Offensive_multiplier=exp.([[att_arsenal]; att_effect]),
    Deffensive_parameter=[[deff_arsenal]; deff_effect],
    Deffensive_multiplier=exp.([[deff_arsenal]; deff_effect]),
)

# Just rounding like in the table
tb2 = hcat(DataFrame(Team=tb2[!, "Team"]), round.(tb2[:, 2:5], digits=2))

function sim_match(home_team, away_team, n_sim=1000)
    ht = filter(row -> row[:Team] == home_team, tb2)
    at = filter(row -> row[:Team] == away_team, tb2)

    lambda_home = exp(intercept + home_adv + ht[1, 2] + at[1, 4])
    lambda_away = exp(intercept + at[1, 2] + ht[1, 4])

    sim_home = rand(Poisson(lambda_home), n_sim)
    sim_away = rand(Poisson(lambda_away), n_sim)

    prob_win = mean((sim_home .- sim_away) .> 0)
    prob_draw = mean((sim_home .- sim_away) .== 0)
    prob_loss = mean((sim_home .- sim_away) .< 0)

    return [prob_win, prob_draw, prob_loss]
end

function sim_season(teams, n_season)

    n_teams = length(teams)

    # store one season
    results = Dict(t => 0 for t in teams)
    sim_result = Float64[]

    # store number of simulated season
    sim_seasons = Dict(t => Float64[] for t in teams)

    # store at top of the table
    top_table = Dict(t => 0 for t in teams)

    for i in 1:n_season

        for home in teams
            for away in teams
                if home != away
                    sim_result = sim_match(home, away, 1)
                    results[home] += sim_result' * [3, 1, 0]
                    results[away] += sim_result' * [0, 1, 3]
                end
            end
        end

        for team in teams
            append!(sim_seasons[team], results[team])
        end

        team_top = findmax(results)[2]
        top_table[team_top] += 1

        results = Dict(t => 0 for t in teams)
        sim_result = Float64[]
    end
    sim_seasons, top_table
end

number_sim = 1000
ss, tt = sim_season(unique(data[!, "team"]), number_sim)

##############################
# Table 4
##############################
tb4 = DataFrame(Team=collect(keys(ss)),
    Simulated_Mean=round.(mean.(values(ss)); digits=2),
    Simulated_STD=round.(std.(values(ss)); digits=2),
    Top_Table=values(tt) ./ number_sim)

tb4 = sort(tb4, [:Simulated_Mean], rev=true)

scatter(tb2[!, "Offensive_parameter"],
    -tb2[!, "Deffensive_parameter"],
    label=nothing,
    color="#f96158",
    series_annotations=text.(tb2[!, "Team"], :bottom, 8),
    markerstrokewidth=0,
    markersize=8)
xlabel!("Offensive availability")
ylabel!("Deffensive availability")
title!("Premier League 95/96")