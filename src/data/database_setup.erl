%% Database initialization coordinator that sets up all Mnesia tables and grid data
%% Orchestrates the creation of bot, cell, and reservation tables during application startup
-module(database_setup).
-export([setup_database/0]).

setup_database() ->
    %% Setup all database tables
    grid_model:create_table(),
    bot_model:create_table(), 
    reservation_model:setup_reservation_table(),
    
    %% Wait for tables to be ready
    db_interface:wait_for_tables([cell, bot, reservation], 5000),
    
    %% Setup grid data
    grid_model:setup_grid().

