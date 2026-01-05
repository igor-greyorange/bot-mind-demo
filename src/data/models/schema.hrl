%% Database record definitions for all Mnesia tables used in the bot coordination system
%% Contains structure definitions for cell, bot, and reservation entities

%% Cell record for grid positions
-record(cell, {
    key,    % Position {X, Y} - Primary key
    type    % empty | obstacle
}).

%% Bot record for Mnesia storage
-record(bot, {
    id,              % Bot ID (primary key)
    session_pid,     % Process handling bot connection  
    position,        % Current {X, Y} position
    status,          % idle | moving | busy
    task,            % Current task or 'none'
    path,            % Current path being followed
    path_id          % Path reservation ID for collision avoidance
}).

%% Reservation record for path reservation system
-record(reservation, {
    position,    % {X, Y} coordinate - Primary key
    bot_id,      % Bot ID that owns this reservation
    timestamp,   % When reserved (unix timestamp)
    path_id      % Unique path identifier
}).
