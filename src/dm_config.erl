%% Author: mat
%% Created: 11/mar/2009
%% Description: TODO: Add description to config
-module(dm_config).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([get/1]).

%%
%% API Functions
%%


get( log_file ) -> none; % "distmap.log";
get( debug )    -> true.

%%
%% Local Functions
%%

