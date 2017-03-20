%% DB Commands
-define(CREATE   ,<<8>>).
-define(ADD      ,<<9>>).
-define(REPLACE  ,<<12>>).
-define(STORE    ,<<13>>).
-define(RETRIEVE ,<<"RETRIEVE ">>).
-define(UPDATING ,<<30>>).
-define(FULL     ,<<31>>).
%% XQuery Commands
-define(QUERY    ,<<0>>).
-define(CLOSE    ,<<2>>).
-define(BIND     ,<<3>>).
-define(RESULTS  ,<<4>>).
-define(EXECUTE  ,<<5>>).
-define(INFO     ,<<6>>).
-define(OPTIONS  ,<<7>>).
-define(CONTEXT  ,<<14>>).
