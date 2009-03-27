{application, distmap,
  [{description, "DistMap distributed database"},
  {vsn, "1.0"},
  {modules, [distmap, distmap_sup, dm_config, dm_finder, dm_log, dm_membership, 
  			 dm_monitor, getoptions, http_server, ring, tcpserver, util]},
  {registered, [distmap_sup, dm_log, dm_finder, dm_membership, dm_monitor]},
  {applications, [kernel, stdlib]},
  {mod, {distmap, []}}
]}.
