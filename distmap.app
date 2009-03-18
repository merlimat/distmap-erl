{application, distmap,
  [{description, "DistMap distributed database"},
  {vsn, "1.0"},
  {modules, [distmap, dm_log, dm_membership, ring]},
  {registered, [dm_log, dm_membership]},
  {applications, [kernel, stdlib]},
  {mod, {distmap, []}}
]}.
