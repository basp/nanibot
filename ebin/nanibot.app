{application, nanibot,
 [{description, "Experimental"},
  {vsn, "0.1.0"},
  {modules, [nani_app]},
  {registered, [nani_sup]},
  {applications, [kernel, stdlib]},
  {mod, {nani_app, []}}
 ]}.