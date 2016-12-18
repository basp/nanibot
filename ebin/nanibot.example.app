{application, nanibot,
 [{description, "An Erlang IRC bot"},
  {vsn, "0.1.0"},
  {modules, [nani_app, nani_sup]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {nani_app, 
   [{host, "irc.freenode.net"}, 
    {port, 6667}, 
    {nick, "Methbot"},
    {alts, []}
  ]}}
]}.